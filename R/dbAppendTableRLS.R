#' RLS-safe table append
#'
#' Appends rows to a database table while remaining compatible with PostgreSQL Row-Level Security (RLS). Avoids direct `COPY FROM` statements into RLS-protected tables, which is not allowed by postgres unless the user is specified as BYPASSRLS or is a superuser. The trade off is that this function is slower than `DBI::dbAppendTable()` for large datasets, but it will work in more scenarios and is more robust to schema changes. The default method is "staging" for PostgreSQL connections, which uses a temporary staging table to perform the insert, and "bind" for other databases, which performs batched inserts using parameterized queries. The `on_conflict` argument allows you to specify how to handle conflicts with existing rows based on specified key columns.
#'
#' @param con A DBI connection.
#' @param target A table identifier accepted by DBI (character, [DBI::Id()], or [DBI::SQL()]).
#' @param value A data.frame or data.table containing rows to append.
#' @param method One of `"auto"`, `"staging"`, or `"bind"`.
#' @param chunk_size Number of rows per batch for `method = "bind"` (not used for `method = "staging"`).
#' @param on_conflict Conflict behavior: one of `"error"`, `"nothing"`, or `"update"`.
#' @param conflict_cols Character vector of conflict key columns used for `on_conflict = "nothing"` or `"update"`.
#' @param update_cols Character vector of columns to update for `on_conflict = "update"`.
#'
#' @return Integer number of rows inserted/affected.
#' @export
#'
#' @examples
#' \dontrun{
#' con <- AquaConnect()
#' dbAppendTableRLS(con, "organizations", data.frame(name = "Example Org"))
#' DBI::dbDisconnect(con)
#' }

dbAppendTableRLS <- function(
  con,
  target,
  value,
  method = c("auto", "staging", "bind"),
  chunk_size = 10000L,
  on_conflict = c("error", "nothing", "update"),
  conflict_cols = NULL,
  update_cols = NULL
) {
  # # Create a test table, just for testing (REMEMBER TO DELETE THIS)
  # DBI::dbExecute(con, "DROP TABLE IF EXISTS test_table;")
  # DBI::dbExecute(
  #   con,
  #   "CREATE TABLE IF NOT EXISTS test_table (id SERIAL PRIMARY KEY, name TEXT, name2 TEXT)"
  # )
  # target <- "public.test_table"
  # value <- data.table::data.table(
  #   name = c("one", "two", "three", "four", "five"),
  #   name2 = c("uno", "dos", "tres", "cuatro", "cinco")
  # )
  # method <- "bind"
  # chunk_size <- 2L
  # on_conflict <- "error"
  # conflict_cols = NULL
  # update_cols = NULL

  method <- match.arg(method)
  on_conflict <- match.arg(on_conflict)

  if (!inherits(value, "data.frame")) {
    # data.frame is also valid for data.table
    stop("dbAppendTableRLS: 'value' must be a data.frame or data.table.")
  }
  if (!is.numeric(chunk_size) || length(chunk_size) != 1 || chunk_size < 1) {
    stop("dbAppendTableRLS: 'chunk_size' must be a single positive number.")
  }

  if (nrow(value) == 0) {
    stop("dbAppendTableRLS: 'value' contains no rows to append.")
  }

  # List possible fields in target table to validate input and construct SQL
  target_fields <- DBI::dbGetQuery(
    con,
    paste0("SELECT * FROM ", target, " LIMIT 0;")
  ) |>
    names()

  if (length(target_fields) == 0) {
    stop("dbAppendTableRLS: target table has no columns or does not exist.")
  }

  # Q: is this necessary, or just let this be handled by the database when we try to insert?
  missing_cols <- setdiff(names(value), target_fields)
  if (length(missing_cols) > 0) {
    stop(
      "dbAppendTableRLS: input contains columns not present in target table: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  insert_cols <- target_fields[target_fields %in% names(value)]
  if (length(insert_cols) == 0) {
    stop(
      "dbAppendTableRLS: no overlapping columns between input and target table."
    )
  }

  if (on_conflict != "error") {
    if (is.null(conflict_cols) || length(conflict_cols) == 0) {
      stop(
        "dbAppendTableRLS: 'conflict_cols' is required when on_conflict is not 'error'."
      )
    }
    unknown_conflict <- setdiff(conflict_cols, target_fields)
    if (length(unknown_conflict) > 0) {
      stop(
        "dbAppendTableRLS: unknown conflict_cols: ",
        paste(unknown_conflict, collapse = ", ")
      )
    }
  }

  if (on_conflict == "update") {
    if (is.null(update_cols) || length(update_cols) == 0) {
      stop(
        "dbAppendTableRLS: 'update_cols' is required for on_conflict = 'update'."
      )
    }
    unknown_update <- setdiff(update_cols, target_fields)
    if (length(unknown_update) > 0) {
      stop(
        "dbAppendTableRLS: unknown update_cols: ",
        paste(unknown_update, collapse = ", ")
      )
    }
    if (length(intersect(update_cols, conflict_cols)) > 0) {
      stop(
        "dbAppendTableRLS: 'update_cols' cannot include columns listed in 'conflict_cols'."
      )
    }
  }

  # Subset and reorder input data to match target table columns, ensuring it's a data.frame for dbBind
  if (inherits(value, "data.table")) {
    # Reorder columns to match target table
    value <- value[, ..insert_cols, drop = FALSE]
  } else {
    value <- value[, insert_cols, drop = FALSE]
  }

  is_postgres <- any(c("PqConnection", "PostgreSQLConnection") %in% class(con))
  if (method == "auto") {
    method <- if (is_postgres) "staging" else "bind"
  }

  cols_sql <- paste(
    as.character(DBI::dbQuoteIdentifier(con, insert_cols)),
    collapse = ", "
  )

  # Generate the appropriate ON CONFLICT clause based on user input

  # Function to generate the appropriate ON CONFLICT clause based on user input
  .db_append_rls_conflict_sql <- function(
    con,
    on_conflict,
    conflict_cols,
    update_cols
  ) {
    if (on_conflict == "error") {
      return("")
    }

    conflict_sql <- paste(
      as.character(DBI::dbQuoteIdentifier(con, conflict_cols)),
      collapse = ", "
    )

    if (on_conflict == "nothing") {
      return(paste0(" ON CONFLICT (", conflict_sql, ") DO NOTHING"))
    }

    set_sql <- paste0(
      as.character(DBI::dbQuoteIdentifier(con, update_cols)),
      " = EXCLUDED.",
      as.character(DBI::dbQuoteIdentifier(con, update_cols)),
      collapse = ", "
    )

    paste0(" ON CONFLICT (", conflict_sql, ") DO UPDATE SET ", set_sql)
  }

  conflict_sql <- .db_append_rls_conflict_sql(
    con = con,
    on_conflict = on_conflict,
    conflict_cols = conflict_cols,
    update_cols = update_cols
  )

  # Define functions to perform the append operation
  # Function to perform the staging table approach for PostgreSQL connections, which is RLS-safe but may be slower for large datasets
  .db_append_rls_staging <- function(
    con,
    target,
    cols_sql,
    value,
    conflict_sql
  ) {
    tmp_name <- paste0("ac_tmp_append_", as.integer(stats::runif(1, 1e7, 9e7)))
    tmp_ident <- as.character(DBI::dbQuoteIdentifier(con, tmp_name))

    # Create a temp table with only the columns we need for the insert. Any constraints are handled on the final insert, no need to worry about those here.
    DBI::dbExecute(
      con,
      paste0(
        "CREATE TEMP TABLE ",
        tmp_ident,
        " AS SELECT ",
        cols_sql,
        " FROM ",
        target,
        " LIMIT 0"
      )
    )
    on.exit(
      {
        suppressMessages(try(
          DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tmp_ident, ";")),
          silent = TRUE
        ))
      }
    )

    # Regular dbAppendTable to the staging table (which is not RLS-protected), then a single INSERT INTO ... SELECT from the staging table to the target with conflict handling
    DBI::dbAppendTable(con, DBI::SQL(tmp_ident), value)

    DBI::dbExecute(
      con,
      paste0(
        "INSERT INTO ",
        target,
        " (",
        cols_sql,
        ") SELECT ",
        cols_sql,
        " FROM ",
        tmp_ident,
        conflict_sql,
        ";"
      )
    )
  }

  # Function to perform batched inserts using parameterized queries, which is more widely compatible but may be slower for large datasets compared to the staging approach
  .db_append_rls_bind <- function(
    con,
    target,
    cols_sql,
    value,
    chunk_size,
    conflict_sql
  ) {
    placeholders <- paste0("$", seq_len(ncol(value)), collapse = ", ")
    insert_sql <- paste0(
      "INSERT INTO ",
      target,
      " (",
      cols_sql,
      ") VALUES (",
      placeholders,
      ")",
      conflict_sql,
      ";"
    )

    n <- nrow(value)
    rows_affected <- 0L

    for (start in seq.int(1L, n, by = chunk_size)) {
      end <- min(start + chunk_size - 1L, n)
      chunk <- value[start:end, , drop = FALSE]

      params <- unname(as.list(chunk))

      res <- DBI::dbSendStatement(con, insert_sql)
      tryCatch(
        {
          DBI::dbBind(res, params)
          rows_affected <- rows_affected + DBI::dbGetRowsAffected(res)
        },
        finally = {
          suppressWarnings(try(DBI::dbClearResult(res), silent = TRUE))
        }
      )
    }

    rows_affected
  }

  # Start a transaction to ensure atomicity of the append operation
  active_trans <- dbTransBegin(con)
  on.exit(
    {
      if (isTRUE(active_trans)) {
        suppressWarnings(try(DBI::dbExecute(con, "ROLLBACK;"), silent = TRUE))
      }
    },
    add = TRUE
  )

  rows_affected <- tryCatch(
    {
      if (method == "staging") {
        if (!is_postgres) {
          stop(
            "dbAppendTableRLS: method='staging' currently requires a PostgreSQL connection."
          )
        }
        .db_append_rls_staging(
          con = con,
          target = target,
          cols_sql = cols_sql,
          value = value,
          conflict_sql = conflict_sql
        )
      } else {
        # method == "bind"
        .db_append_rls_bind(
          con = con,
          target = target,
          cols_sql = cols_sql,
          value = value,
          chunk_size = as.integer(chunk_size),
          conflict_sql = conflict_sql
        )
      }
    },
    error = function(e) {
      stop(
        "dbAppendTableRLS: append failed for table ",
        target,
        " using method '",
        method,
        "'. Error: ",
        e$message,
        call. = FALSE
      )
    }
  )

  if (isTRUE(active_trans)) {
    DBI::dbExecute(con, "COMMIT;")
  }
  on.exit(NULL, add = FALSE)

  invisible(as.integer(rows_affected))
} # end of dbAppendTableRLS()
