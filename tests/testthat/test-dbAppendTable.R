test_that("dbAppendTableRLS validates arguments", {
  con <- connect_test()
  on.exit(cleanup_postgres_session(con))

  DBI::dbExecute(
    con,
    "CREATE TEMP TABLE ac_append_validation (id integer, txt text)"
  )

  expect_error(
    dbAppendTableRLS(con, "ac_append_validation", list(id = 1L)),
    "must be a data.frame"
  )

  expect_error(
    dbAppendTableRLS(
      con,
      "ac_append_validation",
      data.frame(id = 1L),
      chunk_size = 0
    ),
    "chunk_size"
  )

  expect_error(
    dbAppendTableRLS(
      con,
      "ac_append_validation",
      data.frame(missing_col = 1L)
    ),
    "not present in target table"
  )
})


test_that("dbAppendTableRLS returns zero for empty input", {
  con <- connect_test()
  on.exit(cleanup_postgres_session(con))

  DBI::dbExecute(
    con,
    "CREATE TEMP TABLE ac_append_empty (id integer, txt text)"
  )

  expect_error(
    dbAppendTableRLS(
      con,
      "ac_append_empty",
      data.frame(id = integer(), txt = character())
    ),
    "dbAppendTableRLS: 'value' contains no rows to append."
  )
  expect_equal(
    DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM ac_append_empty")$n,
    0
  )
})


test_that("dbAppendTableRLS appends rows with bind strategy", {
  con <- connect_test()
  on.exit(cleanup_postgres_session(con))

  DBI::dbExecute(con, "CREATE TEMP TABLE ac_append_bind (id integer, txt text)")

  df <- data.frame(
    txt = c("a", "b", "c"),
    id = c(1L, 2L, 3L)
  )

  rows <- dbAppendTableRLS(
    con,
    "ac_append_bind",
    df,
    method = "bind",
    chunk_size = 2
  )
  expect_equal(unclass(rows), 3L)

  out <- DBI::dbGetQuery(con, "SELECT id, txt FROM ac_append_bind ORDER BY id")
  expect_equal(out$id, c(1L, 2L, 3L))
  expect_equal(out$txt, c("a", "b", "c"))
})


test_that("dbAppendTableRLS appends rows with staging strategy", {
  con <- connect_test()
  on.exit(cleanup_postgres_session(con))

  DBI::dbExecute(
    con,
    "CREATE TEMP TABLE ac_append_staging (id integer, txt text)"
  )

  df <- data.frame(id = 1:5, txt = letters[1:5])
  rows <- dbAppendTableRLS(con, "ac_append_staging", df, method = "staging")

  expect_equal(unclass(rows), 5L)
  expect_equal(
    DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM ac_append_staging")$n,
    5
  )
})


test_that("dbAppendTableRLS supports ON CONFLICT DO NOTHING", {
  con <- connect_test()
  on.exit(cleanup_postgres_session(con))

  DBI::dbExecute(
    con,
    "CREATE TEMP TABLE ac_append_conflict_nothing (id integer PRIMARY KEY, txt text)"
  )

  DBI::dbExecute(
    con,
    "INSERT INTO ac_append_conflict_nothing (id, txt) VALUES (1, 'old')"
  )

  rows <- dbAppendTableRLS(
    con,
    "ac_append_conflict_nothing",
    data.frame(id = c(1L, 2L), txt = c("new", "fresh")),
    method = "bind",
    on_conflict = "nothing",
    conflict_cols = "id"
  )

  expect_equal(unclass(rows), 1L)

  out <- DBI::dbGetQuery(
    con,
    "SELECT id, txt FROM ac_append_conflict_nothing ORDER BY id"
  )
  expect_equal(out$txt, c("old", "fresh"))
})


test_that("dbAppendTableRLS supports ON CONFLICT DO UPDATE", {
  con <- connect_test()
  on.exit(cleanup_postgres_session(con))

  DBI::dbExecute(
    con,
    "CREATE TEMP TABLE ac_append_conflict_update (id integer PRIMARY KEY, txt text, note text)"
  )

  DBI::dbExecute(
    con,
    "INSERT INTO ac_append_conflict_update (id, txt, note) VALUES (1, 'old', 'keep')"
  )

  rows <- dbAppendTableRLS(
    con,
    "ac_append_conflict_update",
    data.frame(
      id = c(1L, 2L),
      txt = c("new", "fresh"),
      note = c("replace", "new-row")
    ),
    method = "staging",
    on_conflict = "update",
    conflict_cols = "id",
    update_cols = c("txt", "note")
  )

  expect_equal(unclass(rows), 2L)

  out <- DBI::dbGetQuery(
    con,
    "SELECT id, txt, note FROM ac_append_conflict_update ORDER BY id"
  )
  expect_equal(out$txt, c("new", "fresh"))
  expect_equal(out$note, c("replace", "new-row"))
})


test_that("dbAppendTableRLS validates conflict arguments", {
  con <- connect_test()
  on.exit(cleanup_postgres_session(con))

  DBI::dbExecute(
    con,
    "CREATE TEMP TABLE ac_append_conflict_validate (id integer PRIMARY KEY, txt text)"
  )

  df <- data.frame(id = 1L, txt = "x")

  expect_error(
    dbAppendTableRLS(
      con,
      "ac_append_conflict_validate",
      df,
      on_conflict = "nothing"
    ),
    "conflict_cols"
  )

  expect_error(
    dbAppendTableRLS(
      con,
      "ac_append_conflict_validate",
      df,
      on_conflict = "update",
      conflict_cols = "id"
    ),
    "update_cols"
  )

  expect_error(
    dbAppendTableRLS(
      con,
      "ac_append_conflict_validate",
      df,
      on_conflict = "update",
      conflict_cols = "id",
      update_cols = c("id", "txt")
    ),
    "cannot include"
  )
})


test_that("dbAppendTableRLS works for RLS tables when role cannot use COPY FROM", {
  con <- connect_test()
  on.exit(cleanup_postgres_session(con))

  role <- paste0("ac_rls_test_", as.integer(stats::runif(1, 10000, 99999)))

  role_ready <- tryCatch(
    {
      DBI::dbExecute(
        con,
        paste0("DROP ROLE IF EXISTS ", DBI::dbQuoteIdentifier(con, role))
      )
      DBI::dbExecute(
        con,
        paste0("CREATE ROLE ", DBI::dbQuoteIdentifier(con, role), " NOLOGIN")
      )
      DBI::dbExecute(
        con,
        paste0(
          "GRANT ",
          DBI::dbQuoteIdentifier(con, role),
          " TO CURRENT_USER"
        )
      )
      TRUE
    },
    error = function(e) FALSE
  )

  if (!role_ready) {
    skip("Unable to create test role for RLS behavior test.")
  }

  on.exit(
    {
      suppressWarnings(try(DBI::dbExecute(con, "RESET ROLE"), silent = TRUE))
      suppressWarnings(
        try(
          DBI::dbExecute(
            con,
            paste0("DROP ROLE IF EXISTS ", DBI::dbQuoteIdentifier(con, role))
          ),
          silent = TRUE
        )
      )
    },
    add = TRUE
  )

  DBI::dbExecute(con, "DROP TABLE IF EXISTS ac_append_rls")
  DBI::dbExecute(
    con,
    "CREATE TABLE ac_append_rls (id integer PRIMARY KEY, txt text)"
  )
  DBI::dbExecute(con, "ALTER TABLE ac_append_rls ENABLE ROW LEVEL SECURITY")
  DBI::dbExecute(
    con,
    "CREATE POLICY ac_append_rls_policy ON ac_append_rls FOR ALL USING (TRUE) WITH CHECK (TRUE)"
  )

  DBI::dbExecute(
    con,
    paste0(
      "GRANT SELECT, INSERT, UPDATE ON ac_append_rls TO ",
      DBI::dbQuoteIdentifier(con, role)
    )
  )

  can_set_role <- tryCatch(
    {
      DBI::dbExecute(
        con,
        paste0("SET ROLE ", DBI::dbQuoteIdentifier(con, role))
      )
      TRUE
    },
    error = function(e) FALSE
  )

  if (!can_set_role) {
    skip("Unable to SET ROLE for RLS test.")
  }

  expect_error(
    DBI::dbAppendTable(con, "ac_append_rls", data.frame(id = 1L, txt = "x")),
    "COPY FROM not supported with row-level security"
  )

  expect_no_error(
    dbAppendTableRLS(
      con,
      "ac_append_rls",
      data.frame(id = 1L, txt = "x"),
      method = "staging"
    )
  )

  out <- DBI::dbGetQuery(con, "SELECT id, txt FROM ac_append_rls")
  expect_equal(nrow(out), 1)
  expect_equal(out$id, 1L)
  expect_equal(out$txt, "x")
})
