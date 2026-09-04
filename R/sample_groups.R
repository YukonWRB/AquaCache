#' Create a discrete sample group
#'
#' Creates a group used to relate routine samples, blanks, controls, and other
#' samples that share a field event, trip, cooler, shipment, laboratory batch,
#' or quality-control context.
#'
#' When `group_code` is supplied, groups are identified by the database's
#' owner/type/code key. With `existing = "return"`, an existing matching group
#' is reused, which makes source-adapter imports and synchronization idempotent.
#'
#' @param con A DBI connection to AquaCache.
#' @param group_type An active code from `discrete.sample_group_types`, as
#'   returned by [getSampleGroupTypes()].
#' @param owner Organization ID that owns the group.
#' @param group_code Optional external or operational group identifier.
#' @param group_name Optional human-readable group name. At least one of
#'   `group_code` or `group_name` is required.
#' @param start_datetime,end_datetime Optional group period.
#' @param contributor Optional contributing organization ID.
#' @param metadata Optional named list or JSON object with source-specific
#'   metadata.
#' @param active Whether the group is active.
#' @param note Optional group note.
#' @param share_with Database roles with which to share the group.
#' @param existing How to handle an existing owner/type/code match. `"return"`
#'   returns its ID; `"error"` lets the database uniqueness error propagate.
#'
#' @return The integer `sample_group_id`.
#' @export
createSampleGroup <- function(
  con,
  group_type,
  owner,
  group_code = NULL,
  group_name = NULL,
  start_datetime = NULL,
  end_datetime = NULL,
  contributor = NULL,
  metadata = list(),
  active = TRUE,
  note = NULL,
  share_with = "public_reader",
  existing = c("return", "error")
) {
  existing <- match.arg(existing)
  if (length(group_type) != 1L || is.na(group_type)) {
    stop("group_type must be one active sample-group type code.")
  }
  group_type <- trimws(as.character(group_type))
  if (!nzchar(group_type)) {
    stop("group_type must be one active sample-group type code.")
  }
  valid_group_type <- DBI::dbGetQuery(
    con,
    "SELECT group_type
     FROM discrete.sample_group_types
     WHERE group_type = $1 AND active",
    params = list(group_type)
  )
  if (nrow(valid_group_type) != 1L) {
    stop(
      "group_type is not an active code in discrete.sample_group_types: ",
      group_type,
      "."
    )
  }

  owner <- suppressWarnings(as.integer(owner))
  if (length(owner) != 1L || is.na(owner)) {
    stop("owner must be one organization_id.")
  }

  clean_text <- function(x) {
    if (is.null(x) || length(x) == 0L || is.na(x[[1]])) {
      return(NA_character_)
    }
    value <- trimws(as.character(x[[1]]))
    if (!nzchar(value)) NA_character_ else value
  }
  group_code <- clean_text(group_code)
  group_name <- clean_text(group_name)
  note <- clean_text(note)
  if (is.na(group_code) && is.na(group_name)) {
    stop("At least one of group_code or group_name must be supplied.")
  }

  contributor <- suppressWarnings(as.integer(contributor))
  if (length(contributor) == 0L || is.na(contributor[[1]])) {
    contributor <- NA_integer_
  } else if (length(contributor) != 1L) {
    stop("contributor must be one organization_id or NULL.")
  }

  if (is.null(metadata) || length(metadata) == 0L) {
    metadata_json <- "{}"
  } else if (is.character(metadata) && length(metadata) == 1L) {
    metadata_json <- metadata
  } else {
    metadata_json <- as.character(jsonlite::toJSON(
      metadata,
      auto_unbox = TRUE,
      null = "null",
      na = "null"
    ))
  }

  share_with <- unique(as.character(share_with))
  share_with <- share_with[!is.na(share_with) & nzchar(trimws(share_with))]
  if (length(share_with) == 0L) {
    stop("share_with must contain at least one database role.")
  }
  share_with_json <- as.character(jsonlite::toJSON(
    share_with,
    auto_unbox = FALSE
  ))

  conflict_sql <- if (existing == "return" && !is.na(group_code)) {
    "ON CONFLICT (
       owner,
       group_type,
       (lower(btrim(group_code)))
     ) WHERE group_code IS NOT NULL DO NOTHING"
  } else {
    ""
  }

  inserted <- DBI::dbGetQuery(
    con,
    paste(
      "INSERT INTO discrete.sample_groups (",
      "  group_type, group_code, group_name, start_datetime, end_datetime,",
      "  owner, contributor, metadata, active, note, share_with",
      ") VALUES (",
      "  $1, $2, $3, $4, $5, $6, $7, $8::jsonb, $9, $10,",
      "  ARRAY(SELECT jsonb_array_elements_text($11::jsonb))",
      ")",
      conflict_sql,
      "RETURNING sample_group_id;"
    ),
    params = list(
      group_type,
      group_code,
      group_name,
      if (is.null(start_datetime)) as.POSIXct(NA) else start_datetime,
      if (is.null(end_datetime)) as.POSIXct(NA) else end_datetime,
      owner,
      contributor,
      metadata_json,
      isTRUE(active),
      note,
      share_with_json
    )
  )

  if (nrow(inserted) == 1L) {
    return(as.integer(inserted$sample_group_id[[1]]))
  }

  matched <- DBI::dbGetQuery(
    con,
    "SELECT sample_group_id
     FROM discrete.sample_groups
     WHERE owner = $1
       AND group_type = $2
       AND lower(btrim(group_code)) = lower(btrim($3))
     LIMIT 1;",
    params = list(owner, group_type, group_code)
  )
  if (nrow(matched) != 1L) {
    stop("The matching sample group exists but is not visible to this user.")
  }
  as.integer(matched$sample_group_id[[1]])
}


#' List discrete sample-group types
#'
#' Retrieves the governed bilingual catalogue used by sample-group creation
#' and application controls.
#'
#' @param con A DBI connection to AquaCache.
#' @param active_only If `TRUE`, return only active types.
#'
#' @return A data.table ordered by `sort_order` and `group_type`.
#' @export
getSampleGroupTypes <- function(con, active_only = TRUE) {
  if (
    !is.logical(active_only) || length(active_only) != 1L || is.na(active_only)
  ) {
    stop("active_only must be TRUE or FALSE.")
  }
  sql <- "SELECT
            group_type,
            group_type_name,
            group_type_name_fr,
            description,
            description_fr,
            sort_order,
            active
          FROM discrete.sample_group_types"
  if (active_only) {
    sql <- paste0(sql, " WHERE active")
  }
  data.table::as.data.table(DBI::dbGetQuery(
    con,
    paste0(sql, " ORDER BY sort_order, group_type")
  ))
}


#' Assign samples to discrete sample groups
#'
#' Adds one or more rows to `discrete.sample_group_members`. Scalar arguments
#' are recycled, allowing one group to be assigned to many samples or one
#' sample to be assigned to many groups.
#'
#' @param con A DBI connection to AquaCache.
#' @param sample_id One or more discrete sample IDs.
#' @param sample_group_id One or more sample group IDs.
#' @param sequence_in_group Optional positive collection or processing order.
#' @param note Optional membership note.
#' @param existing How to handle an existing group/sample membership:
#'   `"nothing"` leaves it unchanged, `"update"` replaces its sequence and
#'   note, and `"error"` lets the uniqueness error propagate.
#'
#' @return Invisibly, the number of inserted or affected membership rows.
#' @export
assignSamplesToGroup <- function(
  con,
  sample_id,
  sample_group_id,
  sequence_in_group = NULL,
  note = NULL,
  existing = c("nothing", "update", "error")
) {
  existing <- match.arg(existing)
  lengths <- c(length(sample_id), length(sample_group_id))
  if (!is.null(sequence_in_group)) {
    lengths <- c(lengths, length(sequence_in_group))
  }
  if (!is.null(note)) {
    lengths <- c(lengths, length(note))
  }
  n <- max(lengths)
  if (n == 0L || any(!lengths %in% c(1L, n))) {
    stop("Membership arguments must have length one or a common length.")
  }

  recycle <- function(x, default) {
    if (is.null(x)) rep(default, n) else rep(x, length.out = n)
  }
  sample_id <- suppressWarnings(as.integer(recycle(sample_id, NA_integer_)))
  sample_group_id <- suppressWarnings(as.integer(recycle(
    sample_group_id,
    NA_integer_
  )))
  sequence_in_group <- suppressWarnings(as.integer(recycle(
    sequence_in_group,
    NA_integer_
  )))
  note <- as.character(recycle(note, NA_character_))

  if (any(is.na(sample_id) | sample_id < 1L)) {
    stop("sample_id values must be positive integers.")
  }
  if (any(is.na(sample_group_id) | sample_group_id < 1L)) {
    stop("sample_group_id values must be positive integers.")
  }
  if (any(!is.na(sequence_in_group) & sequence_in_group < 1L)) {
    stop("sequence_in_group values must be positive integers or NA.")
  }

  memberships <- data.frame(
    sample_group_id = sample_group_id,
    sample_id = sample_id,
    sequence_in_group = sequence_in_group,
    note = note,
    stringsAsFactors = FALSE
  )
  affected <- if (existing == "error") {
    dbAppendTableRLS(con, "discrete.sample_group_members", memberships)
  } else {
    dbAppendTableRLS(
      con,
      "discrete.sample_group_members",
      memberships,
      on_conflict = existing,
      conflict_cols = c("sample_group_id", "sample_id"),
      update_cols = if (existing == "update") {
        c("sequence_in_group", "note")
      } else {
        NULL
      }
    )
  }
  invisible(affected)
}


#' Resolve and assign sample-group specifications during import
#'
#' Resolves the group specifications returned by a discrete source adapter and
#' assigns the resulting groups to one sample. A specification may identify an
#' existing group by `sample_group_id`, or provide the fields needed by
#' [createSampleGroup()]. Group creation is idempotent for specifications with
#' an owner, type, and code.
#'
#' The operation participates in the caller's transaction when one is already
#' active. Otherwise, it opens a transaction so that group creation and sample
#' membership assignment succeed or fail together.
#'
#' @param con An open DBI connection to an AquaCache database.
#' @param sample_id The integer ID of the sample to assign.
#' @param sample_groups Either a numeric vector of existing sample-group IDs or
#'   a data frame with one group specification per row. Data-frame rows may use
#'   `sample_group_id` directly, or provide `group_type` plus the arguments
#'   accepted by [createSampleGroup()]. Optional `sequence_in_group` and
#'   `member_note` columns describe the resulting membership.
#' @param default_owner Organization ID used when a specification does not
#'   provide `owner`.
#' @param default_contributor Organization ID used when a specification does
#'   not provide `contributor`.
#'
#' @return An integer vector of resolved `sample_group_id` values. An empty
#'   integer vector is returned when no groups are supplied.
#'
#' @keywords internal
#' @noRd
link_discrete_sample_groups <- function(
  con,
  sample_id,
  sample_groups,
  default_owner = NA_integer_,
  default_contributor = NA_integer_
) {
  if (is.null(sample_groups) || length(sample_groups) == 0L) {
    return(integer())
  }
  if (is.numeric(sample_groups)) {
    sample_groups <- data.frame(sample_group_id = sample_groups)
  }
  if (!inherits(sample_groups, "data.frame")) {
    stop(
      "sample_groups must be a data.frame or a vector of sample_group_id values."
    )
  }
  if (nrow(sample_groups) == 0L) {
    return(integer())
  }

  value_at <- function(column, i, default = NULL) {
    if (!column %in% names(sample_groups)) {
      return(default)
    }
    value <- sample_groups[[column]]
    if (is.list(value)) value[[i]] else value[i]
  }

  active_trans <- dbTransBegin(con)
  transaction_finished <- FALSE
  on.exit(
    if (active_trans && !transaction_finished) {
      try(DBI::dbExecute(con, "ROLLBACK;"), silent = TRUE)
    },
    add = TRUE
  )
  tryCatch(
    {
      group_ids <- integer(nrow(sample_groups))
      for (i in seq_len(nrow(sample_groups))) {
        supplied_id <- suppressWarnings(as.integer(value_at(
          "sample_group_id",
          i,
          NA_integer_
        )))
        if (!is.na(supplied_id)) {
          group_ids[[i]] <- supplied_id
          next
        }

        group_owner <- value_at("owner", i, default_owner)
        if (
          is.null(group_owner) ||
            length(group_owner) == 0L ||
            is.na(group_owner)
        ) {
          group_owner <- default_owner
        }
        group_contributor <- value_at(
          "contributor",
          i,
          default_contributor
        )
        if (
          is.null(group_contributor) ||
            length(group_contributor) == 0L ||
            is.na(group_contributor)
        ) {
          group_contributor <- default_contributor
        }
        group_ids[[i]] <- createSampleGroup(
          con = con,
          group_type = value_at("group_type", i),
          owner = group_owner,
          group_code = value_at("group_code", i),
          group_name = value_at("group_name", i),
          start_datetime = value_at("start_datetime", i),
          end_datetime = value_at("end_datetime", i),
          contributor = group_contributor,
          metadata = value_at("metadata", i, list()),
          active = value_at("active", i, TRUE),
          note = value_at("note", i),
          share_with = value_at("share_with", i, "public_reader"),
          existing = "return"
        )
      }

      assignSamplesToGroup(
        con = con,
        sample_id = sample_id,
        sample_group_id = group_ids,
        sequence_in_group = if ("sequence_in_group" %in% names(sample_groups)) {
          sample_groups$sequence_in_group
        } else {
          NULL
        },
        note = if ("member_note" %in% names(sample_groups)) {
          sample_groups$member_note
        } else {
          NULL
        },
        existing = "nothing"
      )
      if (active_trans) {
        DBI::dbExecute(con, "COMMIT;")
      }
      transaction_finished <- TRUE
      group_ids
    },
    error = function(e) {
      if (active_trans) {
        DBI::dbExecute(con, "ROLLBACK;")
        transaction_finished <<- TRUE
      }
      stop(e)
    }
  )
}


#' Find an existing imported sample without a location
#'
#' Looks up a locationless discrete sample by the stable source identity used
#' during synchronization. This supports idempotent updates to field blanks and
#' other samples whose context is represented by sample groups rather than a
#' monitoring location.
#'
#' @param con An open DBI connection to an AquaCache database.
#' @param import_source The source-adapter or import-source name stored on the
#'   sample.
#' @param import_source_id The source system's sample identifier. A missing or
#'   empty value disables the lookup.
#'
#' @return A data frame containing at most one matching row from
#'   `discrete.samples`. An empty data frame is returned when no usable source
#'   ID is supplied or no visible row matches.
#'
#' @keywords internal
#' @noRd
find_locationless_import_sample <- function(
  con,
  import_source,
  import_source_id
) {
  if (
    is.null(import_source_id) ||
      length(import_source_id) == 0L ||
      is.na(import_source_id[[1]])
  ) {
    return(data.frame())
  }
  DBI::dbGetQuery(
    con,
    "SELECT *
     FROM discrete.samples
     WHERE location_id IS NULL
       AND import_source = $1
       AND import_source_id = $2
     LIMIT 1;",
    params = list(import_source, as.character(import_source_id[[1]]))
  )
}


#' Construct one discrete-import result record
#'
#' Packages the outcome of one sample import into the list-column structure
#' returned by discrete source-adapter ingestion and consumed by
#' synchronization. Related sample groups, qualifiers, observers, aggregation
#' metadata, and component observations remain attached to the same record.
#'
#' @param sample_series_id Integer ID of the source sample series.
#' @param sample_id Integer database ID of the affected sample.
#' @param action Character value describing the import outcome, such as an
#'   insertion, update, or skip.
#' @param sample The sample data associated with the outcome.
#' @param results The result data associated with the sample.
#' @param sample_groups Optional sample-group specifications or memberships.
#' @param sample_qualifiers Optional normalized sample-qualifier rows.
#' @param sample_observers Optional normalized sample-observer rows.
#' @param result_aggregations Optional normalized result-aggregation rows.
#' @param result_components Optional normalized result-component rows.
#'
#' @return A one-row data.table with scalar import identifiers and list columns
#'   holding the sample and all related records.
#'
#' @keywords internal
#' @noRd
new_discrete_import_record <- function(
  sample_series_id,
  sample_id,
  action,
  sample,
  results,
  sample_groups = NULL,
  sample_qualifiers = NULL,
  sample_observers = NULL,
  result_aggregations = NULL,
  result_components = NULL
) {
  data.table::data.table(
    sample_series_id = as.integer(sample_series_id),
    sample_id = as.integer(sample_id),
    action = as.character(action),
    sample = list(sample),
    results = list(results),
    sample_groups = list(sample_groups),
    sample_qualifiers = list(sample_qualifiers),
    sample_observers = list(sample_observers),
    result_aggregations = list(result_aggregations),
    result_components = list(result_components)
  )
}


#' Combine discrete-import result records
#'
#' Combines the one-row records produced by
#' `new_discrete_import_record()` (an internal function) into
#' one consistently shaped data.table. The empty-input path returns the same
#' schema, including every relationship list column, so callers do not need a
#' special case when no samples were processed.
#'
#' @param records A list of data.tables returned by
#'   `new_discrete_import_record()`.
#'
#' @return A data.table containing all supplied import records, or a zero-row
#'   data.table with the standard import-record columns.
#'
#' @keywords internal
#' @noRd
bind_discrete_import_records <- function(records) {
  if (length(records) == 0L) {
    return(data.table::data.table(
      sample_series_id = integer(),
      sample_id = integer(),
      action = character(),
      sample = list(),
      results = list(),
      sample_groups = list(),
      sample_qualifiers = list(),
      sample_observers = list(),
      result_aggregations = list(),
      result_components = list()
    ))
  }
  data.table::rbindlist(records, use.names = TRUE, fill = TRUE)
}
