# Functions to adjust the grade, qualifier, approval, owner, and contributor, and data sharing agreement of continuous-type data as it's appended to the database.

# Helper functions; not exported

#' @title Collapse segments with split
#' @description
#'  Collapse existing segments with new segments, splitting at boundaries and optionally bridging the latest existing segment to the next new segment if they have the same value.
#' @param exist A data.frame of existing segments with columns for id, timeseries_id, value, start_dt, and end_dt.
#' @param new_segments A data.frame of new segments with columns for id, timeseries_id, value, start_dt, and end_dt.
#' @param value_col The name of the column containing the value to compare for collapsing.
#' @param id_col The name of the column containing the unique identifier for segments.
#' @param timeseries_id The timeseries_id to assign to the final segments.
#' @param bridge_latest_extension Logical. If TRUE, if the latest existing segment ends before the earliest new segment starts and they have the same value, treat the existing segment as continuing through the start of the new segment instead of creating a brand-new trailing segment.
#' @param protection_col Optional logical column that distinguishes source-protected and source-managed segments. Segments are only collapsed when both their value and protection state match.
#' @return A data.frame of segments with columns for id, timeseries_id, value, start_dt, and end_dt, where consecutive segments with the same value have been collapsed and the new segments have been integrated with the existing segments.
#' @noRd
#' @keywords internal

collapse_segments_with_split <- function(
  exist,
  new_segments,
  value_col,
  id_col,
  timeseries_id,
  bridge_latest_extension = FALSE,
  protection_col = NULL
) {
  if (nrow(new_segments) == 0) {
    return(exist)
  }

  exist <- exist[order(exist$start_dt, exist$end_dt), , drop = FALSE]
  new_segments <- new_segments[
    order(new_segments$start_dt, new_segments$end_dt),
    ,
    drop = FALSE
  ]

  if (!is.null(protection_col)) {
    if (!(protection_col %in% names(exist))) {
      exist[[protection_col]] <- FALSE
    }
    if (!(protection_col %in% names(new_segments))) {
      new_segments[[protection_col]] <- FALSE
    }
    exist[[protection_col]][is.na(exist[[protection_col]])] <- FALSE
    new_segments[[protection_col]][
      is.na(new_segments[[protection_col]])
    ] <- FALSE
  }

  if (bridge_latest_extension && nrow(exist) > 0) {
    latest_existing_idx <- which.max(exist$end_dt)
    first_new_idx <- which.min(new_segments$start_dt)

    if (
      length(latest_existing_idx) == 1 &&
        length(first_new_idx) == 1 &&
        exist$end_dt[latest_existing_idx] <
          new_segments$start_dt[first_new_idx] &&
        identical(
          exist[[value_col]][latest_existing_idx],
          new_segments[[value_col]][first_new_idx]
        ) &&
        (
          is.null(protection_col) ||
            identical(
              exist[[protection_col]][latest_existing_idx],
              new_segments[[protection_col]][first_new_idx]
            )
        )
    ) {
      # When appending new data with the same qualifying value, treat the
      # latest existing segment as continuing through the next imported block
      # instead of creating a brand-new trailing row.
      exist$end_dt[latest_existing_idx] <- new_segments$start_dt[first_new_idx]
    }
  }

  boundaries <- sort(unique(c(
    as.POSIXct(exist$start_dt, tz = "UTC"),
    as.POSIXct(exist$end_dt, tz = "UTC"),
    as.POSIXct(new_segments$start_dt, tz = "UTC"),
    as.POSIXct(new_segments$end_dt, tz = "UTC")
  )))

  rebuilt <- data.frame(
    start_dt = as.POSIXct(character()),
    end_dt = as.POSIXct(character()),
    value = numeric(),
    protected = logical(),
    stringsAsFactors = FALSE
  )

  if (length(boundaries) >= 2) {
    for (i in seq_len(length(boundaries) - 1)) {
      start_i <- boundaries[i]
      end_i <- boundaries[i + 1]
      if (start_i >= end_i) {
        next
      }

      new_match <- which(
        new_segments$start_dt <= start_i & new_segments$end_dt >= end_i
      )
      if (length(new_match) > 0) {
        value_i <- new_segments[[value_col]][new_match[1]]
        protected_i <- if (is.null(protection_col)) {
          FALSE
        } else {
          new_segments[[protection_col]][new_match[1]]
        }
      } else {
        old_match <- which(exist$start_dt <= start_i & exist$end_dt >= end_i)
        value_i <- if (length(old_match) > 0) {
          exist[[value_col]][old_match[1]]
        } else {
          NA
        }
        protected_i <- if (is.null(protection_col) || length(old_match) == 0) {
          FALSE
        } else {
          exist[[protection_col]][old_match[1]]
        }
      }

      if (!is.na(value_i)) {
        rebuilt <- rbind(
          rebuilt,
          data.frame(
            start_dt = start_i,
            end_dt = end_i,
            value = value_i,
            protected = protected_i
          )
        )
      }
    }
  }

  if (nrow(rebuilt) == 0) {
    rebuilt <- data.frame(
      start_dt = new_segments$start_dt,
      end_dt = new_segments$end_dt,
      value = new_segments[[value_col]],
      protected = if (is.null(protection_col)) {
        FALSE
      } else {
        new_segments[[protection_col]]
      }
    )
  }

  merged <- rebuilt[1, , drop = FALSE]
  if (nrow(rebuilt) > 1) {
    for (i in 2:nrow(rebuilt)) {
      same_value <- identical(merged$value[nrow(merged)], rebuilt$value[i])
      same_protection <- identical(
        merged$protected[nrow(merged)],
        rebuilt$protected[i]
      )
      contiguous <- identical(merged$end_dt[nrow(merged)], rebuilt$start_dt[i])
      if (same_value && same_protection && contiguous) {
        merged$end_dt[nrow(merged)] <- rebuilt$end_dt[i]
      } else {
        merged <- rbind(merged, rebuilt[i, , drop = FALSE])
      }
    }
  }

  final <- data.frame(
    id = NA,
    timeseries_id = timeseries_id,
    value = merged$value,
    start_dt = merged$start_dt,
    end_dt = merged$end_dt
  )

  names(final) <- c(id_col, "timeseries_id", value_col, "start_dt", "end_dt")
  if (!is.null(protection_col)) {
    final[[protection_col]] <- merged$protected
  }

  state_cols <- c(value_col, "start_dt", "end_dt")
  if (!is.null(protection_col)) {
    state_cols <- c(state_cols, protection_col)
  }
  used_ids <- integer()

  # Exact matches are aligned first so immutable source-protected intervals
  # retain their IDs even when new segments are inserted before them.
  for (i in seq_len(nrow(final))) {
    exact <- !is.na(exist[[id_col]])
    for (state_col in state_cols) {
      exact <- exact & exist[[state_col]] == final[[state_col]][i]
    }
    exact <- which(exact & !(exist[[id_col]] %in% used_ids))
    if (length(exact) > 0) {
      final[[id_col]][i] <- exist[[id_col]][exact[1]]
      used_ids <- c(used_ids, final[[id_col]][i])
    }
  }

  unused_ids <- exist[[id_col]][
    !is.na(exist[[id_col]]) & !(exist[[id_col]] %in% used_ids)
  ]
  for (i in which(is.na(final[[id_col]]))) {
    if (length(unused_ids) == 0) {
      break
    }
    final[[id_col]][i] <- unused_ids[1]
    used_ids <- c(used_ids, unused_ids[1])
    unused_ids <- unused_ids[-1]
  }

  remove_rows <- exist[
    !is.na(exist[[id_col]]) & !(exist[[id_col]] %in% used_ids),
    ,
    drop = FALSE
  ]
  if (nrow(remove_rows) > 0) {
    remove_rows <- remove_rows[
      ,
      c(id_col, "timeseries_id", value_col, "start_dt", "end_dt", protection_col),
      drop = FALSE
    ]
    remove_rows$timeseries_id <- -1
    final <- rbind(final, remove_rows)
  }

  final
}

#' @title Build interval segments from point attributes
#' @description Collapse ordered point attributes into segments, retaining
#' source-protection boundaries even when the attribute value is unchanged.
#' @param data A data.frame containing `datetime`, a value column, and optional
#' `no_source_update`.
#' @param value_col Attribute value column.
#' @param id_col Segment ID column to create.
#' @param timeseries_id Target time series.
#' @return A data.frame of interval segments.
#' @noRd
#' @keywords internal
build_attribute_segments <- function(data, value_col, id_col, timeseries_id) {
  data <- data[order(data$datetime), , drop = FALSE]
  if (!("no_source_update" %in% names(data))) {
    data$no_source_update <- FALSE
  }
  data$no_source_update[is.na(data$no_source_update)] <- FALSE

  new_run <- c(
    TRUE,
    data[[value_col]][-1] != utils::head(data[[value_col]], -1) |
      data$no_source_update[-1] != utils::head(data$no_source_update, -1)
  )
  run_id <- cumsum(new_run)
  starts <- match(unique(run_id), run_id)
  ends <- vapply(unique(run_id), function(x) max(which(run_id == x)), integer(1))

  segments <- data.frame(
    id = NA_integer_,
    timeseries_id = timeseries_id,
    value = data[[value_col]][starts],
    start_dt = data$datetime[starts],
    end_dt = data$datetime[ends],
    no_source_update = data$no_source_update[starts],
    stringsAsFactors = FALSE
  )
  names(segments)[names(segments) == "id"] <- id_col
  names(segments)[names(segments) == "value"] <- value_col
  segments
}

#' @title Clip source-managed segments around protected intervals
#' @description Subtract immutable source-protected intervals from proposed
#' source-managed segments. Intervals use the database's half-open `[)`
#' semantics.
#' @param segments Proposed segments.
#' @param protected Existing protected segments.
#' @return Proposed segments that do not overlap protected intervals.
#' @noRd
#' @keywords internal
clip_segments_around_protected <- function(segments, protected) {
  if (nrow(segments) == 0 || nrow(protected) == 0) {
    return(segments)
  }

  result <- vector("list", nrow(segments))
  result_count <- 0L
  for (i in seq_len(nrow(segments))) {
    pieces <- segments[i, , drop = FALSE]
    for (j in seq_len(nrow(protected))) {
      if (nrow(pieces) == 0 || protected$start_dt[j] >= protected$end_dt[j]) {
        next
      }
      next_pieces <- vector("list", nrow(pieces) * 2L)
      next_count <- 0L
      for (k in seq_len(nrow(pieces))) {
        piece <- pieces[k, , drop = FALSE]
        overlaps <- piece$start_dt < protected$end_dt[j] &&
          piece$end_dt > protected$start_dt[j]
        if (!overlaps) {
          next_count <- next_count + 1L
          next_pieces[[next_count]] <- piece
          next
        }
        if (piece$start_dt < protected$start_dt[j]) {
          left <- piece
          left$end_dt <- protected$start_dt[j]
          next_count <- next_count + 1L
          next_pieces[[next_count]] <- left
        }
        if (piece$end_dt > protected$end_dt[j]) {
          right <- piece
          right$start_dt <- protected$end_dt[j]
          next_count <- next_count + 1L
          next_pieces[[next_count]] <- right
        }
      }
      pieces <- if (next_count == 0) {
        pieces[FALSE, , drop = FALSE]
      } else {
        do.call(rbind, next_pieces[seq_len(next_count)])
      }
    }
    if (nrow(pieces) > 0) {
      result_count <- result_count + 1L
      result[[result_count]] <- pieces
    }
  }

  if (result_count == 0) {
    return(segments[FALSE, , drop = FALSE])
  }
  result <- do.call(rbind, result[seq_len(result_count)])
  rownames(result) <- NULL
  result
}

#' @title Merge overlapping segments with the same value
#' @description
#' Merge overlapping or touching segments that have the same value while
#' preserving an existing segment ID where possible. Existing IDs made
#' redundant by a merge are returned for deletion.
#' @param segments A data.frame of proposed segments.
#' @param value_col The name of the value column used to group segments.
#' @param id_col The name of the segment ID column.
#' @param protection_col Optional logical column that must also match before
#' segments are merged.
#' @return A list containing the merged `segments` and redundant `delete_ids`.
#' @noRd
#' @keywords internal
merge_overlapping_same_value_segments <- function(
  segments,
  value_col,
  id_col,
  protection_col = NULL
) {
  if (nrow(segments) == 0) {
    return(list(segments = segments, delete_ids = integer()))
  }

  delete_ids <- segments[
    segments$timeseries_id == -1 & !is.na(segments[[id_col]]),
    id_col
  ]
  segments <- segments[
    segments$timeseries_id != -1,
    ,
    drop = FALSE
  ]

  if (nrow(segments) <= 1) {
    return(list(
      segments = segments,
      delete_ids = unique(as.integer(delete_ids))
    ))
  }

  if (!is.null(protection_col) && !(protection_col %in% names(segments))) {
    segments[[protection_col]] <- FALSE
  }
  protection_values <- if (is.null(protection_col)) {
    rep(FALSE, nrow(segments))
  } else {
    segments[[protection_col]]
  }
  order_args <- list(
    segments[[value_col]],
    protection_values,
    segments$start_dt,
    segments$end_dt,
    is.na(segments[[id_col]])
  )
  segments <- segments[do.call(order, order_args), , drop = FALSE]
  protection_values <- if (is.null(protection_col)) {
    rep(FALSE, nrow(segments))
  } else {
    segments[[protection_col]]
  }

  merged <- vector("list", nrow(segments))
  merged_count <- 0L

  group_key <- interaction(
    segments[[value_col]],
    protection_values,
    drop = TRUE,
    lex.order = TRUE
  )
  for (group in unique(group_key)) {
    value_segments <- segments[
      group_key == group,
      ,
      drop = FALSE
    ]
    current <- value_segments[1, , drop = FALSE]

    if (nrow(value_segments) > 1) {
      for (i in 2:nrow(value_segments)) {
        next_segment <- value_segments[i, , drop = FALSE]

        if (next_segment$start_dt <= current$end_dt) {
          current$end_dt <- max(current$end_dt, next_segment$end_dt)

          current_id <- current[[id_col]][1]
          next_id <- next_segment[[id_col]][1]
          if (is.na(current_id) && !is.na(next_id)) {
            current[[id_col]] <- next_id
          } else if (
            !is.na(current_id) &&
              !is.na(next_id) &&
              current_id != next_id
          ) {
            delete_ids <- c(delete_ids, next_id)
          }
        } else {
          merged_count <- merged_count + 1L
          merged[[merged_count]] <- current
          current <- next_segment
        }
      }
    }

    merged_count <- merged_count + 1L
    merged[[merged_count]] <- current
  }

  merged <- do.call(rbind, merged[seq_len(merged_count)])
  rownames(merged) <- NULL

  duplicate_ids <- duplicated(merged[[id_col]]) & !is.na(merged[[id_col]])
  merged[[id_col]][duplicate_ids] <- NA

  list(
    segments = merged,
    delete_ids = unique(as.integer(delete_ids))
  )
}

#' @title Segment state key
#' @description Create a unique key for a set of segments based on the id, timeseries_id, value, start_dt, and end_dt columns, to facilitate comparison of segment states.
#' @param data A data.frame of segments with columns for id, timeseries_id, value, start_dt, and end_dt.
#' @param id_col The name of the column containing the unique identifier for segments.
#' @param value_col The name of the column containing the value to compare for segment state.
#' @return A character vector where each element is a unique key representing the state of the segments in the input data.frame, constructed by concatenating the id, timeseries_id, value, start_dt, and end_dt for each segment.
#' @noRd
#' @keywords internal
segment_state_key <- function(data, id_col, value_col) {
  if (nrow(data) == 0) {
    return(character())
  }

  state_cols <- c(id_col, "timeseries_id", value_col, "start_dt", "end_dt")
  if ("no_source_update" %in% names(data)) {
    state_cols <- c(state_cols, "no_source_update")
  }
  data <- data[
    order(data$start_dt, data$end_dt),
    state_cols,
    drop = FALSE
  ]

  key <- paste(
    ifelse(is.na(data[[id_col]]), "NA", as.character(data[[id_col]])),
    ifelse(
      is.na(data$timeseries_id),
      "NA",
      as.character(data$timeseries_id)
    ),
    ifelse(
      is.na(data[[value_col]]),
      "NA",
      as.character(data[[value_col]])
    ),
    fmt(data$start_dt),
    fmt(data$end_dt),
    sep = "|"
  )
  if ("no_source_update" %in% names(data)) {
    key <- paste(key, data$no_source_update, sep = "|")
  }
  key
}

#' @title Segment state identical
#' @description Check if the state of two sets of segments is identical.
#' @param current A data.frame of the current segment state.
#' @param proposed A data.frame of the proposed segment state.
#' @param id_col The name of the column containing the unique identifier for segments.
#' @param value_col The name of the column containing the value to compare for segment state.
#' @return TRUE if the segment states are identical, FALSE otherwise.
#' @noRd
#' @keywords internal
segments_identical <- function(current, proposed, id_col, value_col) {
  identical(
    segment_state_key(current, id_col, value_col),
    segment_state_key(proposed, id_col, value_col)
  )
}

#' @title Get IDs for synchronization deletion
#' @description Retrieve the IDs of segments that should be deleted to synchronize with a remote data store, based on the timeseries_id and a minimum datetime threshold.
#' @param con A connection to the database.
#' @param table_name The schema-qualified name of the table to query for segments (e.g., "continuous.grades", "continuous.qualifiers", "continuous.approvals").
#' @param id_col The name of the column containing the unique identifier for segments in the specified table.
#' @param timeseries_id The timeseries_id for which to retrieve segment IDs.
#' @param min_datetime The minimum datetime threshold; segments with a start_dt greater than or equal to this value will be considered for deletion.
#' @param protect_source_updates If TRUE, exclude rows marked
#' `no_source_update` from deletion candidates.
#' @return An integer vector of segment IDs that should be deleted to synchronize with the remote data store. If no segments meet the criteria, an empty integer vector is returned.
#' @noRd
#' @keywords internal
get_sync_delete_ids <- function(
  con,
  table_name,
  id_col,
  timeseries_id,
  min_datetime,
  protect_source_updates = FALSE
) {
  protection_sql <- if (protect_source_updates) {
    " AND no_source_update IS FALSE"
  } else {
    ""
  }
  ids <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT %s FROM %s WHERE timeseries_id = $1 AND start_dt >= $2%s;",
      id_col,
      table_name,
      protection_sql
    ),
    params = list(timeseries_id, min_datetime)
  )

  if (nrow(ids) == 0) {
    return(integer())
  }

  ids[[1]]
}

#' @title Reconcile segment changes
#' @description Reconcile the changes between the existing state and proposed state of segments by performing the necessary deletions, updates, and insertions in the database to align with the proposed state.
#' @param con A connection to the database.
#' @param table_name The schema-qualified name of the table to update segments in (e.g., "continuous.grades", "continuous.qualifiers", "continuous.approvals").
#' @param id_col The name of the column containing the unique identifier for segments in the specified table.
#' @param value_col The name of the column containing the value to compare for segment state in the input data.frames.
#' @param db_value_col The name of the column containing the value to update in the database table.
#' @param existing_state A data.frame representing the existing state of segments, with columns for id, timeseries_id, value, start_dt, and end_dt.
#' @param proposed_state A data.frame representing the proposed state of segments, with columns for id, timeseries_id, value, start_dt, and end_dt.
#' @param delete_ids An integer vector of segment IDs that should be deleted as part of the reconciliation process, in addition to any deletions determined by comparing the existing and proposed states.
#' @param protection_col Optional source-protection column persisted with each
#' segment.
#' @param source_update If TRUE, database-level delete and update safeguards
#' prevent changes to protected rows.
#' @return TRUE if any changes were made to the database, FALSE if the existing state and proposed state are identical and no changes were necessary. The function performs the necessary deletions, updates, and insertions in the database to align with the proposed state.
#' @noRd
#' @keywords internal
reconcile_segment_changes <- function(
  con,
  table_name,
  id_col,
  value_col,
  db_value_col,
  existing_state,
  proposed_state,
  delete_ids = integer(),
  protection_col = NULL,
  source_update = FALSE
) {
  proposed_delete_ids <- proposed_state[
    proposed_state$timeseries_id == -1,
    id_col
  ]
  proposed_state <- proposed_state[
    proposed_state$timeseries_id != -1,
    ,
    drop = FALSE
  ]

  delete_ids <- unique(c(delete_ids, proposed_delete_ids))
  delete_ids <- delete_ids[!is.na(delete_ids)]

  kept_ids <- proposed_state[[id_col]]
  kept_ids <- unique(kept_ids[!is.na(kept_ids)])
  delete_ids <- setdiff(delete_ids, kept_ids)

  existing_remaining <- existing_state
  if (length(delete_ids) > 0) {
    existing_remaining <- existing_remaining[
      !(existing_remaining[[id_col]] %in% delete_ids),
      ,
      drop = FALSE
    ]
  }

  if (
    length(delete_ids) == 0 &&
      segments_identical(existing_remaining, proposed_state, id_col, value_col)
  ) {
    return(invisible(FALSE))
  }

  if (length(delete_ids) > 0) {
    protection_sql <- if (source_update && !is.null(protection_col)) {
      paste0(" AND ", protection_col, " IS FALSE")
    } else {
      ""
    }
    DBI::dbExecute(
      con,
      paste0(
        "DELETE FROM ",
        table_name,
        " WHERE ",
        id_col,
        " IN (",
        paste(delete_ids, collapse = ", "),
        ")",
        protection_sql,
        ";"
      )
    )
  }

  for (i in seq_len(nrow(proposed_state))) {
    proposed_row <- proposed_state[i, , drop = FALSE]
    if (!is.na(proposed_row[[id_col]])) {
      current_row <- existing_state[
        existing_state[[id_col]] == proposed_row[[id_col]],
        ,
        drop = FALSE
      ]
      if (
        nrow(current_row) == 1 &&
          segments_identical(current_row, proposed_row, id_col, value_col)
      ) {
        next
      }

      if (is.null(protection_col)) {
        update_sql <- sprintf(
          "UPDATE %s SET %s = $1, start_dt = $2, end_dt = $3 WHERE %s = $4;",
          table_name,
          db_value_col,
          id_col
        )
        update_params <- list(
          proposed_row[[value_col]][1],
          proposed_row$start_dt[1],
          proposed_row$end_dt[1],
          proposed_row[[id_col]][1]
        )
      } else {
        protection_sql <- if (source_update) {
          paste0(" AND ", protection_col, " IS FALSE")
        } else {
          ""
        }
        update_sql <- sprintf(
          "UPDATE %s SET %s = $1, start_dt = $2, end_dt = $3, %s = $4 WHERE %s = $5%s;",
          table_name,
          db_value_col,
          protection_col,
          id_col,
          protection_sql
        )
        update_params <- list(
          proposed_row[[value_col]][1],
          proposed_row$start_dt[1],
          proposed_row$end_dt[1],
          proposed_row[[protection_col]][1],
          proposed_row[[id_col]][1]
        )
      }
      DBI::dbExecute(con, update_sql, params = update_params)
    } else {
      if (is.null(protection_col)) {
        insert_sql <- sprintf(
          "INSERT INTO %s (timeseries_id, %s, start_dt, end_dt) VALUES ($1, $2, $3, $4);",
          table_name,
          db_value_col
        )
        insert_params <- list(
          proposed_row$timeseries_id[1],
          proposed_row[[value_col]][1],
          proposed_row$start_dt[1],
          proposed_row$end_dt[1]
        )
      } else {
        insert_sql <- sprintf(
          "INSERT INTO %s (timeseries_id, %s, start_dt, end_dt, %s) VALUES ($1, $2, $3, $4, $5);",
          table_name,
          db_value_col,
          protection_col
        )
        insert_params <- list(
          proposed_row$timeseries_id[1],
          proposed_row[[value_col]][1],
          proposed_row$start_dt[1],
          proposed_row$end_dt[1],
          proposed_row[[protection_col]][1]
        )
      }
      DBI::dbExecute(con, insert_sql, params = insert_params)
    }
  }

  invisible(TRUE)
}

#' Adjust the grade of a timeseries in the database
#'
#' @param con A connection to the database with write privileges to the 'grades' and 'measurements_continuous' tables.
#' @param timeseries_id The target timeseries_id
#' @param data A data.frame with columns for 'datetime' and 'grade'. 'datetime' should be POSIXct and 'grade' should either character (in which case it must refer to entries in column 'grade_type_code' of table 'grade_types' or integer/numeric, in which case it must refer to column 'grade_type_id' of the same table.
#' @param delete Logical. If TRUE, the function will delete grades which come entirely after the start of 'data'. This ensures synchronization with remote data stores and is called as TRUE from the 'synchronize' functions.
#' @param source_update Logical. Set TRUE when `data` came from a source
#' adapter or synchronization workflow. Existing intervals marked
#' `no_source_update` are then immutable. Direct/manual calls leave this FALSE
#' and may set `data$no_source_update` explicitly.
#'
#' @return Modifies the 'grades' table in the database.
#' @export
#'

adjust_grade <- function(
  con,
  timeseries_id,
  data,
  delete = FALSE,
  source_update = FALSE
) {
  active <- dbTransBegin(con) # returns TRUE if a transaction is not already in progress and was set up, otherwise commit will happen in the original calling function.

  tryCatch(
    {
      # If a column 'date' and no column 'datetime' is present, rename 'date' to 'datetime' and convert to POSIXct
      if ("date" %in% names(data) & !"datetime" %in% names(data)) {
        data$datetime <- as.POSIXct(data$date, tz = "UTC")
        data <- data[, !names(data) == "date"]
      }
      # Ensure that 'datetime' is POSIXct
      if (!inherits(data$datetime[1], "POSIXct")) {
        stop("Column 'datetime' must be of class POSIXct.")
      }
      if (!("no_source_update" %in% names(data))) {
        data$no_source_update <- FALSE
      }
      data$no_source_update[is.na(data$no_source_update)] <- FALSE
      if (source_update) {
        data$no_source_update <- FALSE
      }

      grade_table <- DBI::dbGetQuery(
        con,
        "SELECT grade_type_id, grade_type_code FROM public.grade_types;"
      )

      unspecified_grade <- grade_table[
        grade_table$grade_type_code == "UNS",
        "grade_type_id"
      ]
      unknown_grade <- grade_table[
        grade_table$grade_type_code == "UNK",
        "grade_type_id"
      ]
      data$grade[is.na(data$grade)] <- unspecified_grade

      # Check if 'grade' is character, if so match those characters to 'grade_type_code' in the 'grades' table
      if (inherits(data$grade[1], "character")) {
        data$grade <- grade_table$grade_type_id[match(
          data$grade,
          grade_table$grade_type_code
        )]
      }

      data$grade <- as.integer(data$grade)

      # Ensure that all grades left in the table match to a grade_type_id in the database, if not assign them to 'UNK' for unknown
      data[
        !data$grade %in% grade_table$grade_type_id,
        "grade"
      ] <- unknown_grade

      # Format the datetime to UTC. 'fmt' is a utility function in file utils.R
      min_datetime <- fmt(min(data$datetime))
      max_datetime <- fmt(max(data$datetime))
      sync_delete_ids <- integer()

      if (delete) {
        sync_delete_ids <- get_sync_delete_ids(
          con,
          "continuous.grades",
          "grade_id",
          timeseries_id,
          min(data$datetime),
          protect_source_updates = source_update
        )
      }

      # Get the data where at least one of the following is true:
      # has an end datetime within the range of the data
      # has a start datetime within the range of the data
      # has a start datetime before the range of the data and an end datetime after the range of the data
      # This leaves out entries that are entirely before or after the range of the data.
      exist <- DBI::dbGetQuery(
        con,
        sprintf(
          "WITH matched AS (
          SELECT grade_id, timeseries_id, grade_type_id, start_dt, end_dt,
                 no_source_update
            FROM continuous.grades
          WHERE timeseries_id = %s
            AND (
              (end_dt   BETWEEN '%s' AND '%s')
            OR (start_dt BETWEEN '%s' AND '%s')
            OR (start_dt <= '%s' AND end_dt >= '%s')
            )
          ), fallback AS (
              SELECT grade_id, timeseries_id, grade_type_id, start_dt, end_dt,
                     no_source_update
                FROM continuous.grades
              WHERE timeseries_id = %s
              ORDER BY end_dt DESC
              LIMIT 1
          )
          SELECT * FROM matched
          UNION ALL
          SELECT * FROM fallback
          WHERE NOT EXISTS (SELECT 1 FROM matched)
          ORDER BY start_dt ASC;",
          timeseries_id,
          min_datetime,
          max_datetime,
          min_datetime,
          max_datetime,
          min_datetime,
          max_datetime,
          timeseries_id
        )
      )
      existing_state <- exist

      if (nrow(exist) == 0) {
        exist <- data.frame(
          grade_id = NA,
          timeseries_id = timeseries_id,
          grade_type_id = data$grade[1],
          start_dt = data$datetime[1],
          end_dt = data$datetime[1],
          no_source_update = FALSE
        )
      }
      new_segments <- build_attribute_segments(
        data,
        value_col = "grade",
        id_col = "grade_id",
        timeseries_id = timeseries_id
      )
      names(new_segments)[names(new_segments) == "grade"] <- "grade_type_id"
      if (source_update) {
        new_segments <- clip_segments_around_protected(
          new_segments,
          exist[exist$no_source_update, , drop = FALSE]
        )
      }

      exist <- collapse_segments_with_split(
        exist = exist,
        new_segments = new_segments,
        value_col = "grade_type_id",
        id_col = "grade_id",
        timeseries_id = timeseries_id,
        bridge_latest_extension = TRUE,
        protection_col = "no_source_update"
      )

      # Now commit the changes to the database
      commit_fx <- function(con, exist, existing_state, sync_delete_ids) {
        reconcile_segment_changes(
          con = con,
          table_name = "continuous.grades",
          id_col = "grade_id",
          value_col = "grade_type_id",
          db_value_col = "grade_type_id",
          existing_state = existing_state,
          proposed_state = exist,
          delete_ids = sync_delete_ids,
          protection_col = "no_source_update",
          source_update = source_update
        )
      }

      commit_fx(con, exist, existing_state, sync_delete_ids)

      if (active) {
        DBI::dbExecute(con, "COMMIT;")
      }
    },
    error = function(e) {
      if (active) {
        DBI::dbExecute(con, "ROLLBACK;")
      }
      warning(
        "adjust_grade: Failed to commit changes to the database with error ",
        e$message
      )
    }
  )
} # End of adjust_grade function


#' Adjust the qualifier of a timeseries in the database
#'
#' @param con A connection to the database with write privileges to the 'qualifiers' and 'measurements_continuous' tables.
#' @param timeseries_id The target timeseries_id
#' @param data A data.frame with columns for 'datetime' and 'qualifier'. 'datetime' should be POSIXct and 'qualifier' should either character (in which case it must refer to entries in column 'qualifier_type_code' of table 'qualifiers' or integer/numeric, in which case it must refer to column 'qualifier_type_id' of the same table.
#' @param delete Logical. If TRUE, the function will delete qualifiers which come entirely after the start of 'data'. This ensures synchronization with remote data stores and is called as TRUE from the 'synchronize' functions.
#' @param source_update Logical. Set TRUE when `data` came from a source
#' adapter or synchronization workflow. Existing intervals marked
#' `no_source_update` are then immutable. Direct/manual calls leave this FALSE
#' and may set `data$no_source_update` explicitly.
#'
#' @return Modifies the 'qualifiers' table in the database.
#' @export

adjust_qualifier <- function(
  con,
  timeseries_id,
  data,
  delete = FALSE,
  source_update = FALSE
) {
  active <- dbTransBegin(con) # returns TRUE if a transaction is not already in progress and was set up, otherwise commit will happen in the original calling function.

  tryCatch(
    {
      # If a column 'date' and no column 'datetime' is present, rename 'date' to 'datetime' and convert to POSIXct
      if ("date" %in% names(data) & !"datetime" %in% names(data)) {
        data$datetime <- as.POSIXct(data$date, tz = "UTC")
        data <- data[, !names(data) == "date"]
      }
      # Ensure that 'datetime' is POSIXct
      if (!inherits(data$datetime[1], "POSIXct")) {
        stop("Column 'datetime' must be of class POSIXct.")
      }
      if (!("no_source_update" %in% names(data))) {
        data$no_source_update <- FALSE
      }
      data$no_source_update[is.na(data$no_source_update)] <- FALSE
      if (source_update) {
        data$no_source_update <- FALSE
      }

      qualifier_table <- DBI::dbGetQuery(
        con,
        "SELECT qualifier_type_id, qualifier_type_code FROM public.qualifier_types;"
      )

      unspecified_qualifier <- qualifier_table[
        qualifier_table$qualifier_type_code == "UNS",
        "qualifier_type_id"
      ]
      unknown_qualifier <- qualifier_table[
        qualifier_table$qualifier_type_code == "UNK",
        "qualifier_type_id"
      ]

      data$qualifier[is.na(data$qualifier)] <- unspecified_qualifier

      # Split the 'qualifier' column into separate rows if it contains multiple values separated by commas

      data$qualifier <- as.character(data$qualifier)

      data <- data %>%
        dplyr::mutate(
          qualifier = strsplit(.data$qualifier, "\\s*,\\s*"),
          rank = lapply(.data$qualifier, seq_along)
        )
      data <- data.frame(
        datetime = rep(data$datetime, lengths(data$qualifier)),
        qualifier = unlist(data$qualifier),
        rank = unlist(data$rank),
        no_source_update = rep(
          data$no_source_update,
          lengths(data$qualifier)
        ),
        stringsAsFactors = FALSE
      )

      # Check if 'qualifier' column is now composed of numbers or strings
      if (!grepl("^[0-9]", data$qualifier[1])) {
        # If it's a string, match it to the database numeric
        data$qualifier <- qualifier_table$qualifier_type_id[match(
          data$qualifier,
          qualifier_table$qualifier_type_code
        )]
      }

      data$qualifier <- as.integer(data$qualifier)

      data[
        !data$qualifier %in% qualifier_table$qualifier_type_id,
        "qualifier"
      ] <- unknown_qualifier

      # Break 'data' into a data.frame for each unique 'rank'
      datalist <- split(data, data$rank)
      sync_delete_ids <- integer()

      if (delete) {
        sync_delete_ids <- get_sync_delete_ids(
          con,
          "continuous.qualifiers",
          "qualifier_id",
          timeseries_id,
          min(data$datetime),
          protect_source_updates = source_update
        )
      }

      existing_state_all <- data.frame(
        qualifier_id = integer(),
        timeseries_id = integer(),
        qualifier_type_id = integer(),
        start_dt = as.POSIXct(character(), tz = "UTC"),
        end_dt = as.POSIXct(character(), tz = "UTC"),
        no_source_update = logical()
      )
      proposed_state_all <- existing_state_all

      # Work on each table in the list
      for (tbl in names(datalist)) {
        data <- datalist[[tbl]]

        # Format the datetime to UTC. 'fmt' is a utility function in file utils.R
        min_datetime <- fmt(min(data$datetime))
        max_datetime <- fmt(max(data$datetime))

        # Get the data where at least one of the following is true:
        # has an end datetime within the range of the data
        # has a start datetime within the range of the data
        # has a start datetime before the range of the data and an end datetime after the range of the data
        # This leaves out entries that are entirely before or after the range of the data.
        exist <- DBI::dbGetQuery(
          con,
          sprintf(
            "WITH matched AS (
    SELECT qualifier_id, timeseries_id, qualifier_type_id, start_dt, end_dt,
           no_source_update
      FROM continuous.qualifiers
     WHERE timeseries_id = %s
       AND (
         (end_dt   BETWEEN '%s' AND '%s')
      OR (start_dt BETWEEN '%s' AND '%s')
      OR (start_dt <= '%s' AND end_dt >= '%s')
       )
       AND qualifier_type_id = %s
    ), fallback AS (
        SELECT qualifier_id, timeseries_id, qualifier_type_id, start_dt, end_dt,
               no_source_update
          FROM continuous.qualifiers
         WHERE timeseries_id = %s
           AND qualifier_type_id = %s
         ORDER BY end_dt DESC
         LIMIT 1
    )
    SELECT * FROM matched
    UNION ALL
    SELECT * FROM fallback
     WHERE NOT EXISTS (SELECT 1 FROM matched)
    ORDER BY start_dt ASC;",
            timeseries_id,
            min_datetime,
            max_datetime,
            min_datetime,
            max_datetime,
            min_datetime,
            max_datetime,
            data$qualifier[1],
            timeseries_id,
            data$qualifier[1]
          )
        )
        existing_state_all <- rbind(existing_state_all, exist)

        if (nrow(exist) == 0) {
          exist <- data.frame(
            qualifier_id = NA,
            timeseries_id = timeseries_id,
            qualifier_type_id = data$qualifier[1],
            start_dt = data$datetime[1],
            end_dt = data$datetime[1],
            no_source_update = FALSE
          )
        }
        new_segments <- build_attribute_segments(
          data,
          value_col = "qualifier",
          id_col = "qualifier_id",
          timeseries_id = timeseries_id
        )
        names(new_segments)[
          names(new_segments) == "qualifier"
        ] <- "qualifier_type_id"
        if (source_update) {
          new_segments <- clip_segments_around_protected(
            new_segments,
            exist[exist$no_source_update, , drop = FALSE]
          )
        }

        exist <- collapse_segments_with_split(
          exist = exist,
          new_segments = new_segments,
          value_col = "qualifier_type_id",
          id_col = "qualifier_id",
          timeseries_id = timeseries_id,
          bridge_latest_extension = TRUE,
          protection_col = "no_source_update"
        )

        proposed_state_all <- rbind(proposed_state_all, exist)
      } # End of for loop iterating on tables

      if (nrow(existing_state_all) > 0) {
        existing_state_all <- existing_state_all[
          !duplicated(existing_state_all$qualifier_id),
          ,
          drop = FALSE
        ]
      }

      proposed_types <- unique(proposed_state_all$qualifier_type_id[
        proposed_state_all$timeseries_id != -1
      ])
      missing_existing_types <- setdiff(
        proposed_types,
        unique(existing_state_all$qualifier_type_id)
      )

      if (length(missing_existing_types) > 0) {
        missing_existing <- DBI::dbGetQuery(
          con,
          paste0(
            "SELECT qualifier_id, timeseries_id, qualifier_type_id,
                    start_dt, end_dt, no_source_update
               FROM continuous.qualifiers
              WHERE timeseries_id = $1
                AND qualifier_type_id IN (",
            paste(as.integer(missing_existing_types), collapse = ", "),
            ")
              ORDER BY qualifier_type_id, start_dt, end_dt;"
          ),
          params = list(timeseries_id)
        )

        if (nrow(missing_existing) > 0) {
          overlaps_proposal <- vapply(
            seq_len(nrow(missing_existing)),
            function(i) {
              any(
                proposed_state_all$timeseries_id != -1 &
                  proposed_state_all$qualifier_type_id ==
                    missing_existing$qualifier_type_id[i] &
                  proposed_state_all$start_dt <= missing_existing$end_dt[i] &
                  proposed_state_all$end_dt >= missing_existing$start_dt[i]
              )
            },
            logical(1)
          )
          missing_existing <- missing_existing[
            overlaps_proposal,
            ,
            drop = FALSE
          ]

          existing_state_all <- rbind(
            existing_state_all,
            missing_existing
          )
          proposed_state_all <- rbind(
            proposed_state_all,
            missing_existing
          )
        }
      }

      merged_qualifiers <- merge_overlapping_same_value_segments(
        segments = proposed_state_all,
        value_col = "qualifier_type_id",
        id_col = "qualifier_id",
        protection_col = "no_source_update"
      )
      proposed_state_all <- merged_qualifiers$segments
      sync_delete_ids <- unique(c(
        sync_delete_ids,
        merged_qualifiers$delete_ids
      ))

      # Prefer the ID of an exact existing interval. The rank-based rebuild can
      # otherwise attach a later interval's ID to an earlier interval, causing
      # needless delete/insert churn on every synchronization.
      if (
        nrow(existing_state_all) > 0 &&
          nrow(proposed_state_all) > 0
      ) {
        proposed_ids <- proposed_state_all$qualifier_id
        aligned_ids <- rep(NA_integer_, nrow(proposed_state_all))
        used_ids <- integer()

        for (i in seq_len(nrow(proposed_state_all))) {
          exact_match <- which(
            existing_state_all$qualifier_type_id ==
              proposed_state_all$qualifier_type_id[i] &
              existing_state_all$start_dt == proposed_state_all$start_dt[i] &
              existing_state_all$end_dt == proposed_state_all$end_dt[i] &
              existing_state_all$no_source_update ==
                proposed_state_all$no_source_update[i] &
              !(existing_state_all$qualifier_id %in% used_ids)
          )
          if (length(exact_match) > 0) {
            aligned_ids[i] <- existing_state_all$qualifier_id[exact_match[1]]
            used_ids <- c(used_ids, aligned_ids[i])
          }
        }

        for (i in which(is.na(aligned_ids))) {
          candidate_id <- proposed_ids[i]
          if (
            !is.na(candidate_id) &&
              !(candidate_id %in% used_ids) &&
              any(
                existing_state_all$qualifier_id == candidate_id &
                  existing_state_all$qualifier_type_id ==
                    proposed_state_all$qualifier_type_id[i] &
                  existing_state_all$no_source_update ==
                    proposed_state_all$no_source_update[i]
              )
          ) {
            aligned_ids[i] <- candidate_id
            used_ids <- c(used_ids, candidate_id)
          }
        }

        proposed_state_all$qualifier_id <- aligned_ids
      }

      reconcile_segment_changes(
        con = con,
        table_name = "continuous.qualifiers",
        id_col = "qualifier_id",
        value_col = "qualifier_type_id",
        db_value_col = "qualifier_type_id",
        existing_state = existing_state_all,
        proposed_state = proposed_state_all,
        delete_ids = sync_delete_ids,
        protection_col = "no_source_update",
        source_update = source_update
      )

      if (active) {
        DBI::dbExecute(con, "COMMIT;")
      }
    },
    error = function(e) {
      if (active) {
        DBI::dbExecute(con, "ROLLBACK;")
      }
      warning(
        "adjust_qualifier: Failed to commit changes to the database with error ",
        e$message
      )
    }
  )
} # End of adjust_qualifier function


#' Adjust the approval of a timeseries in the database
#'
#' @param con A connection to the database with write privileges to the 'approvals' and 'measurements_continuous' tables.
#' @param timeseries_id The target timeseries_id
#' @param data A data.frame with columns for 'datetime' and 'approval'. 'datetime' should be POSIXct and 'approval' should either character (in which case it must refer to entries in column 'approval_type_code' of table 'approval_types' or integer/numeric, in which case it must refer to column 'approval_type_id' of the same table.
#' @param delete Logical. If TRUE, the function will delete approvals which come entirely after the start of 'data'. This ensures synchronization with remote data stores and is called as TRUE from the 'synchronize' functions.
#' @param source_update Logical. Set TRUE when `data` came from a source
#' adapter or synchronization workflow. Existing intervals marked
#' `no_source_update` are then immutable. Direct/manual calls leave this FALSE
#' and may set `data$no_source_update` explicitly.
#'
#' @return Modifies the 'approvals' table in the database.
#' @export

adjust_approval <- function(
  con,
  timeseries_id,
  data,
  delete = FALSE,
  source_update = FALSE
) {
  active <- dbTransBegin(con) # returns TRUE if a transaction is not already in progress and was set up, otherwise commit will happen in the original calling function.

  tryCatch(
    {
      # If a column 'date' and no column 'datetime' is present, rename 'date' to 'datetime' and convert to POSIXct
      if ("date" %in% names(data) & !"datetime" %in% names(data)) {
        data$datetime <- as.POSIXct(data$date, tz = "UTC")
        data <- data[, !names(data) == "date"]
      }
      # Ensure that 'datetime' is POSIXct
      if (!inherits(data$datetime[1], "POSIXct")) {
        stop("Column 'datetime' must be of class POSIXct.")
      }
      if (!("no_source_update" %in% names(data))) {
        data$no_source_update <- FALSE
      }
      data$no_source_update[is.na(data$no_source_update)] <- FALSE
      if (source_update) {
        data$no_source_update <- FALSE
      }

      approval_table <- DBI::dbGetQuery(
        con,
        "SELECT approval_type_id, approval_type_code FROM public.approval_types;"
      )

      unspecified_approval <- approval_table[
        approval_table$approval_type_code == "UNS",
        "approval_type_id"
      ]
      unknown_approval <- approval_table[
        approval_table$approval_type_code == "UNK",
        "approval_type_id"
      ]
      data$approval[is.na(data$approval)] <- unspecified_approval

      # Check if 'approval' is character, if so match those characters to 'approval_type_code' in the 'approvals' table
      if (inherits(data$approval[1], "character")) {
        data$approval <- approval_table$approval_type_id[match(
          data$approval,
          approval_table$approval_type_code
        )]
      }

      data$approval <- as.integer(data$approval)

      # Ensure that all approvals left in the table match to an approval_type_id in the database, if not assign them to 'UNK' for unknown
      data[
        !data$approval %in% approval_table$approval_type_id,
        "approval"
      ] <- unknown_approval

      # Format the datetime to UTC. 'fmt' is a utility function in file utils.R
      min_datetime <- fmt(min(data$datetime))
      max_datetime <- fmt(max(data$datetime))
      sync_delete_ids <- integer()

      if (delete) {
        sync_delete_ids <- get_sync_delete_ids(
          con,
          "continuous.approvals",
          "approval_id",
          timeseries_id,
          min(data$datetime),
          protect_source_updates = source_update
        )
      }

      # Get the data where at least one of the following is true:
      # has an end datetime within the range of the data
      # has a start datetime within the range of the data
      # has a start datetime before the range of the data and an end datetime after the range of the data
      # This leaves out entries that are entirely before or after the range of the data.
      exist <- DBI::dbGetQuery(
        con,
        sprintf(
          "WITH matched AS (
    SELECT approval_id, timeseries_id, approval_type_id, start_dt, end_dt,
           no_source_update
      FROM continuous.approvals
     WHERE timeseries_id = %s
       AND (
         (end_dt   BETWEEN '%s' AND '%s')
      OR (start_dt BETWEEN '%s' AND '%s')
      OR (start_dt <= '%s' AND end_dt >= '%s')
       )
    ), fallback AS (
        SELECT approval_id, timeseries_id, approval_type_id, start_dt, end_dt,
               no_source_update
          FROM continuous.approvals
         WHERE timeseries_id = %s
         ORDER BY end_dt DESC
         LIMIT 1
    )
    SELECT * FROM matched
    UNION ALL
    SELECT * FROM fallback
     WHERE NOT EXISTS (SELECT 1 FROM matched)
    ORDER BY start_dt ASC;",
          timeseries_id,
          min_datetime,
          max_datetime,
          min_datetime,
          max_datetime,
          min_datetime,
          max_datetime,
          timeseries_id
        )
      )
      existing_state <- exist

      if (nrow(exist) == 0) {
        exist <- data.frame(
          approval_id = NA,
          timeseries_id = timeseries_id,
          approval_type_id = data$approval[1],
          start_dt = data$datetime[1],
          end_dt = data$datetime[1],
          no_source_update = FALSE
        )
      }

      new_segments <- build_attribute_segments(
        data,
        value_col = "approval",
        id_col = "approval_id",
        timeseries_id = timeseries_id
      )
      names(new_segments)[
        names(new_segments) == "approval"
      ] <- "approval_type_id"
      if (source_update) {
        new_segments <- clip_segments_around_protected(
          new_segments,
          exist[exist$no_source_update, , drop = FALSE]
        )
      }

      exist <- collapse_segments_with_split(
        exist = exist,
        new_segments = new_segments,
        value_col = "approval_type_id",
        id_col = "approval_id",
        timeseries_id = timeseries_id,
        bridge_latest_extension = TRUE,
        protection_col = "no_source_update"
      )

      # Now commit the changes to the database
      commit_fx <- function(con, exist, existing_state, sync_delete_ids) {
        reconcile_segment_changes(
          con = con,
          table_name = "continuous.approvals",
          id_col = "approval_id",
          value_col = "approval_type_id",
          db_value_col = "approval_type_id",
          existing_state = existing_state,
          proposed_state = exist,
          delete_ids = sync_delete_ids,
          protection_col = "no_source_update",
          source_update = source_update
        )
      }

      commit_fx(con, exist, existing_state, sync_delete_ids)

      if (active) {
        DBI::dbExecute(con, "COMMIT;")
      }
    },
    error = function(e) {
      if (active) {
        DBI::dbExecute(con, "ROLLBACK;")
      }
      warning(
        "adjust_approval: Failed to commit changes to the database with error ",
        e$message
      )
    }
  )
} # End of adjust_approval function


#' Adjust the owner of a timeseries in the database
#'
#' @param con A connection to the database with write privileges to the 'owners' and 'measurements_continuous' tables.
#' @param timeseries_id The target timeseries_id
#' @param data A data.frame with columns for 'datetime' and 'owner'. 'datetime' should be POSIXct and 'owner' should be either character (in which case it must refer to entries in column 'name' of table 'organizations' or integer/numeric, in which case it must refer to column 'organization_id' of the same table.
#' @param delete Logical. If TRUE, the function will delete owners which come entirely after the start of 'data'. This ensures synchronization with remote data stores and is called as TRUE from the 'synchronize' functions.
#'
#' @return Modifies the 'owners' table in the database.
#' @export

adjust_owner <- function(con, timeseries_id, data, delete = FALSE) {
  # Make sure that column 'owner' is not all NA
  if (all(is.na(data$owner))) {
    message(
      "adjust_owner: column 'owner' was all NA, skipped. Applies to timeseries_id ",
      timeseries_id,
      "."
    )
  }

  active <- dbTransBegin(con) # returns TRUE if a transaction is not already in progress and was set up, otherwise commit will happen in the original calling function.

  tryCatch(
    {
      # If a column 'date' and no column 'datetime' is present, rename 'date' to 'datetime' and convert to POSIXct
      if ("date" %in% names(data) & !"datetime" %in% names(data)) {
        data$datetime <- as.POSIXct(data$date, tz = "UTC")
        data <- data[, !names(data) == "date"]
      }
      # Ensure that 'datetime' is POSIXct
      if (!inherits(data$datetime[1], "POSIXct")) {
        stop("Column 'datetime' must be of class POSIXct.")
      }

      # Check if 'owner' is character, if so match those characters to 'name' in the 'organizations' table
      if (inherits(data$owner[1], "character")) {
        owner_table <- DBI::dbGetQuery(
          con,
          "SELECT organization_id, name FROM public.organizations;"
        )
        data$owner <- owner_table$organization_id[match(
          data$owner,
          owner_table$name
        )]
      }

      # Format the datetime to UTC. 'fmt' is a utility function in file utils.R
      min_datetime <- fmt(min(data$datetime))
      max_datetime <- fmt(max(data$datetime))
      sync_delete_ids <- integer()

      if (delete) {
        sync_delete_ids <- get_sync_delete_ids(
          con,
          "continuous.owners",
          "owner_id",
          timeseries_id,
          min(data$datetime)
        )
      }

      # Get the data where at least one of the following is true:
      # has an end datetime within the range of the data
      # has a start datetime within the range of the data
      # has a start datetime before the range of the data and an end datetime after the range of the data
      # This leaves out entries that are entirely before or after the range of the data.
      exist <- DBI::dbGetQuery(
        con,
        sprintf(
          "WITH matched AS (
    SELECT owner_id, timeseries_id, organization_id, start_dt, end_dt
      FROM continuous.owners
     WHERE timeseries_id = %s
       AND (
         (end_dt   BETWEEN '%s' AND '%s')
      OR (start_dt BETWEEN '%s' AND '%s')
      OR (start_dt <= '%s' AND end_dt >= '%s')
       )
    ), fallback AS (
        SELECT owner_id, timeseries_id, organization_id, start_dt, end_dt
          FROM continuous.owners
         WHERE timeseries_id = %s
         ORDER BY end_dt DESC
         LIMIT 1
    )
    SELECT * FROM matched
    UNION ALL
    SELECT * FROM fallback
     WHERE NOT EXISTS (SELECT 1 FROM matched)
    ORDER BY start_dt ASC;",
          timeseries_id,
          min_datetime,
          max_datetime,
          min_datetime,
          max_datetime,
          min_datetime,
          max_datetime,
          timeseries_id
        )
      )
      existing_state <- exist

      if (nrow(exist) == 0) {
        exist <- data.frame(
          owner_id = NA,
          timeseries_id = timeseries_id,
          organization_id = data$owner[1],
          start_dt = data$datetime[1],
          end_dt = data$datetime[1]
        )
      }
      # Collapse consecutive rows with the same owner using run-length encoding
      data <- data[order(data$datetime), ]
      runs <- rle(data$owner)
      ends <- cumsum(runs$lengths)
      starts <- c(1, utils::head(ends, -1) + 1)
      new_segments <- data.frame(
        owner_id = NA,
        timeseries_id = timeseries_id,
        organization_id = runs$values,
        start_dt = data$datetime[starts],
        end_dt = data$datetime[ends],
        stringsAsFactors = FALSE
      )

      exist <- collapse_segments_with_split(
        exist = exist,
        new_segments = new_segments,
        value_col = "organization_id",
        id_col = "owner_id",
        timeseries_id = timeseries_id,
        bridge_latest_extension = TRUE
      )

      # Now commit the changes to the database
      commit_fx <- function(con, exist, existing_state, sync_delete_ids) {
        reconcile_segment_changes(
          con = con,
          table_name = "continuous.owners",
          id_col = "owner_id",
          value_col = "organization_id",
          db_value_col = "organization_id",
          existing_state = existing_state,
          proposed_state = exist,
          delete_ids = sync_delete_ids
        )
      }

      commit_fx(con, exist, existing_state, sync_delete_ids)

      if (active) {
        DBI::dbExecute(con, "COMMIT;")
      }
    },
    error = function(e) {
      if (active) {
        DBI::dbExecute(con, "ROLLBACK;")
      }
      warning(
        "adjust_owner: Failed to commit changes to the database with error ",
        e$message
      )
    }
  )
} # End of adjust_owner function


#' Adjust the contributor of a timeseries in the database
#'
#' @param con A connection to the database with write privileges to the 'contributors' and 'measurements_continuous' tables.
#' @param timeseries_id The target timeseries_id
#' @param data A data.frame with columns for 'datetime' and 'contributor'. 'datetime' should be POSIXct and 'contributor' should be either character (in which case it must refer to entries in column 'name' of table 'organizations' or integer/numeric, in which case it must refer to column 'organization_id' of the same table.
#' @param delete Logical. If TRUE, the function will delete contributors which come entirely after the start of 'data'. This ensures synchronization with remote data stores and is called as TRUE from the 'synchronize' functions.
#'
#' @return Modifies the 'contributors' table in the database.
#' @export

adjust_contributor <- function(con, timeseries_id, data, delete = FALSE) {
  # Make sure that column 'contributor' is not all NA
  if (all(is.na(data$contributor))) {
    message(
      "adjust_contributor: column 'contributor' was all NA, skipped. Applies to timeseries_id ",
      timeseries_id,
      "."
    )
  }

  active <- dbTransBegin(con) # returns TRUE if a transaction is not already in progress and was set up, otherwise commit will happen in the original calling function.

  tryCatch(
    {
      # If a column 'date' and no column 'datetime' is present, rename 'date' to 'datetime' and convert to POSIXct
      if ("date" %in% names(data) & !"datetime" %in% names(data)) {
        data$datetime <- as.POSIXct(data$date, tz = "UTC")
        data <- data[, !names(data) == "date"]
      }
      # Ensure that 'datetime' is POSIXct
      if (!inherits(data$datetime[1], "POSIXct")) {
        stop("Column 'datetime' must be of class POSIXct.")
      }

      # Check if 'contributor' is character, if so match those characters to 'name' in the 'organizations' table
      if (inherits(data$contributor[1], "character")) {
        contributor_table <- DBI::dbGetQuery(
          con,
          "SELECT organization_id, name FROM public.organizations;"
        )
        data$contributor <- contributor_table$organization_id[match(
          data$contributor,
          contributor_table$name
        )]
      }

      # Format the datetime to UTC. 'fmt' is a utility function in file utils.R
      min_datetime <- fmt(min(data$datetime))
      max_datetime <- fmt(max(data$datetime))
      sync_delete_ids <- integer()

      if (delete) {
        sync_delete_ids <- get_sync_delete_ids(
          con,
          "continuous.contributors",
          "contributor_id",
          timeseries_id,
          min(data$datetime)
        )
      }

      # Get the data where at least one of the following is true:
      # has an end datetime within the range of the data
      # has a start datetime within the range of the data
      # has a start datetime before the range of the data and an end datetime after the range of the data
      # This leaves out entries that are entirely before or after the range of the data.
      exist <- DBI::dbGetQuery(
        con,
        sprintf(
          "WITH matched AS (
    SELECT contributor_id, timeseries_id, organization_id, start_dt, end_dt
      FROM continuous.contributors
     WHERE timeseries_id = %s
       AND (
         (end_dt   BETWEEN '%s' AND '%s')
      OR (start_dt BETWEEN '%s' AND '%s')
      OR (start_dt <= '%s' AND end_dt >= '%s')
       )
    ), fallback AS (
        SELECT contributor_id, timeseries_id, organization_id, start_dt, end_dt
          FROM continuous.contributors
         WHERE timeseries_id = %s
         ORDER BY end_dt DESC
         LIMIT 1
    )
    SELECT * FROM matched
    UNION ALL
    SELECT * FROM fallback
     WHERE NOT EXISTS (SELECT 1 FROM matched)
    ORDER BY start_dt ASC;",
          timeseries_id,
          min_datetime,
          max_datetime,
          min_datetime,
          max_datetime,
          min_datetime,
          max_datetime,
          timeseries_id
        )
      )
      existing_state <- exist

      if (nrow(exist) == 0) {
        exist <- data.frame(
          contributor_id = NA,
          timeseries_id = timeseries_id,
          organization_id = data$contributor[1],
          start_dt = data$datetime[1],
          end_dt = data$datetime[1]
        )
      }
      # Collapse consecutive rows with the same contributor using run-length encoding
      data <- data[order(data$datetime), ]
      runs <- rle(data$contributor)
      ends <- cumsum(runs$lengths)
      starts <- c(1, utils::head(ends, -1) + 1)
      new_segments <- data.frame(
        contributor_id = NA,
        timeseries_id = timeseries_id,
        organization_id = runs$values,
        start_dt = data$datetime[starts],
        end_dt = data$datetime[ends],
        stringsAsFactors = FALSE
      )

      exist <- collapse_segments_with_split(
        exist = exist,
        new_segments = new_segments,
        value_col = "organization_id",
        id_col = "contributor_id",
        timeseries_id = timeseries_id,
        bridge_latest_extension = TRUE
      )

      # Now commit the changes to the database
      commit_fx <- function(con, exist, existing_state, sync_delete_ids) {
        reconcile_segment_changes(
          con = con,
          table_name = "continuous.contributors",
          id_col = "contributor_id",
          value_col = "organization_id",
          db_value_col = "organization_id",
          existing_state = existing_state,
          proposed_state = exist,
          delete_ids = sync_delete_ids
        )
      }

      commit_fx(con, exist, existing_state, sync_delete_ids)

      if (active) {
        DBI::dbExecute(con, "COMMIT;")
      }
    },
    error = function(e) {
      if (active) {
        DBI::dbExecute(con, "ROLLBACK;")
      }
      warning(
        "adjust_contributor: Failed to commit changes to the database with error ",
        e$message
      )
    }
  )
} # End of adjust_contributor function


#' Adjust the data sharing agreement of a timeseries in the database
#'
#' @param con A connection to the database with write privileges to the 'timeseries_data_sharing_agreements' table.
#' @param timeseries_id The target timeseries_id
#' @param data A data.frame with columns for 'datetime' and 'data_sharing_agreement_id'. 'datetime' should be POSIXct and 'data_sharing_agreement_id' should refer to column 'document_id' of table 'files.documents'.
#' @param delete Logical. If TRUE, the function will delete data sharing agreements which come entirely after the start of 'data'. This ensures synchronization with remote data stores and is called as TRUE from the 'synchronize' functions.
#'
#' @return Modifies the 'timeseries_data_sharing_agreements' table in the database.
#' @export

adjust_data_sharing_agreement <- function(
  con,
  timeseries_id,
  data,
  delete = FALSE
) {
  if (
    "data_sharing_agreement" %in%
      names(data) &&
      !("data_sharing_agreement_id" %in% names(data))
  ) {
    data$data_sharing_agreement_id <- data$data_sharing_agreement
    data$data_sharing_agreement <- NULL
  }

  if (all(is.na(data$data_sharing_agreement_id))) {
    message(
      "adjust_data_sharing_agreement: column 'data_sharing_agreement_id' was all NA, skipped. Applies to timeseries_id ",
      timeseries_id,
      "."
    )
    return(invisible(NULL))
  }

  active <- dbTransBegin(con) # returns TRUE if a transaction is not already in progress and was set up, otherwise commit will happen in the original calling function.

  tryCatch(
    {
      # If a column 'date' and no column 'datetime' is present, rename 'date' to 'datetime' and convert to POSIXct
      if ("date" %in% names(data) & !"datetime" %in% names(data)) {
        data$datetime <- as.POSIXct(data$date, tz = "UTC")
        data <- data[, !names(data) == "date"]
      }
      # Ensure that 'datetime' is POSIXct
      if (!inherits(data$datetime[1], "POSIXct")) {
        stop("Column 'datetime' must be of class POSIXct.")
      }

      if (inherits(data$data_sharing_agreement_id[1], "character")) {
        data$data_sharing_agreement_id <- as.integer(
          data$data_sharing_agreement_id
        )
      }

      # Format the datetime to UTC. 'fmt' is a utility function in file utils.R
      min_datetime <- fmt(min(data$datetime))
      max_datetime <- fmt(max(data$datetime))
      sync_delete_ids <- integer()

      if (delete) {
        sync_delete_ids <- get_sync_delete_ids(
          con,
          "continuous.timeseries_data_sharing_agreements",
          "timeseries_data_sharing_agreement_id",
          timeseries_id,
          min(data$datetime)
        )
      }

      # Get the data where at least one of the following is true:
      # has an end datetime within the range of the data
      # has a start datetime within the range of the data
      # has a start datetime before the range of the data and an end datetime after the range of the data
      # This leaves out entries that are entirely before or after the range of the data.
      exist <- DBI::dbGetQuery(
        con,
        sprintf(
          "WITH matched AS (
    SELECT timeseries_data_sharing_agreement_id,
           timeseries_id,
           data_sharing_agreement_id,
           start_dt,
           end_dt
      FROM continuous.timeseries_data_sharing_agreements
     WHERE timeseries_id = %s
       AND (
         (end_dt   BETWEEN '%s' AND '%s')
      OR (start_dt BETWEEN '%s' AND '%s')
      OR (start_dt <= '%s' AND end_dt >= '%s')
       )
    ), fallback AS (
        SELECT timeseries_data_sharing_agreement_id,
               timeseries_id,
               data_sharing_agreement_id,
               start_dt,
               end_dt
          FROM continuous.timeseries_data_sharing_agreements
         WHERE timeseries_id = %s
         ORDER BY end_dt DESC
         LIMIT 1
    )
    SELECT * FROM matched
    UNION ALL
    SELECT * FROM fallback
     WHERE NOT EXISTS (SELECT 1 FROM matched)
    ORDER BY start_dt ASC;",
          timeseries_id,
          min_datetime,
          max_datetime,
          min_datetime,
          max_datetime,
          min_datetime,
          max_datetime,
          timeseries_id
        )
      )
      existing_state <- exist

      if (nrow(exist) == 0) {
        exist <- data.frame(
          timeseries_data_sharing_agreement_id = NA,
          timeseries_id = timeseries_id,
          data_sharing_agreement_id = data$data_sharing_agreement_id[1],
          start_dt = data$datetime[1],
          end_dt = data$datetime[1]
        )
      }
      # Collapse consecutive rows with the same agreement using run-length encoding
      data <- data[order(data$datetime), ]
      runs <- rle(data$data_sharing_agreement_id)
      ends <- cumsum(runs$lengths)
      starts <- c(1, utils::head(ends, -1) + 1)
      new_segments <- data.frame(
        timeseries_data_sharing_agreement_id = NA,
        timeseries_id = timeseries_id,
        data_sharing_agreement_id = runs$values,
        start_dt = data$datetime[starts],
        end_dt = data$datetime[ends],
        stringsAsFactors = FALSE
      )

      exist <- collapse_segments_with_split(
        exist = exist,
        new_segments = new_segments,
        value_col = "data_sharing_agreement_id",
        id_col = "timeseries_data_sharing_agreement_id",
        timeseries_id = timeseries_id
      )

      # Now commit the changes to the database
      commit_fx <- function(con, exist, existing_state, sync_delete_ids) {
        reconcile_segment_changes(
          con = con,
          table_name = "continuous.timeseries_data_sharing_agreements",
          id_col = "timeseries_data_sharing_agreement_id",
          value_col = "data_sharing_agreement_id",
          db_value_col = "data_sharing_agreement_id",
          existing_state = existing_state,
          proposed_state = exist,
          delete_ids = sync_delete_ids
        )
      }

      commit_fx(con, exist, existing_state, sync_delete_ids)

      if (active) {
        DBI::dbExecute(con, "COMMIT;")
      }
    },
    error = function(e) {
      if (active) {
        DBI::dbExecute(con, "ROLLBACK;")
      }
      warning(
        "adjust_data_sharing_agreement: Failed to commit changes to the database with error ",
        e$message
      )
    }
  )
} # End of adjust_data_sharing_agreement function
