#' Remove one saved QC rule assignment from a continuous timeseries
#'
#' Removes the assignment and applies the selected cleanup to existing
#' qualifiers in the same transaction. The best-effort action compares
#' the combined output with the selected rule temporarily included against
#' the output that remains when it is omitted. Qualifiers do not store rule provenance, so
#' matching unprotected manual qualifiers may also be affected and stale
#' outputs the rule no longer predicts may remain.
#'
#' @param con An open AquaCache database connection.
#' @param timeseries_id One continuous `timeseries_id`.
#' @param timeseries_qc_rule_id The saved assignment id to remove.
#' @param action One of `"best_effort"`, `"leave"`,
#'   `"regrade_remaining"`, or `"clear_unprotected"`. The latter two require
#'   at least one other saved assignment or no other saved assignments,
#'   respectively.
#' @return Invisibly returns a list with the selected action, deleted rule id,
#'   qualifier counts, and whether the remaining rules were regraded.
#' @details `best_effort` previews the grade with the selected rule included
#'   and with it omitted, then subtracts only the selected rule's unique
#'   predicted output ranges from unprotected qualifier ranges with those
#'   codes. This preserves output still produced by another rule. `leave` deletes only the
#'   assignment. `regrade_remaining`
#'   deletes all unprotected qualifiers and runs the remaining saved rules
#'   across the timeseries record. `clear_unprotected` deletes every
#'   unprotected qualifier and is only valid when no other assignment remains.
#'   Protected qualifiers are retained by the latter two actions. The audit
#'   trigger records the assignment deletion.
#' @seealso [getTimeseriesQCRules()], [gradeContinuous()]
#' @export
removeTimeseriesQCRule <- function(
  con,
  timeseries_id,
  timeseries_qc_rule_id,
  action = c(
    "best_effort",
    "leave",
    "regrade_remaining",
    "clear_unprotected"
  )
) {
  tsid <- qc_rule_integer_id(timeseries_id, "timeseries_id")
  rule_id <- qc_rule_integer_id(
    timeseries_qc_rule_id,
    "timeseries_qc_rule_id"
  )
  action <- match.arg(action)

  active_transaction <- dbTransBegin(con)
  if (!active_transaction) {
    DBI::dbExecute(con, "SAVEPOINT remove_timeseries_qc_rule")
  }

  result <- tryCatch(
    {
      locked <- DBI::dbGetQuery(
        con,
        "SELECT timeseries_qc_rule_id
         FROM continuous.timeseries_qc_rules
         WHERE timeseries_id = $1
         FOR UPDATE",
        params = list(tsid)
      )$timeseries_qc_rule_id
      if (!rule_id %in% locked) {
        stop(
          "QC assignment ", rule_id,
          " does not belong to timeseries ", tsid, "."
        )
      }

      assignments <- getTimeseriesQCRules(con, tsid)
      target_index <- match(rule_id, assignments$timeseries_qc_rule_id)
      if (is.na(target_index)) {
        stop("QC assignment ", rule_id, " is not visible.")
      }
      remaining <- assignments[
        assignments$timeseries_qc_rule_id != rule_id,
        ,
        drop = FALSE
      ]
      if (action == "regrade_remaining" && nrow(remaining) == 0L) {
        stop("regrade_remaining requires another saved QC assignment.")
      }
      if (action == "clear_unprotected" && nrow(remaining) > 0L) {
        stop("clear_unprotected is only valid when removing the final QC rule.")
      }

      counts <- list(
        qualifier_rows_deleted = 0L,
        qualifier_rows_updated = 0L,
        qualifier_segments_inserted = 0L,
        regraded = FALSE
      )

      if (action == "best_effort") {
        counts <- qc_remove_rule_output_ranges(
          con,
          tsid,
          assignments,
          target_index
        )
      } else if (action %in% c("regrade_remaining", "clear_unprotected")) {
        counts$qualifier_rows_deleted <- DBI::dbExecute(
          con,
          "DELETE FROM continuous.qualifiers
           WHERE timeseries_id = $1
             AND no_source_update IS DISTINCT FROM TRUE",
          params = list(tsid)
        )
      }

      deleted <- DBI::dbExecute(
        con,
        "DELETE FROM continuous.timeseries_qc_rules
         WHERE timeseries_id = $1 AND timeseries_qc_rule_id = $2",
        params = list(tsid, rule_id)
      )
      if (deleted != 1L) {
        stop("QC assignment deletion did not affect one row.")
      }

      if (action == "regrade_remaining") {
        first_reading <- DBI::dbGetQuery(
          con,
          "SELECT min(datetime) AS first_dt
           FROM continuous.measurements_continuous
           WHERE timeseries_id = $1",
          params = list(tsid)
        )$first_dt[[1L]]
        if (!is.na(first_reading)) {
          gradeContinuous(
            con,
            tsid,
            from = first_reading,
            reconcile_auto_screened = TRUE
          )
        }
        counts$regraded <- TRUE
      }

      c(
        list(action = action, timeseries_qc_rule_id = rule_id),
        counts
      )
    },
    error = function(e) {
      if (active_transaction) {
        DBI::dbExecute(con, "ROLLBACK")
      } else {
        DBI::dbExecute(con, "ROLLBACK TO SAVEPOINT remove_timeseries_qc_rule")
        DBI::dbExecute(con, "RELEASE SAVEPOINT remove_timeseries_qc_rule")
      }
      stop(e)
    }
  )

  if (active_transaction) {
    DBI::dbExecute(con, "COMMIT")
  } else {
    DBI::dbExecute(con, "RELEASE SAVEPOINT remove_timeseries_qc_rule")
  }
  invisible(result)
}

#' Remove qualifier intervals uniquely produced by a QC assignment
#'
#' Previews the timeseries with and without the selected assignment, then
#' removes only unprotected qualifier intervals whose output is unique to that
#' assignment. Qualifier intervals are shortened or split when only part of an
#' existing interval is affected.
#' @param con An open AquaCache database connection.
#' @param timeseries_id The continuous timeseries being reconciled.
#' @param assignments Saved QC assignments for the timeseries.
#' @param target_index Row index of the assignment to remove from the preview.
#' @return Counts of deleted qualifier rows, updated rows, and inserted
#'   replacement segments; `regraded` is `FALSE` for this cleanup path.
#' @noRd
#' @keywords internal
qc_remove_rule_output_ranges <- function(
  con,
  timeseries_id,
  assignments,
  target_index
) {
  target <- assignments[target_index, , drop = FALSE]
  with_target <- assignments
  with_target$active[target_index] <- TRUE
  without_target <- assignments[-target_index, , drop = FALSE]

  preview_with <- gradeContinuous(
    con,
    timeseries_id,
    write = FALSE,
    assignments = with_target,
    reconcile_auto_screened = TRUE
  )
  preview_without <- gradeContinuous(
    con,
    timeseries_id,
    write = FALSE,
    assignments = without_target,
    reconcile_auto_screened = TRUE
  )
  empty_counts <- list(
    qualifier_rows_deleted = 0L,
    qualifier_rows_updated = 0L,
    qualifier_segments_inserted = 0L,
    regraded = FALSE
  )
  if (
    is.null(preview_with) || is.null(preview_without) ||
      is.null(preview_with$readings) || is.null(preview_without$readings) ||
      is.null(preview_with$ranges) || !nrow(preview_with$ranges) ||
      !nrow(preview_with$readings) || !nrow(preview_without$readings)
  ) {
    return(empty_counts)
  }

  args <- target$rule_args[[1L]]
  rule_emits <- as.character(target$emits[[1L]])
  if (length(rule_emits) == 1L) {
    rule_emits <- strsplit(rule_emits, ",", fixed = TRUE)[[1L]]
  }
  emitted_codes <- unique(c(
    trimws(rule_emits),
    as.character(args$qualifier),
    as.character(args$zero_qualifier)
  ))
  emitted_codes <- emitted_codes[!is.na(emitted_codes) & nzchar(emitted_codes)]

  candidates <- qc_unique_rule_output_ranges(
    preview_with,
    preview_without,
    emitted_codes
  )
  if (!nrow(candidates)) {
    return(empty_counts)
  }

  qualifier_types <- DBI::dbGetQuery(
    con,
    "SELECT qualifier_type_id, qualifier_type_code
     FROM public.qualifier_types
     WHERE qualifier_type_code = ANY($1::text[])",
    params = list(qc_pg_text_array(unique(candidates$code)))
  )
  if (!nrow(qualifier_types)) {
    return(empty_counts)
  }
  candidates$qualifier_type_id <- qualifier_types$qualifier_type_id[
    match(candidates$code, qualifier_types$qualifier_type_code)
  ]
  candidates <- candidates[!is.na(candidates$qualifier_type_id), , drop = FALSE]
  if (!nrow(candidates)) {
    return(empty_counts)
  }

  existing <- DBI::dbGetQuery(
    con,
    "SELECT qualifier_id, qualifier_type_id, start_dt, end_dt
     FROM continuous.qualifiers
     WHERE timeseries_id = $1
       AND qualifier_type_id = ANY($2::integer[])
       AND no_source_update IS DISTINCT FROM TRUE
       AND start_dt <= $4 AND end_dt >= $3
     FOR UPDATE",
    params = list(
      timeseries_id,
      qc_pg_int_array(unique(candidates$qualifier_type_id)),
      min(candidates$start_dt),
      max(candidates$end_dt)
    )
  )
  if (!nrow(existing)) {
    return(empty_counts)
  }
  type_codes <- stats::setNames(
    qualifier_types$qualifier_type_code,
    qualifier_types$qualifier_type_id
  )
  existing$code <- type_codes[as.character(existing$qualifier_type_id)]

  counts <- empty_counts
  for (i in seq_len(nrow(existing))) {
    q <- existing[i, , drop = FALSE]
    q_candidates <- candidates[
      candidates$qualifier_type_id == q$qualifier_type_id[[1L]] &
        candidates$start_dt <= q$end_dt[[1L]] &
        candidates$end_dt >= q$start_dt[[1L]],
      ,
      drop = FALSE
    ]
    if (!nrow(q_candidates)) {
      next
    }
    segments <- qc_subtract_qualifier_ranges(
      q$start_dt[[1L]],
      q$end_dt[[1L]],
      q_candidates
    )
    changed <- nrow(segments) != 1L ||
      as.numeric(segments$start_dt[[1L]]) != as.numeric(q$start_dt[[1L]]) ||
      as.numeric(segments$end_dt[[1L]]) != as.numeric(q$end_dt[[1L]])
    if (!changed) {
      next
    }

    if (!nrow(segments)) {
      deleted <- DBI::dbExecute(
        con,
        "DELETE FROM continuous.qualifiers
         WHERE timeseries_id = $1 AND qualifier_id = $2",
        params = list(timeseries_id, q$qualifier_id[[1L]])
      )
      if (deleted != 1L) {
        stop("Best-effort qualifier deletion did not affect one row.")
      }
      counts$qualifier_rows_deleted <- counts$qualifier_rows_deleted + 1L
      next
    }

    updated <- DBI::dbExecute(
      con,
      "UPDATE continuous.qualifiers
       SET start_dt = $3, end_dt = $4
       WHERE timeseries_id = $1 AND qualifier_id = $2",
      params = list(
        timeseries_id,
        q$qualifier_id[[1L]],
        segments$start_dt[[1L]],
        segments$end_dt[[1L]]
      )
    )
    if (updated != 1L) {
      stop("Best-effort qualifier update did not affect one row.")
    }
    counts$qualifier_rows_updated <- counts$qualifier_rows_updated + 1L
    if (nrow(segments) > 1L) {
      additions <- data.frame(
        timeseries_id = rep(as.integer(timeseries_id), nrow(segments) - 1L),
        qualifier_type_id = rep(
          as.integer(q$qualifier_type_id[[1L]]),
          nrow(segments) - 1L
        ),
        start_dt = segments$start_dt[-1L],
        end_dt = segments$end_dt[-1L],
        no_source_update = rep(FALSE, nrow(segments) - 1L),
        stringsAsFactors = FALSE
      )
      dbAppendTableRLS(con, "continuous.qualifiers", additions)
      counts$qualifier_segments_inserted <-
        counts$qualifier_segments_inserted + nrow(additions)
    }
  }
  counts
}

#' Find ranges whose output is unique to one QC assignment
#'
#' Compares previews with and without an assignment and keeps timestamp runs
#' where the selected rule emitted one of its configured qualifier codes and
#' changed the combined output.
#' @param preview_with Result from [gradeContinuous()] with the target
#'   assignment included.
#' @param preview_without Result from [gradeContinuous()] without the target
#'   assignment.
#' @param emitted_codes Qualifier codes the target assignment can emit.
#' @return A data frame with `code`, `start_dt`, `end_dt`, and reading count
#'   (`n`) for each contiguous output range. Returns an empty data frame when
#'   the previews have no unique output.
#' @noRd
#' @keywords internal
qc_unique_rule_output_ranges <- function(
  preview_with,
  preview_without,
  emitted_codes
) {
  empty <- data.frame(
    code = character(),
    start_dt = as.POSIXct(character(), tz = "UTC"),
    end_dt = as.POSIXct(character(), tz = "UTC"),
    n = integer(),
    stringsAsFactors = FALSE
  )
  if (
    is.null(preview_with$readings) || is.null(preview_without$readings) ||
      is.null(preview_with$ranges) || !nrow(preview_with$readings) ||
      !nrow(preview_without$readings) || !nrow(preview_with$ranges)
  ) {
    return(empty)
  }

  readings <- preview_with$readings
  remaining_index <- match(
    readings$datetime,
    preview_without$readings$datetime
  )
  if (
    anyNA(remaining_index) ||
      length(remaining_index) != nrow(preview_without$readings)
  ) {
    stop("QC previews did not produce matching reading timestamps.")
  }
  with_code <- as.character(readings$code)
  without_code <- as.character(
    preview_without$readings$code[remaining_index]
  )
  # A missing remaining code means no output, for example when another
  # qualifier suppresses the placeholder baseline. The selected output is
  # still unique at that reading.
  unique_contribution <-
    qc_covered(readings$datetime, preview_with$ranges) &
    !is.na(with_code) & with_code %in% emitted_codes &
    (is.na(without_code) | with_code != without_code)
  candidates <- qc_runs_to_ranges(
    readings$datetime,
    ifelse(unique_contribution, with_code, NA_character_)
  )
  candidates[!is.na(candidates$code), , drop = FALSE]
}

#' Subtract cut intervals from a qualifier interval
#'
#' Clips and merges cut intervals, then returns the portions of the original
#' interval that remain outside those cuts.
#' @param start_dt Start of the qualifier interval.
#' @param end_dt End of the qualifier interval.
#' @param cuts Data frame of cut intervals with `start_dt` and `end_dt`
#'   columns.
#' @return A data frame with `start_dt` and `end_dt` for each remaining
#'   segment; it has zero rows when the cuts cover the full interval.
#' @noRd
#' @keywords internal
qc_subtract_qualifier_ranges <- function(start_dt, end_dt, cuts) {
  epsilon <- 1e-6
  start <- as.numeric(start_dt)
  end <- as.numeric(end_dt)
  cut_start <- pmax(start, as.numeric(cuts$start_dt))
  cut_end <- pmin(end, as.numeric(cuts$end_dt))
  valid <- is.finite(cut_start) & is.finite(cut_end) & cut_start <= cut_end
  cut_start <- cut_start[valid]
  cut_end <- cut_end[valid]
  if (!length(cut_start)) {
    return(data.frame(
      start_dt = as.POSIXct(start, origin = "1970-01-01", tz = "UTC"),
      end_dt = as.POSIXct(end, origin = "1970-01-01", tz = "UTC")
    ))
  }

  order_cuts <- order(cut_start, cut_end)
  cut_start <- cut_start[order_cuts]
  cut_end <- cut_end[order_cuts]
  merged_start <- numeric()
  merged_end <- numeric()
  for (i in seq_along(cut_start)) {
    if (
      !length(merged_start) ||
        cut_start[[i]] > tail(merged_end, 1L) + epsilon
    ) {
      merged_start <- c(merged_start, cut_start[[i]])
      merged_end <- c(merged_end, cut_end[[i]])
    } else {
      merged_end[[length(merged_end)]] <- max(
        merged_end[[length(merged_end)]],
        cut_end[[i]]
      )
    }
  }

  segments <- vector("list", length(merged_start) + 1L)
  n_segments <- 0L
  cursor <- start
  for (i in seq_along(merged_start)) {
    if (merged_start[[i]] > cursor) {
      left_end <- merged_start[[i]] - epsilon
      if (left_end >= cursor) {
        n_segments <- n_segments + 1L
        segments[[n_segments]] <- c(cursor, left_end)
      }
    }
    cursor <- max(cursor, merged_end[[i]] + epsilon)
    if (cursor > end) {
      break
    }
  }
  if (cursor <= end) {
    n_segments <- n_segments + 1L
    segments[[n_segments]] <- c(cursor, end)
  }
  if (!n_segments) {
    return(data.frame(
      start_dt = as.POSIXct(character()),
      end_dt = as.POSIXct(character())
    ))
  }
  values <- do.call(rbind, segments[seq_len(n_segments)])
  data.frame(
    start_dt = as.POSIXct(values[, 1L], origin = "1970-01-01", tz = "UTC"),
    end_dt = as.POSIXct(values[, 2L], origin = "1970-01-01", tz = "UTC")
  )
}
