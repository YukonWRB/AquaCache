#' Compute increments from cumulative data
#'
#' @description
#' Given a time series of cumulative measurements that may include resets (large drops), this function computes the increments between measurements, accounting for resetsand ignoring small positive noise. The first value provided is treated as the starting point. Since it cannot be determined if this starting point is at zero or not, no increment is computed for it. In cases where values fluctuate, positive differences are only recorded when the next increment value is greater than the previous non-negative value.
#'
#' @param ts data.frame with datetime and value columns
#' @param reset_drop Threshold for a large drop that indicates a true reset. This can be used for example to process standpipe precipitation data where the standpipe is emptied when full or during regular maintenance.
#' @param min_pos Thereshold below which to ignore positive noise in increments. For example, if min_pos is 2, only increments of 2 or more units will be recorded; smaller positive differences will be treated as zero increments.
#' @param max_gap Maximum allowed gap in number of data points above which an increment is not computed.
#' @return A data.frame with datetime and value columns. The value column contains the computed increments.
#' @export

compute_increments <- function(
  ts,
  reset_drop = 20,
  min_pos = 0,
  max_gap = 0
) {
  as_dt <- data.table::is.data.table(ts)
  if (!as_dt) {
    ts <- data.table::as.data.table(ts)
  }

  data.table::setorder(ts, "datetime")
  if (nrow(ts) < 2) {
    stop("compute_increments: timeseries must have at least two points")
  }

  # Add points where there are missing data
  ts <- suppressWarnings(calculate_period(ts, timeseries_id = NA))
  ts[, "period_secs" := as.numeric(lubridate::period(period))]

  # ---- Expand gaps with NA rows at each expected interval ----
  # Build per-row "next observed" and expected step
  ts[, "next_dt" := data.table::shift(datetime, type = "lead")]
  # For each gap, make a seq of missing datetimes at expected cadence
  na_rows <- ts[
    !is.na(next_dt) & (datetime + period_secs) < next_dt,
    {
      gap_sec <- as.numeric(next_dt - datetime)
      # how many expected points are missing strictly between the two observed points
      n_missing <- floor(gap_sec / period_secs) - 1L
      if (is.finite(period_secs) && period_secs > 0 && n_missing > 0L) {
        data.table(
          datetime = seq(
            from = datetime + period_secs,
            by = period_secs,
            length.out = n_missing
          ),
          value = NA_real_
        )
      } else {
        NULL
      }
    },
    by = .I
  ][, .(datetime, value)]

  # Bind and order
  ts <- data.table::rbindlist(
    list(ts[, .(datetime, value)], na_rows),
    use.names = TRUE,
    fill = TRUE
  )
  data.table::setorder(ts, "datetime")

  # Precompute length of preceding NA run
  n <- nrow(ts)
  is_na <- is.na(ts$value)
  na_run_len <- integer(n)
  if (any(is_na)) {
    r <- rle(is_na)
    ends <- cumsum(r$lengths)
    starts <- ends - r$lengths + 1
    for (k in which(r$values)) {
      na_run_len[starts[k]:ends[k]] <- r$lengths[k]
    }
  }

  # Compute increments with reset_drop, min_pos, and max_gap rules
  inc <- rep(NA_real_, n)
  # Initialize baseline
  last_max <- ts$value[1]

  # Loop through time series
  for (i in 2:n) {
    # If we just crossed a gap: enforce max_gap
    if (is_na[i - 1] && na_run_len[i - 1] > max_gap) {
      inc[i] <- NA_real_ # don't compute across oversized gap
      if (!is.na(ts$value[i])) {
        last_max <- ts$value[i]
      } # reset baseline at first valid
      next
    }

    # If either side is NA (but gap not oversized), no increment
    if (is.na(ts$value[i]) || is.na(ts$value[i - 1])) {
      next
    }

    d <- ts$value[i] - ts$value[i - 1]

    # Hard reset if large drop
    if (d <= -reset_drop) {
      last_max <- ts$value[i]
      inc[i] <- 0
      next
    }

    # Increment beyond baseline, ignore small positive noise
    base <- max(last_max, ts$value[i - 1])
    add <- ts$value[i] - base
    if (add >= min_pos) {
      inc[i] <- add
      last_max <- ts$value[i]
    } else {
      inc[i] <- 0
    }
  }

  # Rebuild data.table
  out <- data.table::data.table(datetime = ts$datetime, value = inc)

  # Drop rows with NA 'value' (these were gaps we inserted)
  out <- out[!is.na(value)]

  if (as_dt) {
    return(out)
  } else {
    return(as.data.frame(out))
  }

  # result <- if (as_dt) data else as.data.frame(data)
}
