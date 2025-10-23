#' Compute increments from cumulative data
#'
#' @description
#' Given a time series of cumulative measurements that may include resets (large drops), this function computes the increments between measurements, accounting for resetsand ignoring small positive noise. The first value provided is treated as the starting point. Since it cannot be determined if this starting point is at zero or not, no increment is computed for it. In cases where values fluctuate, positive differences are only recorded when the next increment value is greater than the previous non-negative value.
#'
#' @param ts data.frame with datetime and value columns
#' @param reset_drop Threshold for a large drop that indicates a true reset. This can be used for example to process standpipe precipitation data where the standpipe is emptied when full or during regular maintenance.
#' @param min_pos Thereshold below which to ignore positive noise in increments
#' @return A data.frame with datetime and value columns. The value column contains the computed increments.
#' @export

compute_increments <- function(
  ts,
  reset_drop = 20,
  min_pos = 0
) {
  ts <- ts[order(ts$datetime), ]
  n <- nrow(ts)
  inc <- rep(NA_real_, n)
  if (n < 2) {
    stop("compute_increments: timeseries must have at least two points")
  }

  # Initialize baseline
  last_max <- ts$value[1]

  # Loop through time series
  for (i in 2:n) {
    d <- ts$value[i] - ts$value[i - 1]
    if (is.na(d)) {
      next
    }

    # Large drop -> hard reset: start new run from current value
    if (d <= -reset_drop) {
      last_max <- ts$value[i]
      inc[i] <- 0
      next
    }

    # Non-negative: only count increment beyond the current baseline
    base <- max(last_max, ts$value[i - 1])
    add <- ts$value[i] - base
    if (add > min_pos) {
      inc[i] <- add
      last_max <- ts$value[i] # advance baseline
    } else {
      # flat or not yet above baseline: no increment
      inc[i] <- 0
    }
  }

  data.frame(datetime = ts$datetime, value = inc)
}
