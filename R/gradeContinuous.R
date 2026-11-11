# Post-import quality-control grading for continuous timeseries.
#
# Rules are catalogued in public.qc_rule_capabilities and assigned per
# timeseries in continuous.timeseries_qc_rules, mirroring the source adapter
# pair (source_adapter_capabilities / timeseries_source_adapters). Each rule
# here is an R function registered in qc_rule_registry; a catalogue row with
# no R implementation stops gradeContinuous().
#
# Why not adjust_qualifier(): collapse_segments_with_split() rebuilds a
# half-open time partition and skips zero-width intervals, so a qualifier on
# a single reading - the common case for QC flags - would be dropped. It also
# only fetches existing segments of the first row's qualifier type.
# gradeContinuous() therefore reconciles its own ranges (qc_write_ranges()).
#
# The rules and the ERROR/SUS/CT/SSM/PSM mapping come from NWT ECC's legacy
# QC (flag_data() in the nwtclimate package), so data loaded from that
# system and data graded here are marked alike.

# One code per reading. PSM sits above CT on purpose: in flag_data() the
# snowmelt test overwrites rain_flag, so historically QC_PSM won over the
# QC_CT-type condition. Keeping that order marks future data like the past.
qc_severity <- c("ERROR", "SSM", "PSM", "CT", "SUS")

# --- Rolling helpers (centred, by READING not by time, NA if any reading in
# the window is NA or the window runs off either end - same as zoo with
# fill = NA, which flag_data() used) -----------------------------------------

#' Shift a numeric vector by a reading offset
#'
#' Positions moved beyond either end are filled with `NA_real_`.
#' @param x Numeric vector.
#' @param s Integer shift; positive values move values toward lower indices
#'   and negative values move them toward higher indices.
#' @return A numeric vector with the same length as `x`.
#' @noRd
#' @keywords internal
qc_shift <- function(x, s) {
  n <- length(x)
  if (abs(s) >= n) {
    return(rep(NA_real_, n))
  }
  if (s >= 0) {
    c(x[(s + 1):n], rep(NA_real_, s))
  } else {
    c(rep(NA_real_, -s), x[1:(n + s)])
  }
}

#' Build centered reading windows
#'
#' Each output row contains the `k` readings centered on the corresponding
#' input row; incomplete windows contain `NA` values.
#' @param x Numeric input vector.
#' @param k Number of readings in each window.
#' @return A matrix with one row per input reading and `k` columns.
#' @noRd
#' @keywords internal
qc_window_matrix <- function(x, k) {
  lo <- (k - 1) %/% 2
  vapply(seq_len(k) - 1L - lo, function(s) qc_shift(x, s), numeric(length(x)))
}

# Direct deviations, not a running sum of squares: thresholds go down to
# sd < 0.001 and cumulative sums would lose that to cancellation.
#' Calculate centered rolling sample standard deviations
#'
#' Uses direct deviations from each window mean to retain precision for very
#' small standard deviations.
#' @param x Numeric input vector.
#' @param k Number of readings in each window.
#' @return A numeric vector; incomplete or missing-value windows yield `NA`.
#' @noRd
#' @keywords internal
qc_roll_sd <- function(x, k) {
  m <- matrix(qc_window_matrix(x, k), ncol = k)
  mu <- rowMeans(m)
  sqrt(rowSums((m - mu)^2) / (k - 1))
}

# Running sums are fine for a mean, and k = 720 is too wide for a matrix.
# align = "center" as zoo's default; "right" = trailing (the reading and the
# k - 1 before it), as zoo's align = "right".
#' Calculate rolling means over readings
#'
#' Centered alignment places the reading in the middle of the window. Right
#' alignment uses the reading and its `k - 1` predecessors.
#' @param x Numeric input vector.
#' @param k Number of readings in each window.
#' @param align Either `"center"` or `"right"`.
#' @return A numeric vector; incomplete or missing-value windows yield `NA`.
#' @noRd
#' @keywords internal
qc_roll_mean <- function(x, k, align = "center") {
  n <- length(x)
  lo <- if (align == "right") k - 1 else (k - 1) %/% 2
  hi <- k - 1 - lo
  i <- seq_len(n)
  a <- i - lo
  b <- i + hi
  ok <- a >= 1 & b <= n
  cs <- c(0, cumsum(ifelse(is.na(x), 0, x)))
  cn <- c(0, cumsum(is.na(x)))
  out <- rep(NA_real_, n)
  out[ok] <- (cs[b[ok] + 1] - cs[a[ok]]) / k
  out[ok][(cn[b[ok] + 1] - cn[a[ok]]) > 0] <- NA_real_
  out
}

# The windows above count READINGS. Across a gap in the record that reaches
# back (or forward) in time: where a record resumes in July after a gap, the
# 720 readings before an early-August reading end in the previous winter and
# give a 30-day mean of about -18 C. qc_window_span_ok() is FALSE where the k readings span more
# than k - 1 recording intervals, i.e. where rows are missing - the same "any
# gap = no result" rule the helpers apply to NA values. flag_data() had no
# such check (zoo windows count rows too).
#' Check that a reading window has no time gaps
#'
#' The window must span no more than the expected duration for `k` readings,
#' allowing a one-percent cadence tolerance.
#' @param datetime POSIXct reading timestamps in order.
#' @param k Number of readings in the window.
#' @param align Either `"center"` or `"right"`.
#' @param rate_s Expected seconds between readings.
#' @return A logical vector with one value per timestamp.
#' @noRd
#' @keywords internal
qc_window_span_ok <- function(datetime, k, align = "center", rate_s) {
  t <- as.numeric(datetime)
  lo <- if (align == "right") k - 1 else (k - 1) %/% 2
  hi <- k - 1 - lo
  first <- qc_shift(t, -lo)
  last <- qc_shift(t, hi)
  !is.na(first) & !is.na(last) & (last - first) <= (k - 1) * rate_s * 1.01
}

# --- Rules -------------------------------------------------------------------
# Each takes x (datetime, value, plus one column per companion role), its
# merged arguments, and ctx (utc_offset in hours, rate_s in seconds). Returns a character vector:
# a qualifier code, or NA for no flag.

#' Flag precipitation recorded while air temperature is below threshold
#'
#' This rule emits its configured qualifier where precipitation is positive
#' and the `air_temp` companion is below `max_air_temp`.
#' @param x Data frame with `value` and `air_temp` columns.
#' @param a Merged rule arguments, including `max_air_temp` and `qualifier`.
#' @param ctx Rule context (unused by this rule).
#' @return A qualifier-code vector, with `NA` where no reading is flagged.
#' @noRd
#' @keywords internal
qc_rain_cold <- function(x, a, ctx) {
  hit <- !is.na(x$value) &
    x$value > 0 &
    !is.na(x$air_temp) &
    x$air_temp < a$max_air_temp
  ifelse(hit, a$qualifier, NA_character_)
}

# Section 10 split for a stuck 0 m/s (zero_split = TRUE, wind speed only):
# below zero_max_air_temp or no air temp -> SUS (frozen propeller OR calm cold
# night); at or above it -> no flag (calm is real). Stuck at any other value
# -> ERROR.
#' Flag a sensor value that remains nearly constant
#'
#' Uses rolling standard deviation and cadence checks. When `zero_split` is
#' enabled, cold or missing air temperature maps stuck zero readings to
#' `zero_qualifier`; warm zero readings are left unflagged.
#' @param x Data frame with `datetime`, `value`, and optionally `air_temp`.
#' @param a Merged rule arguments (`width`, `min_sd`, qualifier codes, and
#'   optional zero-split settings).
#' @param ctx Rule context containing the expected `rate_s`.
#' @return A qualifier-code vector, with `NA` where no reading is flagged.
#' @noRd
#' @keywords internal
qc_stuck_sensor <- function(x, a, ctx) {
  s <- qc_roll_sd(x$value, a$width)
  stuck <- !is.na(s) &
    s < a$min_sd &
    qc_window_span_ok(x$datetime, a$width, "center", ctx$rate_s)
  out <- ifelse(stuck, a$qualifier, NA_character_)
  if (isTRUE(a$zero_split)) {
    tair <- if ("air_temp" %in% names(x)) x$air_temp else NA_real_
    zero <- stuck & x$value == 0
    cold <- is.na(tair) | tair < a$zero_max_air_temp
    out[zero & cold] <- a$zero_qualifier
    out[zero & !cold] <- NA_character_
  }
  out
}

#' Flag wind direction recorded at calm wind speed
#'
#' Emits the configured qualifier when `wind_speed` is at or below the
#' configured maximum.
#' @param x Data frame with `value` and `wind_speed` columns.
#' @param a Merged rule arguments, including `max_wind_speed` and `qualifier`.
#' @param ctx Rule context (unused by this rule).
#' @return A qualifier-code vector, with `NA` where no reading is flagged.
#' @noRd
#' @keywords internal
qc_dir_calm <- function(x, a, ctx) {
  hit <- !is.na(x$value) &
    !is.na(x$wind_speed) &
    x$wind_speed <= a$max_wind_speed
  ifelse(hit, a$qualifier, NA_character_)
}

# Two conditions from flag_data(), both on rain > 0:
#   early: the first first_warm_days days of the calendar year (local) from
#          the first day with air temp above 0 - unless the year's data
#          already starts warm, as in flag_data().
#   thaw:  air temp above 0 while the mean air temp over cold_window readings
#          is below 0 (warm after cold).
# cold_align: "trailing" (default) averages the reading
# and the cold_window - 1 before it - "has the past month been cold?" - which
# is the snowmelt question, and flags data as it arrives. It runs straight
# across January 1. "centred" reproduces flag_data() (zoo::rollmean's
# default, rolled within each calendar year as it grouped by station-year),
# whose flag can only appear once cold_window / 2 readings follow.
# Trailing finds no FALL melt (in October the past month is warm). NWT judged
# fall melt too rare to flag for now; revisit if it becomes common.
# Difference from flag_data(): it ran per download batch (e.g. April to
# September); this always looks back to January 1 (window() asks for it).
#' Flag rain recorded during spring snowmelt conditions
#'
#' Combines the configured early-season window with a warm-after-cold rolling
#' air-temperature test. The rolling alignment and year boundary come from the
#' rule arguments.
#' @param x Data frame with `datetime`, `value`, and `air_temp` columns.
#' @param a Merged rule arguments for warm-day and cold-window thresholds.
#' @param ctx Rule context containing `utc_offset` and `rate_s`.
#' @return A qualifier-code vector, with `NA` where no reading is flagged.
#' @noRd
#' @keywords internal
qc_snowmelt_tips <- function(x, a, ctx) {
  day <- as.numeric(as.Date(x$datetime + ctx$utc_offset * 3600, tz = "UTC"))
  yr <- format(as.Date(day, origin = "1970-01-01"), "%Y")
  warm <- !is.na(x$air_temp) & x$air_temp > 0
  rain <- !is.na(x$value) & x$value > 0
  first_warm <- tapply(day[warm], yr[warm], min)
  first_seen <- tapply(day, yr, min)
  d0 <- unname(first_warm[yr])
  d0[!is.na(d0) & d0 == unname(first_seen[yr])] <- NA
  early <- rain & !is.na(d0) & day >= d0 & day < d0 + a$first_warm_days
  align <- if (identical(a$cold_align, "centred")) "center" else "right"
  cold_mean <- if (align == "center") {
    unsplit(lapply(split(x$air_temp, yr), qc_roll_mean, k = a$cold_window), yr)
  } else {
    qc_roll_mean(x$air_temp, a$cold_window, align = "right")
  }
  cold_mean[
    !qc_window_span_ok(x$datetime, a$cold_window, align, ctx$rate_s)
  ] <- NA
  thaw <- rain & warm & !is.na(cold_mean) & cold_mean < 0
  ifelse(early | thaw, a$qualifier, NA_character_)
}

# window(): how many READINGS of context a rule needs before (back) and after
# (ahead) a reading, and whether it needs the calendar year from January 1.
#' Validate an integer-valued QC rule argument
#'
#' @param args Named list of merged rule arguments.
#' @param name Argument name to validate.
#' @param min_value Smallest permitted integer.
#' @return The validated value as an integer.
#' @noRd
#' @keywords internal
qc_validate_integer_arg <- function(args, name, min_value) {
  value <- args[[name]]
  if (
    length(value) != 1L ||
      !is.numeric(value) ||
      is.na(value) ||
      !is.finite(value) ||
      value < min_value ||
      value > .Machine$integer.max ||
      value != floor(value)
  ) {
    stop(
      "gradeContinuous: argument '",
      name,
      "' must be an integer greater than or equal to ",
      min_value
    )
  }
  as.integer(value)
}

qc_rule_registry <- list(
  "qc_rain_cold@1" = list(
    fx = qc_rain_cold,
    window = function(a) list(back = 0, ahead = 0, year_start = FALSE),
    primary_parameters = "precipitation, rain"
  ),
  "qc_stuck_sensor@1" = list(
    fx = qc_stuck_sensor,
    window = function(a) {
      width <- qc_validate_integer_arg(a, "width", 2L)
      list(
        back = (width - 1L) %/% 2L,
        ahead = width %/% 2L,
        year_start = FALSE
      )
    }
  ),
  "qc_dir_calm@1" = list(
    fx = qc_dir_calm,
    window = function(a) list(back = 0, ahead = 0, year_start = FALSE),
    primary_parameters = "wind direction (direction from, expressed 0-360 deg)"
  ),
  "qc_snowmelt_tips@1" = list(
    fx = qc_snowmelt_tips,
    primary_parameters = c("precipitation, rain", "precipitation, total"),
    window = function(a) {
      cold_window <- qc_validate_integer_arg(a, "cold_window", 1L)
      qc_validate_integer_arg(a, "first_warm_days", 0L)
      if (
        length(a$cold_align) != 1L ||
          is.na(a$cold_align) ||
          !a$cold_align %in% c("trailing", "centred")
      ) {
        stop(
          "gradeContinuous: argument 'cold_align' must be 'trailing' or 'centred'"
        )
      }
      if (identical(a$cold_align, "centred")) {
        list(
          back = (cold_window - 1L) %/% 2L,
          ahead = cold_window %/% 2L,
          year_start = TRUE
        )
      } else {
        list(back = cold_window - 1L, ahead = 0L, year_start = TRUE)
      }
    }
  )
)

#' Get the primary parameter names for a registered QC rule
#'
#' @param rule_fx Rule function name from the capability catalogue.
#' @param rule_version Version of that rule.
#' @return Character vector of primary parameter names, or `character()` when
#'   the rule has no registered parameter metadata.
#' @noRd
#' @keywords internal
qc_rule_primary_parameters <- function(rule_fx, rule_version) {
  key <- qc_rule_capability_key(rule_fx, rule_version)
  definition <- qc_rule_registry[[key]]
  if (is.null(definition) || is.null(definition$primary_parameters)) {
    return(character())
  }
  as.character(definition$primary_parameters)
}

# --- Plumbing ----------------------------------------------------------------

#' Format an integer vector as a PostgreSQL array literal
#'
#' @param x Integer values.
#' @return A brace-delimited PostgreSQL integer-array string.
#' @noRd
#' @keywords internal
qc_pg_int_array <- function(x) {
  paste0("{", paste(as.integer(x), collapse = ","), "}")
}

#' Format a character vector as a PostgreSQL text-array literal
#'
#' Embedded quotes and backslashes are escaped for PostgreSQL array syntax.
#' @param x Character values.
#' @return A brace-delimited PostgreSQL text-array string.
#' @noRd
#' @keywords internal
qc_pg_text_array <- function(x) {
  paste0(
    "{",
    paste0('"', gsub('(["\\\\])', "\\\\\\1", x), '"', collapse = ","),
    "}"
  )
}

#' Mark timestamps covered by any supplied range
#'
#' @param datetime POSIXct timestamps to check.
#' @param ranges Data frame with `start_dt` and `end_dt` columns.
#' @return Logical vector aligned with `datetime`.
#' @noRd
#' @keywords internal
qc_covered <- function(datetime, ranges) {
  held <- rep(FALSE, length(datetime))
  for (i in seq_len(nrow(ranges))) {
    held <- held |
      (datetime >= ranges$start_dt[i] & datetime <= ranges$end_dt[i])
  }
  held
}

#' Resolve effective QC rule arguments
#'
#' Validates the catalogue schema, applies its defaults, and overlays saved
#' assignment values.
#' @param schema_json JSON text from the rule capability's argument schema.
#' @param args_json JSON text containing assignment-specific values.
#' @return A named list of effective rule arguments.
#' @noRd
#' @keywords internal
qc_merge_rule_args <- function(schema_json, args_json) {
  schema <- jsonlite::fromJSON(schema_json, simplifyVector = FALSE)
  validateQCRuleArgumentSchema(schema)
  qc_rule_effective_args(schema, args_json)
}

#' Resolve companion timeseries for a QC rule
#'
#' Explicit assignment overrides take precedence over the schema's default
#' same-location selection. Overrides are revalidated against current active
#' companion candidates.
#' @param con AquaCache database connection.
#' @param location_id Target timeseries location ID.
#' @param target_timeseries_id Timeseries being graded; excluded from candidates.
#' @param descriptors Active companion-role descriptors from the rule schema.
#' @param overrides Named list of explicit companion timeseries IDs.
#' @return Named integer list mapping companion roles to timeseries IDs; missing
#'   roles have value `NA_integer_`.
#' @noRd
#' @keywords internal
qc_resolve_companions <- function(
  con,
  location_id,
  target_timeseries_id,
  descriptors,
  overrides
) {
  out <- list()
  for (descriptor in descriptors) {
    role <- descriptor$role
    explicit <- overrides[[role]]
    candidates <- getQCRuleCompanionCandidates(
      con,
      location_id,
      descriptor$parameter_name,
      exclude_timeseries_id = target_timeseries_id
    )
    if (length(explicit)) {
      if (!isTRUE(descriptor$allow_explicit_override)) {
        stop(
          "gradeContinuous: companion role '",
          role,
          "' does not allow overrides"
        )
      }
      if (!explicit %in% candidates$timeseries_id) {
        stop(
          "gradeContinuous: configured companion '",
          role,
          "' is no longer an active same-location series for '",
          descriptor$parameter_name,
          "'. Update the QC assignment."
        )
      }
      out[[role]] <- as.integer(explicit)
    } else {
      out[[role]] <- if (nrow(candidates)) {
        candidates$timeseries_id[[1L]]
      } else {
        NA_integer_
      }
    }
  }
  out
}

#' Read corrected measurements for a QC evaluation window
#'
#' Uses the Patch 53 set-returning database function. Trimmed readings remain in
#' the result with a missing corrected value.
#' @param con AquaCache database connection.
#' @param tsid Timeseries ID.
#' @param from Inclusive start datetime.
#' @param to Inclusive end datetime.
#' @return Data frame with ordered `datetime` and corrected numeric `value`.
#' @noRd
#' @keywords internal
qc_read_corrected <- function(con, tsid, from, to) {
  DBI::dbGetQuery(
    con,
    "SELECT datetime, value_corrected::double precision AS value FROM continuous.measurements_continuous_corrected($1::integer, $2::timestamptz, $3::timestamptz) ORDER BY datetime",
    params = list(tsid, from, to)
  )
}

#' Combine QC rule outputs by configured severity
#'
#' Codes absent from `qc_severity` rank below the registered standard codes.
#' @param codes List of equal-length qualifier-code vectors, one per rule.
#' @return One qualifier code or `NA` per reading.
#' @noRd
#' @keywords internal
qc_combine_codes <- function(codes) {
  rank_of <- function(v) {
    ifelse(
      is.na(v),
      NA_integer_,
      match(v, qc_severity, nomatch = length(qc_severity) + 1L)
    )
  }
  out <- codes[[1]]
  for (v in codes[-1]) {
    take <- !is.na(v) & (is.na(out) | rank_of(v) < rank_of(out))
    out[take] <- v[take]
  }
  out
}

#' Convert consecutive qualifier codes to time ranges
#'
#' Each consecutive run of one code becomes a range from its first timestamp
#' through its last timestamp.
#' @param datetime Ordered POSIXct reading timestamps.
#' @param code Qualifier code for each reading; `NA` separates runs.
#' @return Data frame with `code`, `start_dt`, `end_dt`, and reading count `n`.
#' @noRd
#' @keywords internal
qc_runs_to_ranges <- function(datetime, code) {
  r <- rle(code)
  end <- cumsum(r$lengths)
  start <- end - r$lengths + 1L
  data.frame(
    code = r$values,
    start_dt = datetime[start],
    end_dt = datetime[end],
    n = r$lengths,
    stringsAsFactors = FALSE
  )
}

# --- Main --------------------------------------------------------------------

#' Apply configured QC rules to a continuous timeseries
#'
#' @description
#' Applies the quality-control rules assigned to a timeseries in
#' `continuous.timeseries_qc_rules` and writes the resulting qualifiers to
#' `continuous.qualifiers`. Eligible readings reviewed by an active rule also
#' receive approval code `S` (auto screened, no human review). Readings with
#' no rule flags get `baseline` (UNS).
#' Called after new data are imported (see [getNewContinuous()] and
#' [synchronize_continuous()]); can also be run directly, e.g. with
#' `write = FALSE` to preview.
#'
#' Rules read CORRECTED values, so readings hidden by a trim correction are
#' treated as missing. Each reading gets at most one rule qualifier, by
#' severity: ERROR > SSM > PSM > CT > SUS.
#'
#' @details
#' **One managed code per reading.** After grading, every reading in the
#' window carries exactly one of the codes the grader manages: a new code
#' replaces the old one rather than being added beside it, e.g. a reading
#' first left UNS for lack of "ahead" context and later judged ERROR.
#'
#' **Which ranges are replaced.** Only stored qualifiers of the codes the
#' grader manages (`UNK`, `UNS`, `baseline`, `replace` and the codes its rules
#' emit), inside the graded window, and only if not protected
#' (`no_source_update = TRUE`). A baseline `UNK` or `UNS` is omitted where a
#' different qualifier already covers the reading; a rule qualifier may
#' overlap another non-placeholder qualifier.
#' Codes it does not manage are left alone. Ranges it writes are
#' source-managed (`no_source_update = FALSE`), so later runs can replace
#' them.
#'
#' Inactive saved assignments also contribute managed codes, so unprotected
#' flags can be cleared to `baseline`. Previously auto-screened `S` ranges are
#' reconsidered only for explicit rule-removal reconciliation (or when
#' `reconcile_auto_screened = TRUE`). With no active rules, unprotected managed
#' flags in the eligible graded window become `baseline`. Use `from = NULL` to
#' clear the whole record. Series with no saved assignments are left alone. A
#' preview also manages codes from saved assignments omitted from the proposed
#' set.
#'
#' **Approval-aware screening.** Readings whose current approval code is
#' `UNK`, `UNS` or `N` (not reviewed), or which have no approval row, may be
#' screened. Other approval codes are retained and excluded from automatic
#' QC. Protected approval ranges keep their current code. Qualifier and
#' approval writes are split around these ranges, including when no reading
#' falls within an existing approval interval. `S` is included only when
#' `reconcile_auto_screened = TRUE`, for explicit rule-removal reconciliation;
#' it is otherwise treated as an existing approval that must be left alone.
#'
#' **Protected readings.** A reading covered by a protected range of a
#' managed code gets no new code of any type: the protected range is never
#' changed, and the grader writes around it. Rules only reproduce what they
#' detect, so this is how a flag set some other way (by hand, or loaded from
#' a legacy QC system) survives a regrade, and also a re-sync, which replaces
#' unprotected qualifiers. With `from = NULL` the whole record is regraded,
#' with a warning.
#'
#' **Windows.** Rules that look at neighbouring readings are given enough
#' context either side of the graded readings, and readings near the start
#' of the window are re-graded once later readings exist. A rolling window
#' that spans a gap in the record gives no result.
#'
#' @param con An open AquaCache connection. Write access is needed only when
#'   `write = TRUE`.
#' @param timeseries_id One timeseries_id.
#' @param from,to Datetimes (UTC) of the readings to grade, usually the new
#'   data. NULL means the start / end of the record.
#' @param baseline Qualifier code for readings no rule flags.
#' @param replace Other qualifier codes the grader may replace inside its
#'   window. [getNewContinuous()] writes UNK on new rows when the source
#'   supplies no qualifier, so new readings arrive as UNK and leave as
#'   `baseline` or a rule's code.
#' @param write FALSE returns what would be written, without writing.
#' @param assignments Optional desired QC assignments for a read-only preview.
#'   When supplied, these values are validated and graded without saving them.
#'   This is allowed only with `write = FALSE`.
#' @param reconcile_auto_screened Logical. Set TRUE only while explicitly
#'   reconciling a saved rule change; this allows prior `S` ranges to be
#'   recalculated. Normal imports and fetches leave this FALSE and skip `S`.
#'
#' @return Invisibly. With `write = FALSE`, a list: `ranges` (what would be
#'   written, around protected readings), `readings` (one code per reading
#'   over the whole window, for comparison with stored qualifiers),
#'   `approvals` (the approval state that would be written), `window` and
#'   `protected` (the number of readings held by protected ranges). With
#'   `write = TRUE`, a data.frame of readings and ranges per code, plus the
#'   total `approval_readings` screened. An approval-only update returns one
#'   row with `code = NA`, `n = 0`, and `ranges = 0`. NULL if the series has no
#'   saved or proposed assignments, or nothing to write.
#' @seealso [adjust_qualifier()]
#' @export
gradeContinuous <- function(
  con,
  timeseries_id,
  from = NULL,
  to = NULL,
  baseline = "UNS",
  replace = "UNK",
  write = TRUE,
  assignments = NULL,
  reconcile_auto_screened = FALSE
) {
  tsid <- as.integer(timeseries_id)
  stopifnot(length(tsid) == 1, !is.na(tsid))
  if (
    !is.logical(reconcile_auto_screened) ||
      length(reconcile_auto_screened) != 1L ||
      is.na(reconcile_auto_screened)
  ) {
    stop("reconcile_auto_screened must be TRUE or FALSE.")
  }
  full_regrade <- is.null(from)
  if (!is.null(assignments) && write) {
    stop(
      "gradeContinuous: assignments can only be supplied with write = FALSE."
    )
  }

  # Retain inactive assignments for reconciliation. The assignment API
  # deactivates omitted rows rather than deleting their output-code history.
  saved_rules <- DBI::dbGetQuery(
    con,
    "SELECT r.rule_fx, r.rule_version, r.active, r.rule_args::text AS rule_args,
         r.companion_timeseries::text AS companion_timeseries,
         array_to_string(c.emits, ',') AS emits,
          c.argument_schema::text AS argument_schema, c.enabled
       FROM continuous.timeseries_qc_rules r
       JOIN public.qc_rule_capabilities c
         ON c.rule_fx = r.rule_fx AND c.rule_version = r.rule_version
       WHERE r.timeseries_id = $1
       ORDER BY r.run_order, r.timeseries_qc_rule_id",
    params = list(tsid)
  )
  if (is.null(assignments)) {
    saved_enabled <- if ("enabled" %in% names(saved_rules)) {
      !is.na(saved_rules$enabled) & saved_rules$enabled
    } else {
      rep(TRUE, nrow(saved_rules))
    }
    rules <- saved_rules[
      saved_rules$active & saved_enabled,
      ,
      drop = FALSE
    ]
  } else {
    rules <- qc_rule_assignments_normalize(con, tsid, assignments)
    rules <- rules[rules$active, , drop = FALSE]
    if (nrow(rules)) {
      rule_capabilities <- DBI::dbGetQuery(
        con,
        "SELECT rule_fx, rule_version,
           array_to_string(emits, ',') AS emits,
           argument_schema::text AS argument_schema
         FROM public.qc_rule_capabilities"
      )
      capability_index <- match(
        qc_rule_capability_key(rules$rule_fx, rules$rule_version),
        qc_rule_capability_key(
          rule_capabilities$rule_fx,
          rule_capabilities$rule_version
        )
      )
      if (anyNA(capability_index)) {
        stop("gradeContinuous: a preview assignment has no rule definition.")
      }
      rules$emits <- rule_capabilities$emits[capability_index]
      rules$argument_schema <-
        rule_capabilities$argument_schema[capability_index]
      rules <- rules[
        order(rules$run_order, seq_len(nrow(rules))),
        ,
        drop = FALSE
      ]
    }
  }
  if (nrow(rules) == 0 && nrow(saved_rules) == 0) {
    return(invisible(NULL))
  }
  # Saved arguments are materialized by setTimeseriesQCRules(). Read output
  # overrides without validating or executing inactive rule implementations.
  saved_args <- lapply(saved_rules$rule_args, function(a) {
    jsonlite::fromJSON(a, simplifyVector = FALSE)
  })
  rules$rule_key <- if (nrow(rules)) {
    qc_rule_capability_key(rules$rule_fx, rules$rule_version)
  } else {
    character()
  }
  unknown <- setdiff(rules$rule_key, names(qc_rule_registry))
  if (length(unknown)) {
    stop(
      "gradeContinuous: no R implementation for rule version(s) ",
      paste(unknown, collapse = ", ")
    )
  }
  schemas <- lapply(rules$argument_schema, qc_rule_schema_decode)
  args <- Map(qc_merge_rule_args, rules$argument_schema, rules$rule_args)
  companion_overrides <- Map(
    qc_rule_companion_overrides,
    rules$companion_timeseries,
    schemas,
    args
  )
  active_companions <- Map(qc_rule_active_companions, schemas, args)

  meta <- DBI::dbGetQuery(
    con,
    "SELECT t.location_id, COALESCE(t.timezone_daily_calc, 0) AS utc_offset, EXTRACT(EPOCH FROM t.record_rate::interval) AS rate_s, (SELECT min(datetime) FROM continuous.measurements_continuous WHERE timeseries_id = $1) AS first_dt, (SELECT max(datetime) FROM continuous.measurements_continuous WHERE timeseries_id = $1) AS last_dt FROM continuous.timeseries t WHERE t.timeseries_id = $1",
    params = list(tsid)
  )
  if (nrow(meta) == 0 || is.na(meta$first_dt)) {
    return(invisible(NULL))
  }

  # Windows. Rows in [from - ahead, from) were graded last time without the
  # readings that follow them, so they are graded again; rows at the very
  # end lack their own "ahead" context and stay unflagged until the next run
  # reaches back over them. Context is padded x2 for gaps, since the rules
  # count readings but the query selects by time.
  rate <- if (is.na(meta$rate_s) || meta$rate_s <= 0) 3600 else meta$rate_s
  ctx <- list(utc_offset = meta$utc_offset, rate_s = rate)
  win <- lapply(seq_len(nrow(rules)), function(i) {
    qc_rule_registry[[rules$rule_key[i]]]$window(args[[i]])
  })
  back <- 2 * rate * max(0, vapply(win, function(w) w$back, numeric(1)))
  ahead <- 2 * rate * max(0, vapply(win, function(w) w$ahead, numeric(1)))
  from <- if (is.null(from)) meta$first_dt else as.POSIXct(from, tz = "UTC")
  to <- if (is.null(to)) meta$last_dt else as.POSIXct(to, tz = "UTC")
  write_start <- max(from - ahead, meta$first_dt)
  eval_start <- write_start - back
  if (any(vapply(win, function(w) w$year_start, logical(1)))) {
    local_year <- format(write_start + ctx$utc_offset * 3600, "%Y", tz = "UTC")
    eval_start <- min(
      eval_start,
      as.POSIXct(paste0(local_year, "-01-01"), tz = "UTC") -
        ctx$utc_offset * 3600
    )
  }

  # Readings, corrected, plus companions joined on exact datetime.
  x <- qc_read_corrected(con, tsid, eval_start, to + ahead)
  companion_values <- list()
  warned_missing_roles <- character()
  codes <- vector("list", nrow(rules))
  for (i in seq_len(nrow(rules))) {
    rule <- qc_rule_registry[[rules$rule_key[i]]]
    rule_x <- x
    comp <- qc_resolve_companions(
      con,
      meta$location_id,
      tsid,
      active_companions[[i]],
      companion_overrides[[i]]
    )
    for (role in names(comp)) {
      companion_id <- comp[[role]]
      if (is.na(companion_id)) {
        if (!role %in% warned_missing_roles) {
          warning(
            "gradeContinuous: timeseries ",
            tsid,
            " has no '",
            role,
            "' companion; rules needing it see NA"
          )
          warned_missing_roles <- c(warned_missing_roles, role)
        }
        rule_x[[role]] <- rep(NA_real_, nrow(x))
        next
      }

      cache_key <- as.character(companion_id)
      if (is.null(companion_values[[cache_key]])) {
        cx <- qc_read_corrected(con, companion_id, eval_start, to + ahead)
        companion_values[[cache_key]] <- cx$value[
          match(as.numeric(x$datetime), as.numeric(cx$datetime))
        ]
      }
      rule_x[[role]] <- companion_values[[cache_key]]
    }
    codes[[i]] <- rule$fx(rule_x, args[[i]], ctx)
  }
  x$code <- if (length(codes)) {
    qc_combine_codes(codes)
  } else {
    rep(NA_character_, nrow(x))
  }
  w <- x[x$datetime >= write_start & x$datetime <= to, c("datetime", "code")]
  if (nrow(w) == 0) {
    return(invisible(NULL))
  }
  emitted <- unique(c(
    unlist(strsplit(saved_rules$emits, ",")),
    unlist(strsplit(as.character(rules$emits), ",")),
    unlist(lapply(c(saved_args, args), function(a) {
      c(a$qualifier, a$zero_qualifier)
    }))
  ))
  managed <- unique(c("UNK", "UNS", baseline, replace, emitted))
  qualifier_types <- DBI::dbGetQuery(
    con,
    "SELECT qualifier_type_id, qualifier_type_code FROM public.qualifier_types"
  )
  managed_ids <- qualifier_types$qualifier_type_id[
    qualifier_types$qualifier_type_code %in% managed
  ]

  # Existing upstream or user-supplied approvals take precedence. Missing
  # approvals are treated as unspecified, matching import defaults.
  approval_rows <- DBI::dbGetQuery(
    con,
    "SELECT a.start_dt, a.end_dt, a.no_source_update,
            t.approval_type_code
       FROM continuous.approvals a
       JOIN public.approval_types t USING (approval_type_id)
      WHERE a.timeseries_id = $1
        AND a.start_dt <= $3 AND a.end_dt >= $2
      ORDER BY a.start_dt",
    params = list(tsid, min(w$datetime), max(w$datetime))
  )
  approval_code <- rep("UNS", nrow(w))
  approval_protected <- rep(FALSE, nrow(w))
  if (nrow(approval_rows)) {
    for (i in seq_len(nrow(approval_rows))) {
      covered <- w$datetime >= approval_rows$start_dt[i] &
        w$datetime <= approval_rows$end_dt[i]
      approval_code[covered] <- approval_rows$approval_type_code[i]
      approval_protected[covered] <-
        approval_rows$no_source_update[i] %in% TRUE
    }
  }
  approval_codes_allowing_qc <- c(
    "UNK", "UNS", "N",
    if (reconcile_auto_screened) "S"
  )
  approval_allows_qc <- approval_code %in% approval_codes_allowing_qc
  w$code[!approval_allows_qc] <- NA_character_
  w$code[approval_allows_qc & is.na(w$code)] <- baseline

  # Placeholder qualifiers carry no additional information when another
  # qualifier already covers the reading, so do not write an overlapping
  # baseline UNS/UNK range.
  if (length(managed_ids)) {
    other_qualifiers <- DBI::dbGetQuery(
      con,
      "SELECT q.start_dt, q.end_dt
         FROM continuous.qualifiers q
        WHERE q.timeseries_id = $1
          AND NOT (q.qualifier_type_id = ANY($2::int[]))
          AND q.start_dt <= $4 AND q.end_dt >= $3",
      params = list(
        tsid,
        qc_pg_int_array(managed_ids),
        min(w$datetime),
        max(w$datetime)
      )
    )
    covered_by_other <- qc_covered(w$datetime, other_qualifiers)
    placeholder <- w$code %in% c("UNK", "UNS")
    w$code[placeholder & covered_by_other] <- NA_character_
  }

  if (write && full_regrade) {
    warning(
      "gradeContinuous: timeseries ",
      tsid,
      ": the WHOLE record is being regraded; unprotected ",
      paste(managed, collapse = "/"),
      " flags that no rule reproduces (e.g. manual ones) will become ",
      baseline
    )
  }

  # Protected readings keep their code (see details). Masking them to NA
  # before building ranges also breaks runs there, so no new range spans one.
  prot <- DBI::dbGetQuery(
    con,
    "SELECT q.start_dt, q.end_dt FROM continuous.qualifiers q JOIN public.qualifier_types t ON t.qualifier_type_id = q.qualifier_type_id WHERE q.timeseries_id = $1 AND q.no_source_update AND t.qualifier_type_code = ANY($2::text[]) AND q.start_dt <= $4 AND q.end_dt >= $3",
    params = list(
      tsid,
      qc_pg_text_array(managed),
      min(w$datetime),
      max(w$datetime)
    )
  )
  held <- qc_covered(w$datetime, prot) |
    !approval_allows_qc | approval_protected

  # A range writer reconciles everything between its first and last reading.
  # Keep separate runs around protected or higher approval ranges so neither
  # qualifiers nor approvals are rewritten through those periods, including
  # when a range falls between two sparse measurements.
  qc_candidates <- which(approval_allows_qc & !approval_protected)
  approval_barriers <- approval_rows[
    !approval_rows$approval_type_code %in% approval_codes_allowing_qc |
      approval_rows$no_source_update %in% TRUE,
    ,
    drop = FALSE
  ]
  approval_barrier_crossing <- rep(FALSE, max(0L, nrow(w) - 1L))
  if (nrow(approval_barriers) && length(approval_barrier_crossing)) {
    barrier_order <- order(approval_barriers$start_dt)
    barrier_starts <- as.numeric(
      approval_barriers$start_dt[barrier_order]
    )
    barrier_max_ends <- cummax(as.numeric(
      approval_barriers$end_dt[barrier_order]
    ))
    gap_end <- as.numeric(w$datetime[-1L])
    gap_start <- as.numeric(w$datetime[-nrow(w)])
    last_barrier <- findInterval(gap_end, barrier_starts, left.open = TRUE)
    has_barrier <- last_barrier > 0L
    approval_barrier_crossing[has_barrier] <-
      barrier_max_ends[last_barrier[has_barrier]] > gap_start[has_barrier]
  }
  qc_runs <- list()
  if (length(qc_candidates)) {
    run_id <- integer(length(qc_candidates))
    run_id[1L] <- 1L
    if (length(qc_candidates) > 1L) {
      for (i in 2:length(qc_candidates)) {
        previous <- qc_candidates[i - 1L]
        current <- qc_candidates[i]
        separated <- current != previous + 1L
        crosses_barrier <- !separated && approval_barrier_crossing[previous]
        run_id[i] <- run_id[i - 1L] + as.integer(separated || crosses_barrier)
      }
    }
    qc_runs <- split(qc_candidates, run_id)
  }
  range_parts <- lapply(qc_runs, function(indices) {
    run_ranges <- qc_runs_to_ranges(
      w$datetime[indices],
      ifelse(held[indices], NA_character_, w$code[indices])
    )
    run_ranges[!is.na(run_ranges$code), , drop = FALSE]
  })
  range_parts <- Filter(function(x) nrow(x) > 0L, range_parts)
  ranges <- if (length(range_parts)) {
    do.call(rbind, range_parts)
  } else {
    data.frame(
      code = character(),
      start_dt = as.POSIXct(character(), tz = "UTC"),
      end_dt = as.POSIXct(character(), tz = "UTC"),
      n = integer()
    )
  }

  approval_data <- data.frame(
    datetime = w$datetime,
    approval = approval_code,
    no_source_update = approval_protected,
    stringsAsFactors = FALSE
  )
  approval_updates <- if (nrow(rules)) {
    which(approval_allows_qc & !approval_protected & !qc_covered(w$datetime, prot))
  } else {
    integer()
  }
  if (length(approval_updates)) {
    approval_data$approval[approval_updates] <- "S"
  }

  if (!write) {
    return(invisible(list(
      ranges = ranges,
      readings = w,
      approvals = approval_data,
      window = c(
        eval_start = eval_start,
        write_start = write_start,
        write_end = to
      ),
      protected = sum(held)
    )))
  }
  needs_approval_write <- length(approval_updates) > 0L &&
    any(approval_code[approval_updates] != "S")
  if (nrow(ranges) == 0L && !needs_approval_write) {
    return(invisible(NULL))
  }

  # An approval-only update has no qualifier code to aggregate. Keep one
  # explicit summary row so its approval count is still available to callers.
  summary <- if (nrow(ranges)) {
    counts <- stats::aggregate(n ~ code, data = ranges, FUN = sum)
    counts$ranges <- as.vector(table(ranges$code)[counts$code])
    counts
  } else {
    data.frame(code = NA_character_, n = 0L, ranges = 0L)
  }
  summary$approval_readings <- length(approval_updates)

  active_transaction <- dbTransBegin(con)
  savepoint <- "grade_continuous_write"
  if (!active_transaction) {
    DBI::dbExecute(con, paste("SAVEPOINT", savepoint))
  }
  tryCatch(
    {
      for (indices in qc_runs) {
        run_ranges <- qc_runs_to_ranges(
          w$datetime[indices],
          ifelse(held[indices], NA_character_, w$code[indices])
        )
        run_ranges <- run_ranges[!is.na(run_ranges$code), , drop = FALSE]
        if (nrow(run_ranges)) {
          qc_write_ranges(con, tsid, run_ranges, managed = managed)
        }
      }
      if (needs_approval_write) {
        for (indices in qc_runs) {
          if (!any(approval_updates %in% indices)) {
            next
          }
          withCallingHandlers(
            adjust_approval(con, tsid, approval_data[indices, , drop = FALSE]),
            warning = function(warning) {
              stop(conditionMessage(warning), call. = FALSE)
            }
          )
        }
      }
      if (active_transaction) {
        DBI::dbExecute(con, "COMMIT")
      } else {
        DBI::dbExecute(con, paste("RELEASE SAVEPOINT", savepoint))
      }
    },
    error = function(e) {
      if (active_transaction) {
        DBI::dbExecute(con, "ROLLBACK")
      } else {
        DBI::dbExecute(con, paste("ROLLBACK TO SAVEPOINT", savepoint))
        DBI::dbExecute(con, paste("RELEASE SAVEPOINT", savepoint))
      }
      stop(e)
    }
  )

  invisible(summary)
}

#' Add a QC grade without replacing an assigned grade
#'
#' Future QC rule implementations that produce grades use this helper to fill
#' only `UNK` or `UNS` grade ranges where approval allows QC. Existing grades,
#' protected ranges, and readings with an upstream or reviewed approval remain
#' unchanged.
#'
#' @param con An open AquaCache connection.
#' @param timeseries_id One continuous timeseries id.
#' @param data A data frame with POSIXct `datetime` and proposed `grade` values
#'   (grade codes or IDs accepted by [adjust_grade()]).
#' @return Invisibly, the result of [adjust_grade()].
#' @noRd
#' @keywords internal
qc_add_grade <- function(con, timeseries_id, data) {
  tsid <- qc_rule_integer_id(timeseries_id, "timeseries_id")
  if (!is.data.frame(data) || !all(c("datetime", "grade") %in% names(data))) {
    stop("qc_add_grade: 'data' must contain 'datetime' and 'grade' columns.")
  }
  if (nrow(data) == 0L) {
    return(invisible(NULL))
  }
  if (!inherits(data$datetime, "POSIXct") || anyNA(data$datetime)) {
    stop("qc_add_grade: 'datetime' must be non-missing POSIXct values.")
  }

  approval_rows <- DBI::dbGetQuery(
    con,
    "SELECT a.start_dt, a.end_dt, a.no_source_update,
            t.approval_type_code
       FROM continuous.approvals a
       JOIN public.approval_types t USING (approval_type_id)
      WHERE a.timeseries_id = $1
        AND a.start_dt <= $3 AND a.end_dt >= $2
      ORDER BY a.start_dt",
    params = list(tsid, min(data$datetime), max(data$datetime))
  )
  approval_code <- rep("UNS", nrow(data))
  approval_protected <- rep(FALSE, nrow(data))
  if (nrow(approval_rows)) {
    for (i in seq_len(nrow(approval_rows))) {
      covered <- data$datetime >= approval_rows$start_dt[i] &
        data$datetime <= approval_rows$end_dt[i]
      approval_code[covered] <- approval_rows$approval_type_code[i]
      approval_protected[covered] <-
        approval_rows$no_source_update[i] %in% TRUE
    }
  }
  approval_allows_qc <- approval_code %in% c("UNK", "UNS", "N") &
    !approval_protected

  current <- DBI::dbGetQuery(
    con,
    "SELECT g.start_dt, g.end_dt, g.no_source_update,
            t.grade_type_code
       FROM continuous.grades g
       JOIN public.grade_types t USING (grade_type_id)
      WHERE g.timeseries_id = $1
        AND g.start_dt <= $3 AND g.end_dt >= $2
      ORDER BY g.start_dt",
    params = list(tsid, min(data$datetime), max(data$datetime))
  )
  current_code <- rep("UNS", nrow(data))
  protected <- rep(FALSE, nrow(data))
  if (nrow(current)) {
    for (i in seq_len(nrow(current))) {
      covered <- data$datetime >= current$start_dt[i] &
        data$datetime <= current$end_dt[i]
      current_code[covered] <- current$grade_type_code[i]
      protected[covered] <- current$no_source_update[i] %in% TRUE
    }
  }

  fillable <- current_code %in% c("UNK", "UNS") &
    !protected & approval_allows_qc
  data$grade <- as.character(data$grade)
  data$grade[!fillable] <- current_code[!fillable]
  data$no_source_update <- protected
  adjust_grade(con, tsid, data)
}

#' Reconcile calculated qualifier ranges in a grading window
#'
#' Only codes listed in `managed` are replaced. Stored ranges inside the
#' window are rebuilt; ranges straddling its edges are clipped, split, or joined
#' to matching new ranges. Protected ranges remain unchanged, and overlapping
#' new ranges of the same type are dropped with a warning. Codes outside the
#' managed set are left alone.
#'
#' A prior imported range can extend through later QC ranges of other types.
#' When clipping such a range, this helper finds the last reading before the
#' new window that is not covered by a later-starting managed range. This avoids
#' leaving a double-coded interval while preserving the imported range's
#' earlier valid portion.
#'
#' @param con AquaCache database connection.
#' @param tsid Timeseries ID.
#' @param ranges Data frame of proposed `code`, `start_dt`, `end_dt`, and `n`
#'   values from `qc_runs_to_ranges()`.
#' @param managed Character vector of qualifier codes whose stored ranges may
#'   be reconciled.
#' @return Invisibly, counts of deleted, updated, and inserted qualifier rows.
#' @noRd
#' @keywords internal
qc_write_ranges <- function(con, tsid, ranges, managed) {
  types <- DBI::dbGetQuery(
    con,
    "SELECT qualifier_type_id, qualifier_type_code FROM public.qualifier_types"
  )
  missing_types <- setdiff(managed, types$qualifier_type_code)
  if (length(missing_types)) {
    stop(
      "gradeContinuous: qualifier type(s) not in public.qualifier_types: ",
      paste(missing_types, collapse = ", ")
    )
  }
  ranges$qualifier_type_id <- types$qualifier_type_id[match(
    ranges$code,
    types$qualifier_type_code
  )]
  managed_ids <- types$qualifier_type_id[types$qualifier_type_code %in% managed]

  ws <- min(ranges$start_dt)
  we <- max(ranges$end_dt)
  prev_dt <- DBI::dbGetQuery(
    con,
    "SELECT max(datetime) AS d FROM continuous.measurements_continuous WHERE timeseries_id = $1 AND datetime < $2",
    params = list(tsid, ws)
  )$d
  next_dt <- DBI::dbGetQuery(
    con,
    "SELECT min(datetime) AS d FROM continuous.measurements_continuous WHERE timeseries_id = $1 AND datetime > $2",
    params = list(tsid, we)
  )$d
  lo <- if (is.na(prev_dt)) ws else prev_dt
  hi <- if (is.na(next_dt)) we else next_dt

  # Overlapping [lo, hi] also catches ranges that merely touch the window
  # (ending at prev_dt / starting at next_dt): those are merge candidates.
  existing <- DBI::dbGetQuery(
    con,
    "SELECT qualifier_id, qualifier_type_id, start_dt, end_dt, no_source_update FROM continuous.qualifiers WHERE timeseries_id = $1 AND qualifier_type_id = ANY($2::int[]) AND start_dt <= $4 AND end_dt >= $3",
    params = list(tsid, qc_pg_int_array(managed_ids), lo, hi)
  )
  prot <- existing[existing$no_source_update %in% TRUE, ]
  edit <- existing[!existing$no_source_update %in% TRUE, ]

  new <- data.frame(
    qualifier_id = NA_integer_,
    qualifier_type_id = ranges$qualifier_type_id,
    start_dt = ranges$start_dt,
    end_dt = ranges$end_dt,
    role = "new",
    stringsAsFactors = FALSE
  )
  if (nrow(prot)) {
    blocked <- vapply(
      seq_len(nrow(new)),
      function(i) {
        any(
          prot$qualifier_type_id == new$qualifier_type_id[i] &
            new$start_dt[i] <= prot$end_dt &
            new$end_dt[i] >= prot$start_dt
        )
      },
      logical(1)
    )
    if (any(blocked)) {
      warning(
        "gradeContinuous: timeseries ",
        tsid,
        ": ",
        sum(blocked),
        " new range(s) overlap protected qualifiers of the same type and were not written"
      )
    }
    new <- new[!blocked, ]
  }

  # Clip editable ranges to outside the window. The left part keeps the id;
  # a right part split off a range spanning the window is a new row.
  parts <- list()
  for (i in seq_len(nrow(edit))) {
    e <- edit[i, ]
    has_left <- e$start_dt < ws && !is.na(prev_dt) && prev_dt >= e$start_dt
    has_right <- e$end_dt > we && !is.na(next_dt) && next_dt <= e$end_dt
    if (has_left) {
      # Last reading before the window covered by no LATER-STARTING range of
      # another managed code (see the bridge note above): a bridge stretches
      # an older range over newer ones, never the reverse, so the newer
      # ranges keep their readings. Usually prev_dt itself.
      left_end <- DBI::dbGetQuery(
        con,
        "SELECT m.datetime FROM continuous.measurements_continuous m WHERE m.timeseries_id = $1 AND m.datetime >= $2 AND m.datetime < $3 AND NOT EXISTS (SELECT 1 FROM continuous.qualifiers q WHERE q.timeseries_id = $1 AND q.qualifier_type_id = ANY($4::int[]) AND q.start_dt > $2 AND m.datetime BETWEEN q.start_dt AND q.end_dt) ORDER BY m.datetime DESC LIMIT 1",
        params = list(
          tsid,
          e$start_dt,
          ws,
          qc_pg_int_array(setdiff(managed_ids, e$qualifier_type_id))
        )
      )$datetime
      has_left <- length(left_end) == 1 && !is.na(left_end)
    }
    if (has_left) {
      parts[[length(parts) + 1]] <- data.frame(
        qualifier_id = e$qualifier_id,
        qualifier_type_id = e$qualifier_type_id,
        start_dt = e$start_dt,
        end_dt = min(e$end_dt, left_end),
        role = "left"
      )
    }
    if (has_right) {
      parts[[length(parts) + 1]] <- data.frame(
        qualifier_id = if (has_left) NA_integer_ else e$qualifier_id,
        qualifier_type_id = e$qualifier_type_id,
        start_dt = max(e$start_dt, next_dt),
        end_dt = e$end_dt,
        role = "right"
      )
    }
  }
  final <- do.call(rbind, c(parts, list(new)))

  # Left seam: first new range + a left part of the same type ending at prev_dt.
  i <- which(final$role == "new" & final$start_dt == ws)
  j <- which(final$role == "left" & final$end_dt == prev_dt)
  j <- j[final$qualifier_type_id[j] %in% final$qualifier_type_id[i]]
  if (length(i) == 1 && length(j) == 1) {
    final$end_dt[j] <- final$end_dt[i]
    final$role[j] <- "merged"
    final <- final[-i, ]
  }
  # Right seam: the range ending at the last reading + a right part of the
  # same type starting at next_dt.
  i <- which(final$role %in% c("new", "merged") & final$end_dt == we)
  k <- which(final$role == "right" & final$start_dt == next_dt)
  k <- k[final$qualifier_type_id[k] %in% final$qualifier_type_id[i]]
  if (length(i) == 1 && length(k) == 1) {
    final$end_dt[i] <- final$end_dt[k]
    if (is.na(final$qualifier_id[i])) {
      final$qualifier_id[i] <- final$qualifier_id[k]
    }
    final <- final[-k, ]
  }

  del <- setdiff(edit$qualifier_id, final$qualifier_id)
  kept <- final[!is.na(final$qualifier_id), ]
  was <- edit[match(kept$qualifier_id, edit$qualifier_id), ]
  upd <- kept[kept$start_dt != was$start_dt | kept$end_dt != was$end_dt, ]
  ins <- final[is.na(final$qualifier_id), ]

  active <- dbTransBegin(con)
  tryCatch(
    {
      if (length(del)) {
        DBI::dbExecute(
          con,
          "DELETE FROM continuous.qualifiers WHERE qualifier_id = ANY($1::int[])",
          params = list(qc_pg_int_array(del))
        )
      }
      for (r in seq_len(nrow(upd))) {
        DBI::dbExecute(
          con,
          "UPDATE continuous.qualifiers SET start_dt = $2, end_dt = $3 WHERE qualifier_id = $1",
          params = list(upd$qualifier_id[r], upd$start_dt[r], upd$end_dt[r])
        )
      }
      if (nrow(ins)) {
        dbAppendTableRLS(
          con,
          "continuous.qualifiers",
          data.frame(
            timeseries_id = tsid,
            qualifier_type_id = ins$qualifier_type_id,
            start_dt = ins$start_dt,
            end_dt = ins$end_dt,
            no_source_update = FALSE
          )
        )
      }
      if (active) {
        # Fire deferred constraints now, while a failure can still be rolled
        # back cleanly. Only when we own the transaction.
        DBI::dbExecute(con, "SET CONSTRAINTS ALL IMMEDIATE")
        DBI::dbExecute(con, "COMMIT;")
      } else {
        # Keep unrelated import constraints deferred, but surface QC range
        # conflicts before grade_after_import releases its savepoint.
        qc_validate_qualifier_overlaps(con)
      }
    },
    error = function(e) {
      if (active) {
        DBI::dbExecute(con, "ROLLBACK;")
      }
      stop(e)
    }
  )
  invisible(list(
    deleted = length(del),
    updated = nrow(upd),
    inserted = nrow(ins)
  ))
}

#' Validate qualifier overlap constraints in a caller-owned transaction
#'
#' Temporarily checks the same-code exclusion and cross-code `UNK`/`UNS`
#' constraints, then restores both to deferred mode. Other constraints are
#' unchanged.
#' @param con AquaCache database connection with an open transaction.
#' @return Invisibly, `TRUE` after both constraints validate.
#' @noRd
#' @keywords internal
qc_validate_qualifier_overlaps <- function(con) {
  DBI::dbExecute(
    con,
    "SET CONSTRAINTS continuous.qualifiers_no_overlap,
       continuous.check_special_qualifier_overlap IMMEDIATE"
  )
  DBI::dbExecute(
    con,
    "SET CONSTRAINTS continuous.qualifiers_no_overlap,
       continuous.check_special_qualifier_overlap DEFERRED"
  )
  invisible(TRUE)
}

#' Grade newly imported data inside an import's transaction
#'
#' Runs [gradeContinuous()] under a SAVEPOINT in the caller's transaction, so
#' a grading failure undoes only the grading: the import still commits and
#' its new rows keep the qualifier they were imported with (UNK, or the
#' adapter's own, e.g. UNS from [downloadECCCwx()]). Warnings become
#' messages, because [synchronize_continuous()] turns warnings raised inside
#' its write step into errors. Series with no assigned rules cost one query.
#'
#' @param con A connection with an open transaction.
#' @param timeseries_id The timeseries just written to.
#' @param from Earliest datetime written or rewritten.
#' @return Invisibly, TRUE if grading succeeded.
#' @noRd
#' @keywords internal
grade_after_import <- function(con, timeseries_id, from) {
  qc_schema_available <- DBI::dbGetQuery(
    con,
    "SELECT to_regclass('public.qc_rule_capabilities') IS NOT NULL
              AND to_regclass('continuous.timeseries_qc_rules') IS NOT NULL
       AS available"
  )$available[[1L]]
  if (!isTRUE(qc_schema_available)) {
    return(invisible(FALSE))
  }

  DBI::dbExecute(con, "SAVEPOINT grade_after_import")
  ok <- tryCatch(
    {
      withCallingHandlers(
        gradeContinuous(con, as.integer(timeseries_id), from = from),
        warning = function(w) {
          message(conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
      TRUE
    },
    error = function(e) {
      DBI::dbExecute(con, "ROLLBACK TO SAVEPOINT grade_after_import")
      message(
        "gradeContinuous failed for timeseries ",
        timeseries_id,
        "; its new data keep their imported qualifier: ",
        conditionMessage(e)
      )
      FALSE
    }
  )
  DBI::dbExecute(con, "RELEASE SAVEPOINT grade_after_import")
  invisible(ok)
}

#' Run the post-import QC hook only when datetimes were written
#'
#' Skips empty input before calculating the earliest datetime.
#' @param con AquaCache database connection.
#' @param timeseries_id Timeseries ID just written to.
#' @param datetimes Datetimes written or replaced by the import.
#' @return Invisibly, `FALSE` for no data or unavailable QC schema; otherwise
#'   the result of `grade_after_import()`.
#' @noRd
#' @keywords internal
grade_after_import_if_data <- function(con, timeseries_id, datetimes) {
  datetimes <- datetimes[!is.na(datetimes)]
  if (!length(datetimes)) {
    return(invisible(FALSE))
  }
  grade_after_import(con, timeseries_id, from = min(datetimes))
}
