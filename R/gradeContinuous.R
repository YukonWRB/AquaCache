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

# Companion role -> parameter, used when rule_args gives no explicit id.
qc_companion_parameters <- c(air_temp = "temperature, air",
                             wind_speed = "velocity, wind")

# --- Rolling helpers (centred, by READING not by time, NA if any reading in
# the window is NA or the window runs off either end - same as zoo with
# fill = NA, which flag_data() used) -----------------------------------------

qc_shift <- function(x, s) {
  n <- length(x)
  if (abs(s) >= n) return(rep(NA_real_, n))
  if (s >= 0) c(x[(s + 1):n], rep(NA_real_, s)) else c(rep(NA_real_, -s), x[1:(n + s)])
}

qc_window_matrix <- function(x, k) {
  lo <- (k - 1) %/% 2
  vapply(seq_len(k) - 1L - lo, function(s) qc_shift(x, s), numeric(length(x)))
}

# Direct deviations, not a running sum of squares: thresholds go down to
# sd < 0.001 and cumulative sums would lose that to cancellation.
qc_roll_sd <- function(x, k) {
  m <- matrix(qc_window_matrix(x, k), ncol = k)
  mu <- rowMeans(m)
  sqrt(rowSums((m - mu)^2) / (k - 1))
}

# Running sums are fine for a mean, and k = 720 is too wide for a matrix.
# align = "center" as zoo's default; "right" = trailing (the reading and the
# k - 1 before it), as zoo's align = "right".
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

qc_rain_cold <- function(x, a, ctx) {
  hit <- !is.na(x$value) & x$value > 0 & !is.na(x$air_temp) & x$air_temp < a$max_air_temp
  ifelse(hit, a$qualifier, NA_character_)
}

# Section 10 split for a stuck 0 m/s (zero_split = TRUE, wind speed only):
# below zero_max_air_temp or no air temp -> SUS (frozen propeller OR calm cold
# night); at or above it -> no flag (calm is real). Stuck at any other value
# -> ERROR.
qc_stuck_sensor <- function(x, a, ctx) {
  s <- qc_roll_sd(x$value, a$width)
  stuck <- !is.na(s) & s < a$min_sd & qc_window_span_ok(x$datetime, a$width, "center", ctx$rate_s)
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

qc_dir_calm <- function(x, a, ctx) {
  hit <- !is.na(x$value) & !is.na(x$wind_speed) & x$wind_speed <= a$max_wind_speed
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
  cold_mean[!qc_window_span_ok(x$datetime, a$cold_window, align, ctx$rate_s)] <- NA
  thaw <- rain & warm & !is.na(cold_mean) & cold_mean < 0
  ifelse(early | thaw, a$qualifier, NA_character_)
}

# window(): how many READINGS of context a rule needs before (back) and after
# (ahead) a reading, and whether it needs the calendar year from January 1.
qc_rule_registry <- list(
  qc_rain_cold = list(fx = qc_rain_cold, window = function(a) list(back = 0, ahead = 0, year_start = FALSE)),
  qc_stuck_sensor = list(fx = qc_stuck_sensor, window = function(a) list(back = (a$width - 1) %/% 2, ahead = a$width %/% 2, year_start = FALSE)),
  qc_dir_calm = list(fx = qc_dir_calm, window = function(a) list(back = 0, ahead = 0, year_start = FALSE)),
  qc_snowmelt_tips = list(fx = qc_snowmelt_tips, window = function(a) if (identical(a$cold_align, "centred")) list(back = (a$cold_window - 1) %/% 2, ahead = a$cold_window %/% 2, year_start = TRUE) else list(back = a$cold_window - 1, ahead = 0, year_start = TRUE))
)

# --- Plumbing ----------------------------------------------------------------

qc_pg_int_array <- function(x) paste0("{", paste(as.integer(x), collapse = ","), "}")

# Defaults come from the catalogue's argument_schema - one source of truth,
# the one YGwater would build its form from - overridden by rule_args.
qc_merge_rule_args <- function(schema_json, args_json) {
  schema <- jsonlite::fromJSON(schema_json, simplifyVector = FALSE)
  defaults <- stats::setNames(lapply(schema$arguments, `[[`, "default"), vapply(schema$arguments, `[[`, character(1), "name"))
  given <- if (is.na(args_json)) list() else jsonlite::fromJSON(args_json, simplifyVector = FALSE)
  utils::modifyList(defaults, given)
}

# Explicit rule_args$companions$<role> wins; otherwise the same location's
# series of the role's parameter with the lowest sensor_priority.
qc_resolve_companions <- function(con, location_id, roles, args) {
  out <- list()
  for (role in roles) {
    explicit <- unlist(lapply(args, function(a) a$companions[[role]]))
    if (length(explicit)) {
      out[[role]] <- as.integer(explicit[1])
      next
    }
    if (!role %in% names(qc_companion_parameters)) stop("gradeContinuous: no parameter known for companion role '", role, "'")
    id <- DBI::dbGetQuery(con, "SELECT t.timeseries_id FROM continuous.timeseries t JOIN public.parameters p ON p.parameter_id = t.parameter_id WHERE t.location_id = $1 AND p.param_name = $2 ORDER BY t.sensor_priority LIMIT 1", params = list(location_id, qc_companion_parameters[[role]]))$timeseries_id
    out[[role]] <- if (length(id)) id else NA_integer_
  }
  out
}

# Since patch 53 this is a set-returning FUNCTION (timeseries, from, to; both
# ends inclusive), not the old view. Trimmed readings come back with
# value_corrected NULL.
qc_read_corrected <- function(con, tsid, from, to) {
  DBI::dbGetQuery(con, "SELECT datetime, value_corrected::double precision AS value FROM continuous.measurements_continuous_corrected($1::integer, $2::timestamptz, $3::timestamptz) ORDER BY datetime", params = list(tsid, from, to))
}

# Fold rule outputs into one code per reading by qc_severity; codes not in
# qc_severity (custom qualifier arguments) rank last.
qc_combine_codes <- function(codes) {
  rank_of <- function(v) ifelse(is.na(v), NA_integer_, match(v, qc_severity, nomatch = length(qc_severity) + 1L))
  out <- codes[[1]]
  for (v in codes[-1]) {
    take <- !is.na(v) & (is.na(out) | rank_of(v) < rank_of(out))
    out[take] <- v[take]
  }
  out
}

# A run of consecutive readings with one code is one range, first to last
# reading inclusive (the convention of build_attribute_segments()).
qc_runs_to_ranges <- function(datetime, code) {
  r <- rle(code)
  end <- cumsum(r$lengths)
  start <- end - r$lengths + 1L
  data.frame(code = r$values, start_dt = datetime[start], end_dt = datetime[end], n = r$lengths, stringsAsFactors = FALSE)
}

# --- Main --------------------------------------------------------------------

#' Apply configured QC rules to a continuous timeseries
#'
#' @description
#' Applies the quality-control rules assigned to a timeseries in
#' `continuous.timeseries_qc_rules` and writes the resulting qualifiers to
#' `continuous.qualifiers`. Readings no rule flags get `baseline` (UNS).
#' Called after new data are imported (see [getNewContinuous()] and
#' [synchronize_continuous()]); can also be run directly, e.g. with
#' `write = FALSE` to preview.
#'
#' Rules read CORRECTED values, so readings hidden by a trim correction are
#' treated as missing. Each reading gets at most one rule qualifier, by
#' severity: ERROR > SSM > PSM > CT > SUS.
#'
#' @details
#' **Which ranges are replaced.** Only stored qualifiers of the codes the
#' grader manages (`baseline`, `replace` and the codes its rules emit), inside
#' the graded window, and only if not protected (`no_source_update = TRUE`).
#' Codes it does not manage are left alone. Ranges it writes are
#' source-managed (`no_source_update = FALSE`), so later runs can replace
#' them; protected ranges are never changed, and new ranges of the same type
#' overlapping one are skipped with a warning.
#'
#' **Cutoff.** Nothing is written before the latest `apply_from` of the
#' series' active rules, even when `from` is NULL. Rules only reproduce what
#' they detect, so a flag set some other way (by hand, or by a legacy
#' system) would otherwise become `baseline` on a regrade. With no
#' `apply_from` and `from = NULL` the whole record is regraded, with a
#' warning.
#'
#' **Windows.** Rules that look at neighbouring readings are given enough
#' context either side of the graded readings, and readings near the start
#' of the window are re-graded once later readings exist. A rolling window
#' that spans a gap in the record gives no result.
#'
#' @param con A connection with write access to `continuous.qualifiers`.
#' @param timeseries_id One timeseries_id.
#' @param from,to Datetimes (UTC) of the readings to grade, usually the new
#'   data. NULL means the start / end of the record.
#' @param baseline Qualifier code for readings no rule flags.
#' @param replace Other qualifier codes the grader may replace inside its
#'   window. [getNewContinuous()] writes UNK on new rows when the source
#'   supplies no qualifier, so new readings arrive as UNK and leave as
#'   `baseline` or a rule's code.
#' @param write FALSE returns what would be written, without writing.
#'
#' @return Invisibly. With `write = FALSE`, a list: `ranges` (what would be
#'   written, after the cutoff), `readings` (one code per reading over the
#'   whole window, for comparison with stored qualifiers), `window` and
#'   `cutoff`. With `write = TRUE`, a data.frame of readings and ranges per
#'   code. NULL if the series has no active rules or nothing to write.
#' @seealso [adjust_qualifier()]
#' @export
gradeContinuous <- function(con, timeseries_id, from = NULL, to = NULL, baseline = "UNS", replace = "UNK", write = TRUE) {
  tsid <- as.integer(timeseries_id)
  stopifnot(length(tsid) == 1, !is.na(tsid))
  full_regrade <- is.null(from)

  rules <- DBI::dbGetQuery(con, "SELECT r.rule_fx, r.rule_args::text AS rule_args, r.apply_from, array_to_string(c.emits, ',') AS emits, array_to_string(c.companion_roles, ',') AS roles, c.argument_schema::text AS argument_schema FROM continuous.timeseries_qc_rules r JOIN public.qc_rule_capabilities c ON c.rule_fx = r.rule_fx WHERE r.timeseries_id = $1 AND r.active AND c.enabled ORDER BY r.run_order", params = list(tsid))
  if (nrow(rules) == 0) return(invisible(NULL))
  unknown <- setdiff(rules$rule_fx, names(qc_rule_registry))
  if (length(unknown)) stop("gradeContinuous: no R implementation for rule(s) ", paste(unknown, collapse = ", "))
  args <- unname(Map(qc_merge_rule_args, rules$argument_schema, rules$rule_args))

  meta <- DBI::dbGetQuery(con, "SELECT t.location_id, COALESCE(t.timezone_daily_calc, 0) AS utc_offset, EXTRACT(EPOCH FROM t.record_rate::interval) AS rate_s, (SELECT min(datetime) FROM continuous.measurements_continuous WHERE timeseries_id = $1) AS first_dt, (SELECT max(datetime) FROM continuous.measurements_continuous WHERE timeseries_id = $1) AS last_dt FROM continuous.timeseries t WHERE t.timeseries_id = $1", params = list(tsid))
  if (nrow(meta) == 0 || is.na(meta$first_dt)) return(invisible(NULL))

  # Windows. Rows in [from - ahead, from) were graded last time without the
  # readings that follow them, so they are graded again; rows at the very
  # end lack their own "ahead" context and stay unflagged until the next run
  # reaches back over them. Context is padded x2 for gaps, since the rules
  # count readings but the query selects by time.
  rate <- if (is.na(meta$rate_s) || meta$rate_s <= 0) 3600 else meta$rate_s
  ctx <- list(utc_offset = meta$utc_offset, rate_s = rate)
  win <- lapply(seq_len(nrow(rules)), function(i) qc_rule_registry[[rules$rule_fx[i]]]$window(args[[i]]))
  back <- 2 * rate * max(vapply(win, function(w) w$back, numeric(1)))
  ahead <- 2 * rate * max(vapply(win, function(w) w$ahead, numeric(1)))
  from <- if (is.null(from)) meta$first_dt else as.POSIXct(from, tz = "UTC")
  to <- if (is.null(to)) meta$last_dt else as.POSIXct(to, tz = "UTC")
  write_start <- max(from - ahead, meta$first_dt)
  eval_start <- write_start - back
  if (any(vapply(win, function(w) w$year_start, logical(1)))) {
    local_year <- format(write_start + ctx$utc_offset * 3600, "%Y", tz = "UTC")
    eval_start <- min(eval_start, as.POSIXct(paste0(local_year, "-01-01"), tz = "UTC") - ctx$utc_offset * 3600)
  }

  # Readings, corrected, plus companions joined on exact datetime.
  x <- qc_read_corrected(con, tsid, eval_start, to + ahead)
  roles <- unique(unlist(strsplit(rules$roles[nzchar(rules$roles)], ",")))
  comp <- qc_resolve_companions(con, meta$location_id, roles, args)
  for (role in names(comp)) {
    if (is.na(comp[[role]])) {
      warning("gradeContinuous: timeseries ", tsid, " has no '", role, "' companion; rules needing it see NA")
      x[[role]] <- NA_real_
      next
    }
    cx <- qc_read_corrected(con, comp[[role]], eval_start, to + ahead)
    x[[role]] <- cx$value[match(as.numeric(x$datetime), as.numeric(cx$datetime))]
  }

  codes <- lapply(seq_len(nrow(rules)), function(i) qc_rule_registry[[rules$rule_fx[i]]]$fx(x, args[[i]], ctx))
  x$code <- qc_combine_codes(codes)
  w <- x[x$datetime >= write_start & x$datetime <= to, c("datetime", "code")]
  if (nrow(w) == 0) return(invisible(NULL))
  w$code[is.na(w$code)] <- baseline

  # The cutoff: never write before the latest apply_from (see details).
  cutoff <- if (all(is.na(rules$apply_from))) NULL else max(rules$apply_from, na.rm = TRUE)
  if (is.null(cutoff) && write && full_regrade) warning("gradeContinuous: timeseries ", tsid, " has no apply_from, so the WHOLE record is being regraded; stored ", paste(unique(unlist(strsplit(rules$emits, ","))), collapse = "/"), " flags that no rule reproduces (e.g. manual ones) will become ", baseline)
  ww <- if (is.null(cutoff)) w else w[w$datetime >= cutoff, ]
  ranges <- if (nrow(ww)) qc_runs_to_ranges(ww$datetime, ww$code) else qc_runs_to_ranges(ww$datetime[0], character(0))

  if (!write) return(invisible(list(ranges = ranges, readings = w, window = c(eval_start = eval_start, write_start = write_start, write_end = to), cutoff = cutoff)))
  if (nrow(ranges) == 0) return(invisible(NULL))   # everything is before the cutoff

  emitted <- unique(c(unlist(strsplit(rules$emits, ",")), unlist(lapply(args, function(a) c(a$qualifier, a$zero_qualifier)))))
  qc_write_ranges(con, tsid, ranges, managed = unique(c(baseline, replace, emitted)))

  summary <- stats::aggregate(n ~ code, data = ranges, FUN = sum)
  summary$ranges <- as.vector(table(ranges$code)[summary$code])
  invisible(summary)
}

# Reconcile new ranges with stored ones of the MANAGED codes only, inside
# [first, last reading of the window]:
#   - stored ranges inside the window are deleted;
#   - ranges straddling an edge are clipped to the reading just outside it
#     (split in two if they span the whole window);
#   - at each edge, a clipped range and the new range touching it are merged
#     when they carry the same code, so a daily run extends yesterday's UNS
#     instead of starting a new one;
#   - PROTECTED ranges (no_source_update = TRUE - a person set them) are
#     never changed, and new ranges of the same type overlapping one are
#     dropped with a warning;
#   - a range reaching the window from before is cut at the last reading
#     before the window that no later-starting range of another managed code
#     covers, not simply at the reading before the window. When no stored range of the import's code
#     (UNS from downloadECCCwx, else UNK) overlaps the new rows, the import's
#     adjust_qualifier() stretches the LATEST range of that code forward to
#     them (bridge_latest_extension), straight across later ranges of other
#     codes, which would leave those readings double-coded. Shortening a
#     bridged range undoes the import rather than regrading, so it may reach
#     before apply_from.
# Codes the grader does not manage (e.g. a manual GW or a source qualifier)
# are left alone; the overlap trigger only checks within a type.
qc_write_ranges <- function(con, tsid, ranges, managed) {
  types <- DBI::dbGetQuery(con, "SELECT qualifier_type_id, qualifier_type_code FROM public.qualifier_types")
  missing_types <- setdiff(managed, types$qualifier_type_code)
  if (length(missing_types)) stop("gradeContinuous: qualifier type(s) not in public.qualifier_types: ", paste(missing_types, collapse = ", "))
  ranges$qualifier_type_id <- types$qualifier_type_id[match(ranges$code, types$qualifier_type_code)]
  managed_ids <- types$qualifier_type_id[types$qualifier_type_code %in% managed]

  ws <- min(ranges$start_dt)
  we <- max(ranges$end_dt)
  prev_dt <- DBI::dbGetQuery(con, "SELECT max(datetime) AS d FROM continuous.measurements_continuous WHERE timeseries_id = $1 AND datetime < $2", params = list(tsid, ws))$d
  next_dt <- DBI::dbGetQuery(con, "SELECT min(datetime) AS d FROM continuous.measurements_continuous WHERE timeseries_id = $1 AND datetime > $2", params = list(tsid, we))$d
  lo <- if (is.na(prev_dt)) ws else prev_dt
  hi <- if (is.na(next_dt)) we else next_dt

  # Overlapping [lo, hi] also catches ranges that merely touch the window
  # (ending at prev_dt / starting at next_dt): those are merge candidates.
  existing <- DBI::dbGetQuery(con, "SELECT qualifier_id, qualifier_type_id, start_dt, end_dt, no_source_update FROM continuous.qualifiers WHERE timeseries_id = $1 AND qualifier_type_id = ANY($2::int[]) AND start_dt <= $4 AND end_dt >= $3", params = list(tsid, qc_pg_int_array(managed_ids), lo, hi))
  prot <- existing[existing$no_source_update %in% TRUE, ]
  edit <- existing[!existing$no_source_update %in% TRUE, ]

  new <- data.frame(qualifier_id = NA_integer_, qualifier_type_id = ranges$qualifier_type_id, start_dt = ranges$start_dt, end_dt = ranges$end_dt, role = "new", stringsAsFactors = FALSE)
  if (nrow(prot)) {
    blocked <- vapply(seq_len(nrow(new)), function(i) any(prot$qualifier_type_id == new$qualifier_type_id[i] & new$start_dt[i] <= prot$end_dt & new$end_dt[i] >= prot$start_dt), logical(1))
    if (any(blocked)) warning("gradeContinuous: timeseries ", tsid, ": ", sum(blocked), " new range(s) overlap protected qualifiers of the same type and were not written")
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
      left_end <- DBI::dbGetQuery(con, "SELECT m.datetime FROM continuous.measurements_continuous m WHERE m.timeseries_id = $1 AND m.datetime >= $2 AND m.datetime < $3 AND NOT EXISTS (SELECT 1 FROM continuous.qualifiers q WHERE q.timeseries_id = $1 AND q.qualifier_type_id = ANY($4::int[]) AND q.start_dt > $2 AND m.datetime BETWEEN q.start_dt AND q.end_dt) ORDER BY m.datetime DESC LIMIT 1", params = list(tsid, e$start_dt, ws, qc_pg_int_array(setdiff(managed_ids, e$qualifier_type_id))))$datetime
      has_left <- length(left_end) == 1 && !is.na(left_end)
    }
    if (has_left) parts[[length(parts) + 1]] <- data.frame(qualifier_id = e$qualifier_id, qualifier_type_id = e$qualifier_type_id, start_dt = e$start_dt, end_dt = min(e$end_dt, left_end), role = "left")
    if (has_right) parts[[length(parts) + 1]] <- data.frame(qualifier_id = if (has_left) NA_integer_ else e$qualifier_id, qualifier_type_id = e$qualifier_type_id, start_dt = max(e$start_dt, next_dt), end_dt = e$end_dt, role = "right")
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
    if (is.na(final$qualifier_id[i])) final$qualifier_id[i] <- final$qualifier_id[k]
    final <- final[-k, ]
  }

  del <- setdiff(edit$qualifier_id, final$qualifier_id)
  kept <- final[!is.na(final$qualifier_id), ]
  was <- edit[match(kept$qualifier_id, edit$qualifier_id), ]
  upd <- kept[kept$start_dt != was$start_dt | kept$end_dt != was$end_dt, ]
  ins <- final[is.na(final$qualifier_id), ]

  active <- dbTransBegin(con)
  tryCatch({
    if (length(del)) DBI::dbExecute(con, "DELETE FROM continuous.qualifiers WHERE qualifier_id = ANY($1::int[])", params = list(qc_pg_int_array(del)))
    for (r in seq_len(nrow(upd))) DBI::dbExecute(con, "UPDATE continuous.qualifiers SET start_dt = $2, end_dt = $3 WHERE qualifier_id = $1", params = list(upd$qualifier_id[r], upd$start_dt[r], upd$end_dt[r]))
    if (nrow(ins)) dbAppendTableRLS(con, "continuous.qualifiers", data.frame(timeseries_id = tsid, qualifier_type_id = ins$qualifier_type_id, start_dt = ins$start_dt, end_dt = ins$end_dt, no_source_update = FALSE))
    if (active) {
      # Fire the deferred overlap triggers now, while a failure can still be
      # rolled back cleanly. Only when we own the transaction: inside an
      # import, the import's own writes may rely on deferral.
      DBI::dbExecute(con, "SET CONSTRAINTS ALL IMMEDIATE")
      DBI::dbExecute(con, "COMMIT;")
    }
  }, error = function(e) {
    if (active) DBI::dbExecute(con, "ROLLBACK;")
    stop(e)
  })
  invisible(list(deleted = length(del), updated = nrow(upd), inserted = nrow(ins)))
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
  DBI::dbExecute(con, "SAVEPOINT grade_after_import")
  ok <- tryCatch({
    withCallingHandlers(
      gradeContinuous(con, timeseries_id, from = from),
      warning = function(w) {
        message(conditionMessage(w))
        invokeRestart("muffleWarning")
      })
    TRUE
  }, error = function(e) {
    DBI::dbExecute(con, "ROLLBACK TO SAVEPOINT grade_after_import")
    message("gradeContinuous failed for timeseries ", timeseries_id, "; its new data keep their imported qualifier: ", conditionMessage(e))
    FALSE
  })
  DBI::dbExecute(con, "RELEASE SAVEPOINT grade_after_import")
  invisible(ok)
}
