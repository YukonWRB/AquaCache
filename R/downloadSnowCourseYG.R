#' Fetch Yukon Government snow-course samples and component results
#'
#' @description
#' Retrieves snow-course surveys from the Yukon Government snow database in
#' the discrete source-adapter format used by [getNewDiscrete()] and
#' [synchronize_discrete()]. Each reportable SWE or snow-depth result is
#' returned as a canonical result backed by all of its source observations,
#' including observations excluded from the mean.
#'
#' The function only reads the snow database and returns data. AquaCache
#' insertion and replacement are handled transactionally by the calling
#' ingestion function.
#'
#' Every returned canonical result uses the `mean` aggregation type. Source
#' `exclude_flag` values control `included_in_aggregate`; an exclusion without
#' a source note receives an explicit fallback note required by AquaCache.
#' Standard surveys declare an expected count of 10 observations. Missing
#' component values are retained and configured to be ignored by the
#' aggregation calculation.
#'
#' @param location Snow-database location code. It should match
#'   `public.locations.location_code` in AquaCache; `alias` is also accepted.
#' @param start_datetime Earliest survey datetime to retrieve, as a `Date`,
#'   `POSIXct`, or coercible character value. The lower boundary is exclusive.
#' @param end_datetime Latest survey datetime to retrieve. A `Date` includes
#'   that whole day; the upper survey-date boundary remains exclusive for
#'   compatibility with the discrete ingestion framework.
#' @param old_loc Optional earlier snow-database location whose surveys should
#'   be used to extend `location`. Surveys at `location` take precedence on
#'   overlapping dates. When parallel observations exist, raw observations
#'   from `old_loc` are retained as components and a parameter-specific
#'   multiplier is recorded in the aggregation calculation arguments.
#' @param adjust_start,adjust_end Optional bounds on parallel surveys used to
#'   calculate `old_loc` multipliers. Setting both to the same date disables
#'   adjustment.
#' @param share_with Roles with which newly inserted samples should be shared.
#'   Existing sample visibility is not changed by synchronization.
#' @param con AquaCache connection used to resolve reference identifiers. A
#'   connection is opened and closed automatically when `NULL`.
#' @param snowCon Connection to the Yukon snow database. A connection is opened
#'   and closed automatically when `NULL`.
#'
#' @return A list with one element per survey. Each element contains `sample`,
#'   `results`, `result_aggregations`, and `result_components` data frames.
#'   Surveys without a calculable SWE or depth result are omitted because the
#'   discrete source-adapter contract requires at least one result. An empty
#'   list is returned when no qualifying surveys are available.
#'
#' @export
downloadSnowCourseYG <- function(
  location,
  start_datetime,
  end_datetime = Sys.time(),
  old_loc = NULL,
  adjust_start = NULL,
  adjust_end = NULL,
  share_with = "yg_reader_group",
  con = NULL,
  snowCon = NULL
) {
  coerce_boundary <- function(value, argument, end_of_day = FALSE) {
    converted <- tryCatch(
      {
        if (inherits(value, "POSIXct")) {
          as.POSIXct(value, tz = "UTC")
        } else if (inherits(value, "Date")) {
          as.POSIXct(value, tz = "UTC")
        } else if (is.character(value) && length(value) == 1L) {
          as.POSIXct(value, tz = "UTC")
        } else {
          as.POSIXct(NA, tz = "UTC")
        }
      },
      error = function(e) as.POSIXct(NA, tz = "UTC")
    )
    if (length(converted) != 1L || is.na(converted)) {
      stop("Failed to convert parameter ", argument, " to POSIXct.")
    }
    if (
      end_of_day &&
        (inherits(value, "Date") ||
          (is.character(value) && nchar(value) == 10L))
    ) {
      converted <- converted + 24 * 60 * 60 - 1
    }
    converted
  }

  start_datetime <- coerce_boundary(start_datetime, "start_datetime")
  end_datetime <- coerce_boundary(
    end_datetime,
    "end_datetime",
    end_of_day = TRUE
  )
  if (end_datetime <= start_datetime) {
    stop("end_datetime must be later than start_datetime.")
  }
  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
  }
  if (is.null(snowCon)) {
    snowCon <- snowConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(snowCon), add = TRUE)
  }
  DBI::dbExecute(con, "SET timezone = 'UTC'")
  DBI::dbExecute(snowCon, "SET timezone = 'UTC'")

  reference_ids <- DBI::dbGetQuery(
    con,
    "SELECT
       (SELECT parameter_id FROM public.parameters
        WHERE param_name = 'snow water equivalent') AS swe_parameter_id,
       (SELECT parameter_id FROM public.parameters
        WHERE param_name = 'snow depth') AS depth_parameter_id,
       (SELECT media_id FROM public.media_types
        WHERE media_type = 'snow') AS media_id,
       (SELECT sample_type_id FROM discrete.sample_types
        WHERE lower(sample_type) =
          'sample-field msr/obs - no lab results expected') AS sample_type_id,
       (SELECT organization_id FROM public.organizations
        WHERE lower(name) =
          'yukon government department of environment, water science and stewardship'
        ORDER BY organization_id LIMIT 1) AS owner_id,
       (SELECT organization_id FROM public.organizations
        WHERE lower(name) LIKE
          'yukon government department of environment, water science and stewardship%'
        ORDER BY organization_id LIMIT 1) AS contributor_id,
       (SELECT collection_method_id FROM discrete.collection_methods
        WHERE lower(collection_method) = 'observation') AS collection_method_id,
       (SELECT result_value_type_id FROM discrete.result_value_types
        WHERE lower(result_value_type) = 'estimated') AS estimated_result_id,
       (SELECT result_value_type_id FROM discrete.result_value_types
        WHERE lower(result_value_type) = 'actual') AS actual_result_id,
       (SELECT result_type_id FROM discrete.result_types
        WHERE lower(result_type) = 'field') AS field_result_type_id,
       (SELECT protocol_id FROM discrete.protocols_methods
        WHERE lower(protocol_name) = 'bc snow survey sampling guide')
          AS protocol_method_id"
  )
  if (any(is.na(reference_ids[1L, ]))) {
    missing_references <- names(reference_ids)[is.na(reference_ids[1L, ])]
    stop(
      "AquaCache is missing snow-course reference values: ",
      paste(missing_references, collapse = ", "),
      "."
    )
  }

  aqua_location <- DBI::dbGetQuery(
    con,
    "SELECT location_id
     FROM public.locations
     WHERE lower(location_code) = $1 OR lower(alias) = $1",
    params = list(tolower(location))
  )
  if (nrow(aqua_location) != 1L) {
    stop(
      "location must match exactly one AquaCache location_code or alias: ",
      location,
      "."
    )
  }

  fetch_surveys <- function(
    source_location,
    lower_datetime = NULL,
    upper_datetime
  ) {
    lower_clause <- if (is.null(lower_datetime)) {
      ""
    } else {
      "AND ((s.survey_date::timestamp + interval '19 hours')
        AT TIME ZONE 'UTC') > $3::timestamptz"
    }
    query <- paste0(
      "SELECT
         s.survey_id AS import_source_id,
         s.location AS source_location,
         ((s.target_date::timestamp + interval '19 hours')
           AT TIME ZONE 'UTC') AS target_datetime,
         ((s.survey_date::timestamp + interval '19 hours')
           AT TIME ZONE 'UTC') AS datetime,
         s.survey_date,
         NULLIF(btrim(s.notes), 'NA') AS note,
         s.method,
         m.measurement_id,
         (m.sample_datetime AT TIME ZONE 'Etc/GMT+7')
           AS observation_datetime,
         m.estimate_flag,
         m.exclude_flag,
         m.swe,
         m.depth,
         NULLIF(NULLIF(btrim(m.notes), ''), 'NA') AS component_note
       FROM public.surveys s
       LEFT JOIN public.measurements m USING (survey_id)
       WHERE s.location = $1
         AND ((s.survey_date::timestamp + interval '19 hours')
           AT TIME ZONE 'UTC') < $2::timestamptz
         ",
      lower_clause,
      "
       ORDER BY s.survey_date, s.survey_id, m.measurement_id"
    )
    params <- list(source_location, upper_datetime)
    if (!is.null(lower_datetime)) {
      params <- c(params, list(lower_datetime))
    }
    data.table::as.data.table(DBI::dbGetQuery(snowCon, query, params = params))
  }

  requested <- fetch_surveys(location, start_datetime, end_datetime)

  survey_parameter_means <- function(rows, value_column) {
    if (!nrow(rows)) {
      return(data.table::data.table(
        survey_date = as.Date(character()),
        value = numeric()
      ))
    }
    included <- rows[
      !is.na(measurement_id) &
        !is.na(get(value_column)) &
        (is.na(exclude_flag) | !exclude_flag)
    ]
    if (!nrow(included)) {
      return(data.table::data.table(
        survey_date = as.Date(character()),
        value = numeric()
      ))
    }
    included[, .(value = mean(get(value_column))), by = survey_date]
  }

  calculate_multiplier <- function(old_rows, new_rows, value_column) {
    if (
      !is.null(adjust_start) &&
        !is.null(adjust_end) &&
        as.Date(adjust_start) == as.Date(adjust_end)
    ) {
      return(c(multiplier = 1, comparison_count = 0))
    }
    old_means <- survey_parameter_means(old_rows, value_column)
    new_means <- survey_parameter_means(new_rows, value_column)
    comparison <- merge(
      old_means,
      new_means,
      by = "survey_date",
      suffixes = c("_old", "_new")
    )
    if (!is.null(adjust_start)) {
      comparison <- comparison[
        survey_date >= as.Date(adjust_start),
      ]
    }
    if (!is.null(adjust_end)) {
      comparison <- comparison[
        survey_date <= as.Date(adjust_end),
      ]
    }
    if (!nrow(comparison) || mean(comparison$value_old) == 0) {
      return(c(multiplier = 1, comparison_count = 0))
    }
    multiplier <- mean(comparison$value_new) / mean(comparison$value_old)
    if (!is.finite(multiplier)) {
      multiplier <- 1
    }
    c(multiplier = multiplier, comparison_count = nrow(comparison))
  }

  old_rows <- data.table::data.table()
  old_multipliers <- c(swe = 1, depth = 1)
  old_comparison_count <- 0L
  if (!is.null(old_loc)) {
    old_loc <- trimws(as.character(old_loc)[1L])
    if (!nzchar(old_loc)) {
      stop("old_loc cannot be blank.")
    }
    recent_old_rows <- fetch_surveys(old_loc, start_datetime, end_datetime)
    if (nrow(recent_old_rows)) {
      old_rows <- fetch_surveys(old_loc, upper_datetime = end_datetime)
      comparison_rows <- fetch_surveys(
        location,
        upper_datetime = end_datetime
      )
      swe_adjustment <- calculate_multiplier(
        old_rows,
        comparison_rows,
        "swe"
      )
      depth_adjustment <- calculate_multiplier(
        old_rows,
        comparison_rows,
        "depth"
      )
      old_multipliers <- c(
        swe = unname(swe_adjustment[["multiplier"]]),
        depth = unname(depth_adjustment[["multiplier"]])
      )
      old_comparison_count <- max(
        as.integer(swe_adjustment[["comparison_count"]]),
        as.integer(depth_adjustment[["comparison_count"]])
      )
      current_dates <- unique(comparison_rows$survey_date)
      old_rows <- old_rows[!survey_date %in% current_dates]
    }
  }

  build_records <- function(
    rows,
    multipliers = c(swe = 1, depth = 1),
    actual_location = NULL,
    comparison_count = 0L
  ) {
    if (!nrow(rows)) {
      return(list())
    }
    survey_rows <- unique(rows[, .(
      import_source_id,
      target_datetime,
      datetime,
      survey_date,
      note,
      method
    )])
    records <- vector("list", nrow(survey_rows))

    for (survey_index in seq_len(nrow(survey_rows))) {
      survey <- survey_rows[survey_index]
      measurements <- rows[
        import_source_id == survey$import_source_id &
          !is.na(measurement_id)
      ]
      data.table::setorder(measurements, measurement_id)
      if (nrow(measurements)) {
        measurements[, observation_number := seq_len(.N)]
        measurements[is.na(exclude_flag), exclude_flag := FALSE]
        missing_exclusion_note <- measurements$exclude_flag &
          (is.na(measurements$component_note) |
            !nzchar(trimws(measurements$component_note)))
        measurements[
          which(missing_exclusion_note),
          component_note := "Excluded in source SnowDB; no reason recorded."
        ]
      }

      sample_note <- survey$note
      if (!is.null(actual_location)) {
        adjustment_note <- if (any(abs(multipliers - 1) > 1e-12)) {
          paste0(
            "Source snow-course location: ",
            actual_location,
            ". Aggregation multipliers: SWE ",
            format(round(multipliers[["swe"]], 6), trim = TRUE),
            ", depth ",
            format(round(multipliers[["depth"]], 6), trim = TRUE),
            ", calculated from ",
            comparison_count,
            " parallel survey date(s)."
          )
        } else {
          paste0(
            "Source snow-course location: ",
            actual_location,
            ". No aggregation multiplier was applied."
          )
        }
        sample_note <- paste(
          c(
            sample_note[!is.na(sample_note) & nzchar(sample_note)],
            adjustment_note
          ),
          collapse = " "
        )
      }

      sample <- data.frame(
        import_source_id = as.character(survey$import_source_id),
        target_datetime = as.POSIXct(survey$target_datetime, tz = "UTC"),
        datetime = as.POSIXct(survey$datetime, tz = "UTC"),
        note = if (length(sample_note) && nzchar(sample_note)) {
          sample_note
        } else {
          NA_character_
        },
        sample_type = reference_ids$sample_type_id,
        owner = reference_ids$owner_id,
        contributor = reference_ids$contributor_id,
        collection_method = reference_ids$collection_method_id,
        media_id = reference_ids$media_id,
        share_with = paste(share_with, collapse = ","),
        stringsAsFactors = FALSE
      )

      result_rows <- list()
      aggregation_rows <- list()
      component_rows <- list()
      survey_is_estimated <- any(
        measurements$estimate_flag[
          !measurements$exclude_flag &
            (!is.na(measurements$swe) | !is.na(measurements$depth))
        ] %in%
          TRUE
      )
      parameters <- list(
        list(
          source_column = "swe",
          parameter_id = reference_ids$swe_parameter_id,
          multiplier = multipliers[["swe"]]
        ),
        list(
          source_column = "depth",
          parameter_id = reference_ids$depth_parameter_id,
          multiplier = multipliers[["depth"]]
        )
      )

      for (parameter in parameters) {
        source_values <- measurements[[parameter$source_column]]
        included <- !measurements$exclude_flag & !is.na(source_values)
        if (!length(source_values) || !any(included)) {
          next
        }
        result_row <- length(result_rows) + 1L
        result_rows[[result_row]] <- data.frame(
          parameter_id = as.integer(parameter$parameter_id),
          result = mean(source_values[included]) * parameter$multiplier,
          result_value_type = if (survey_is_estimated) {
            reference_ids$estimated_result_id
          } else {
            reference_ids$actual_result_id
          },
          result_type = reference_ids$field_result_type_id,
          protocol_method = reference_ids$protocol_method_id
        )
        arguments <- list(
          missing_values = "ignore",
          non_detects = "exclude"
        )
        if (abs(parameter$multiplier - 1) > 1e-12) {
          arguments$multiplier <- unname(parameter$multiplier)
        }
        aggregation_rows[[result_row]] <- data.frame(
          result_row = result_row,
          aggregation_type = "mean",
          calculation_version = 1L,
          calculation_arguments = as.character(jsonlite::toJSON(
            arguments,
            auto_unbox = TRUE
          )),
          expected_count = if (identical(tolower(survey$method), "standard")) {
            10L
          } else {
            NA_integer_
          },
          note = "Arithmetic mean of included snow-course observations.",
          stringsAsFactors = FALSE
        )
        component_rows[[result_row]] <- data.frame(
          result_row = result_row,
          observation_number = measurements$observation_number,
          observation_datetime = as.POSIXct(
            measurements$observation_datetime,
            tz = "UTC"
          ),
          result = as.numeric(source_values),
          included_in_aggregate = !measurements$exclude_flag,
          note = measurements$component_note,
          stringsAsFactors = FALSE
        )
      }

      if (!length(result_rows)) {
        records[[survey_index]] <- NULL
        next
      }
      records[[survey_index]] <- list(
        sample = sample,
        results = data.table::rbindlist(result_rows, fill = TRUE),
        result_aggregations = data.table::rbindlist(
          aggregation_rows,
          fill = TRUE
        ),
        result_components = data.table::rbindlist(
          component_rows,
          fill = TRUE
        )
      )
    }
    Filter(Negate(is.null), records)
  }

  records <- c(
    build_records(
      old_rows,
      multipliers = old_multipliers,
      actual_location = if (nrow(old_rows)) old_loc else NULL,
      comparison_count = old_comparison_count
    ),
    build_records(requested)
  )
  if (!length(records)) {
    return(list())
  }
  sample_datetimes <- vapply(
    records,
    function(record) as.numeric(record$sample$datetime[[1L]]),
    numeric(1)
  )
  records[order(sample_datetimes)]
}
