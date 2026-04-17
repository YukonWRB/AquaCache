#' Get minute-resolution weather observations from the ECCC SWOB realtime API
#'
#' @description
#' Fetches minute-resolution SWOB observations from the filtered realtime API at
#' https://api.weather.gc.ca/. The query is restricted to one station, one
#' parameter, and the requested datetime interval, then returned as CSV for fast
#' parsing into AquaCache's standard one-timeseries-at-a-time workflow.
#'
#' @details
#' Raw parameter names are available from the SWOB queryables page at
#' https://api.weather.gc.ca/collections/swob-realtime/queryables?f=html. The
#' function also accepts familiar aliases such as `"temp"`, `"wind_spd"`,
#' `"wind_dir"`, `"wind_gust"`, `"stn_press"`, and `"dew_point"`, though you are
#' recommended to use exact SWOB parameter names when possible.
#'
#' @param location A four-letter station code used by the SWOB realtime API,
#'   such as `"CVXY"`. See details.
#' @param parameter The SWOB element name to extract, or a supported alias from
#'   [downloadECCCwx()] such as `"temp"`, `"wind_spd"`, `"wind_dir"`,
#'   `"wind_gust"`, `"stn_press"`, or `"dew_point"`. See details.
#' @param start_datetime Specify as class Date, POSIXct OR as character string
#'   which can be interpreted as POSIXct. If character, UTC offset of 0 will be
#'   assigned, otherwise conversion to UTC 0 will be performed on POSIXct class
#'   input. If date, time will default to 00:00 to capture whole day.
#' @param end_datetime Specify as class Date, POSIXct OR as character string
#'   which can be interpreted as POSIXct. If character, UTC offset of 0 will be
#'   assigned, otherwise conversion to UTC 0 will be performed on POSIXct class
#'   input. If Date, time will default to 23:59:59 to capture whole day.
#' @param con A connection to the aquacache database, necessary to allow for the
#'   mapping of approvals, grades, qualifiers, and owners to the database. If
#'   left NULL connection will be made and closed automatically.
#'
#' @return A data.table object of weather observations, with datetimes in UTC-0.
#' @export
downloadECCCwxMinute <- function(
  location,
  parameter,
  start_datetime,
  end_datetime = Sys.time(),
  con = NULL
) {
  if (length(location) != 1 || is.na(location)) {
    stop("downloadECCCwxMinute: 'location' must be a single non-NA value.")
  }
  if (length(parameter) != 1 || is.na(parameter)) {
    stop("downloadECCCwxMinute: 'parameter' must be a single non-NA value.")
  }

  location <- toupper(trimws(as.character(location)))
  parameter_requested <- trimws(as.character(parameter))
  if (!nzchar(location)) {
    stop("downloadECCCwxMinute: 'location' cannot be an empty string.")
  }
  if (!nzchar(parameter_requested)) {
    stop("downloadECCCwxMinute: 'parameter' cannot be an empty string.")
  }

  parameter_swob <- dlECCCwxMinute_resolve_parameter(parameter_requested)

  bounds <- dlECCCwxMinute_normalize_datetimes(
    start_datetime = start_datetime,
    end_datetime = end_datetime
  )
  start_datetime <- bounds$start_datetime
  end_datetime <- bounds$end_datetime

  # IF start_datetime if > 1 month ago, trim (otherwise it takes too long)
  if (start_datetime < (Sys.time() - 30 * 24 * 3600)) {
    start_datetime <- Sys.time() - 30 * 24 * 3600
  }

  if (start_datetime > end_datetime) {
    stop(
      "downloadECCCwxMinute: 'start_datetime' must be earlier than 'end_datetime'."
    )
  }

  windows <- dlECCCwxMinute_day_windows(
    start_datetime = start_datetime,
    end_datetime = end_datetime
  )
  if (length(windows) == 0) {
    return(dlECCCwxMinute_empty_result())
  }

  all_rows <- data.table::rbindlist(
    lapply(
      windows,
      function(window) {
        dlECCCwxMinute_fetch_window(
          location = location,
          parameter = parameter_swob,
          start_datetime = window$start_datetime,
          end_datetime = window$end_datetime
        )
      }
    ),
    use.names = TRUE,
    fill = TRUE
  )
  if (nrow(all_rows) == 0) {
    return(dlECCCwxMinute_empty_result())
  }

  if (!("date_tm-value" %in% names(all_rows))) {
    stop(
      "downloadECCCwxMinute: The API response did not contain 'date_tm-value'."
    )
  }
  if (!(parameter_swob %in% names(all_rows))) {
    stop(
      "downloadECCCwxMinute: The parameter '",
      parameter_requested,
      "' is not available in the downloaded data."
    )
  }

  data <- data.table::data.table(
    datetime = all_rows[["date_tm-value"]],
    value = suppressWarnings(as.numeric(all_rows[[parameter_swob]]))
  )
  data <- data[
    !is.na(datetime) &
      datetime >= start_datetime &
      datetime <= end_datetime &
      !is.na(value)
  ]
  if (nrow(data) == 0) {
    return(dlECCCwxMinute_empty_result())
  }

  data.table::setorderv(data, "datetime")
  data <- data[!duplicated(datetime, fromLast = TRUE)]

  unit_column <- dlECCCwxMinute_unit_column(parameter_swob)
  unit_value <- NULL
  if (unit_column %in% names(all_rows)) {
    unit_values <- as.character(all_rows[[unit_column]])
    unit_values <- trimws(unit_values[!is.na(all_rows[[unit_column]])])
    unit_values <- unique(unit_values[nzchar(unit_values)])
    if (length(unit_values) > 0) {
      unit_value <- unit_values[[1]]
    }
  }

  data$value <- dlECCCwxMinute_convert_values(
    values = data$value,
    parameter = parameter_swob,
    unit = unit_value
  )

  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
  }

  defaults <- dlECCCwxMinute_db_defaults(con = con)
  data$grade <- defaults$grade
  data$approval <- defaults$approval
  data$qualifier <- defaults$qualifier
  data$owner <- defaults$organization
  data$contributor <- defaults$organization

  return(data)
}

#' downloadECCCwxMinute helper
#' @keywords internal
#' @noRd
dlECCCwxMinute_normalize_datetimes <- function(
  start_datetime,
  end_datetime
) {
  list(
    start_datetime = dlECCCwxMinute_as_utc_datetime(
      x = start_datetime,
      is_end = FALSE
    ),
    end_datetime = dlECCCwxMinute_as_utc_datetime(
      x = end_datetime,
      is_end = TRUE
    )
  )
}

#' downloadECCCwxMinute helper
#' @keywords internal
#' @noRd
dlECCCwxMinute_as_utc_datetime <- function(x, is_end = FALSE) {
  tryCatch(
    {
      if (inherits(x, "character") && nchar(x) > 10) {
        x <- as.POSIXct(x, tz = "UTC")
      } else if (inherits(x, "POSIXct")) {
        attr(x, "tzone") <- "UTC"
      } else if (
        inherits(x, "Date") ||
          (inherits(x, "character") && nchar(x) == 10)
      ) {
        x <- as.POSIXct(x, tz = "UTC")
        if (is_end) {
          x <- x + 60 * 60 * 23.9999
        }
      } else {
        stop("Parameter could not be coerced to POSIXct.")
      }
    },
    error = function(e) {
      stop("Failed to convert input datetime to POSIXct.")
    }
  )

  x
}

#' downloadECCCwxMinute helper
#' @keywords internal
#' @noRd
dlECCCwxMinute_empty_result <- function() {
  data.table::data.table(
    datetime = as.POSIXct(character(), tz = "UTC"),
    value = numeric(),
    grade = integer(),
    approval = integer(),
    qualifier = integer(),
    owner = integer(),
    contributor = integer()
  )
}

#' downloadECCCwxMinute helper
#' @keywords internal
#' @noRd
dlECCCwxMinute_day_windows <- function(start_datetime, end_datetime) {
  windows <- list()
  window_start <- start_datetime

  repeat {
    next_day <- as.POSIXct(
      paste0(as.Date(window_start, tz = "UTC") + 1, " 00:00:00"),
      tz = "UTC"
    )
    window_end <- min(end_datetime, next_day - 1)
    windows[[length(windows) + 1L]] <- list(
      start_datetime = window_start,
      end_datetime = window_end
    )

    if (window_end >= end_datetime) {
      break
    }

    window_start <- window_end + 1
  }

  windows
}

#' downloadECCCwxMinute helper
#' @keywords internal
#' @noRd
dlECCCwxMinute_fetch_window <- function(
  location,
  parameter,
  start_datetime,
  end_datetime
) {
  page <- dlECCCwxMinute_fetch_csv(dlECCCwxMinute_api_url(
    location = location,
    parameter = parameter,
    start_datetime = start_datetime,
    end_datetime = end_datetime
  ))
  if (!data.table::is.data.table(page)) {
    page <- data.table::as.data.table(page)
  }

  page
}

#' downloadECCCwxMinute helper
#' @keywords internal
#' @noRd
dlECCCwxMinute_fetch_csv <- function(url) {
  response <- httr::GET(url)
  status <- httr::status_code(response)
  if (status == 404) {
    return(data.table::data.table())
  }

  httr::stop_for_status(response)
  csv_txt <- httr::content(response, as = "text", encoding = "UTF-8")
  if (!nzchar(trimws(csv_txt))) {
    return(data.table::data.table())
  }

  data.table::fread(
    text = csv_txt,
    na.strings = c("", "MSNG", "MISSING", "NULL", "NA"),
    check.names = FALSE,
    showProgress = FALSE
  )
}

#' downloadECCCwxMinute helper
#' @keywords internal
#' @noRd
dlECCCwxMinute_api_url <- function(
  location,
  parameter,
  start_datetime,
  end_datetime,
  limit = 2000L
) {
  properties <- unique(c(
    "date_tm-value",
    parameter,
    dlECCCwxMinute_unit_column(parameter)
  ))
  query <- c(
    lang = "en",
    offset = "0",
    limit = as.character(as.integer(limit)),
    sortby = "-date_tm-value",
    url = location,
    properties = paste(properties, collapse = ","),
    f = "csv",
    datetime = paste0(
      dlECCCwxMinute_format_interval_datetime(start_datetime),
      "/",
      dlECCCwxMinute_format_interval_datetime(end_datetime)
    )
  )

  paste0(
    "https://api.weather.gc.ca/collections/swob-realtime/items?",
    paste(
      paste0(
        names(query),
        "=",
        vapply(query, utils::URLencode, character(1), reserved = TRUE)
      ),
      collapse = "&"
    )
  )
}

#' downloadECCCwxMinute helper
#' @keywords internal
#' @noRd
dlECCCwxMinute_format_interval_datetime <- function(x) {
  format(x, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

#' downloadECCCwxMinute helper
#' @keywords internal
#' @noRd
dlECCCwxMinute_parameter_aliases <- function() {
  c(
    temp = "air_temp",
    wind_spd = "avg_wnd_spd_10m_pst1mt",
    wind_dir = "avg_wnd_dir_10m_pst1mt",
    wind_gust = "max_wnd_spd_10m_pst1mt",
    stn_press = "stn_pres",
    dew_point = "dwpt_temp"
  )
}

#' downloadECCCwxMinute helper
#' @keywords internal
#' @noRd
dlECCCwxMinute_resolve_parameter <- function(parameter) {
  parameter <- tolower(parameter)
  aliases <- dlECCCwxMinute_parameter_aliases()
  if (parameter %in% names(aliases)) {
    return(unname(aliases[[parameter]]))
  }

  parameter
}

#' downloadECCCwxMinute helper
#' @keywords internal
#' @noRd
dlECCCwxMinute_unit_column <- function(parameter) {
  paste0(parameter, "-uom")
}

#' downloadECCCwxMinute helper
#' @keywords internal
#' @noRd
dlECCCwxMinute_convert_values <- function(values, parameter, unit) {
  if (
    !is.null(unit) &&
      identical(unit, "km/h") &&
      grepl("wnd_spd", parameter, fixed = TRUE)
  ) {
    return(values / 3.6)
  }

  values
}

#' downloadECCCwxMinute helper
#' @keywords internal
#' @noRd
dlECCCwxMinute_db_defaults <- function(con) {
  organization_id <- DBI::dbGetQuery(
    con,
    "SELECT organization_id FROM organizations WHERE name = 'Environment and Climate Change Canada'"
  )[1, 1]
  if (is.na(organization_id)) {
    organization_id <- DBI::dbGetQuery(
      con,
      "INSERT INTO organizations (name, name_fr) VALUES ($1, $2) RETURNING organization_id;",
      params = list(
        "Environment and Climate Change Canada",
        "Environnement et Changement Climatique Canada"
      )
    )[1, 1]
  }

  list(
    organization = as.integer(organization_id),
    grade = as.integer(DBI::dbGetQuery(
      con,
      "SELECT grade_type_id FROM grade_types WHERE grade_type_code = 'UNS'"
    )[1, 1]),
    approval = as.integer(DBI::dbGetQuery(
      con,
      "SELECT approval_type_id FROM approval_types WHERE approval_type_code = 'UNS'"
    )[1, 1]),
    qualifier = as.integer(DBI::dbGetQuery(
      con,
      "SELECT qualifier_type_id FROM qualifier_types WHERE qualifier_type_code = 'UNS'"
    )[1, 1])
  )
}
