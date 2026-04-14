#' Get minute-resolution weather observations from the ECCC SWOB archive
#'
#' @description
#' Fetches minute-resolution SWOB XML observations from the ECCC data
#' distribution archive. Because each XML file contains all parameters for one
#' minute, parsed observations are cached by station and day in the session
#' temporary directory. Subsequent calls for other parameters at the same
#' station/day can therefore reuse the same cached data instead of downloading
#' the same XML files again.
#'
#' @details
#' Data is fetched from the ECCC API at https://api.weather.gc.ca/. To see a list of stations, use this url: https://api.weather.gc.ca/collections/swob-realtime/items?limit=2000&offset=0. Once you find your station, click on one of the .xml links to see an example URL and parse out the desired location and parameter codes, such as https://dd.weather.gc.ca//20260316/WXO-DD/observations/swob-ml/20260316/CVXY/2026-03-16-0742-CVXY-AUTO-minute-swob.xml. In this example (Whitehorse Auto), the `location` is `CVXY`, the `station_type` is `AUTO`. Available parameters are extensive and listed in the right-side table below the obs_date_tm and processed_date_tm fields, beginning with `data_avail_pst1mt`.
#'
#' @param location A four-letter station code used in the SWOB archive, such as
#'   `"CVXY"`. See details.
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
#' @param station_type File suffix used in the SWOB archive. Defaults to
#'   `"AUTO"`.
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
  station_type = "AUTO",
  con = NULL
) {
  if (length(location) != 1 || is.na(location)) {
    stop("downloadECCCwxMinute: 'location' must be a single non-NA value.")
  }
  if (length(parameter) != 1 || is.na(parameter)) {
    stop("downloadECCCwxMinute: 'parameter' must be a single non-NA value.")
  }
  if (length(station_type) != 1 || is.na(station_type)) {
    stop("downloadECCCwxMinute: 'station_type' must be a single non-NA value.")
  }

  location <- toupper(trimws(as.character(location)))
  station_type <- toupper(trimws(as.character(station_type)))
  parameter_requested <- trimws(as.character(parameter))
  if (!nzchar(location)) {
    stop("downloadECCCwxMinute: 'location' cannot be an empty string.")
  }
  if (!nzchar(parameter_requested)) {
    stop("downloadECCCwxMinute: 'parameter' cannot be an empty string.")
  }
  if (!nzchar(station_type)) {
    stop("downloadECCCwxMinute: 'station_type' cannot be an empty string.")
  }

  parameter_swob <- downloadECCCwxMinute_resolve_parameter(parameter_requested)

  bounds <- downloadECCCwxMinute_normalize_datetimes(
    start_datetime = start_datetime,
    end_datetime = end_datetime
  )
  start_datetime <- bounds$start_datetime
  end_datetime <- bounds$end_datetime

  if (start_datetime > end_datetime) {
    stop(
      "downloadECCCwxMinute: 'start_datetime' must be earlier than 'end_datetime'."
    )
  }

  requested_minutes <- downloadECCCwxMinute_requested_minutes(
    start_datetime = start_datetime,
    end_datetime = end_datetime
  )
  if (length(requested_minutes) == 0) {
    return(downloadECCCwxMinute_empty_result())
  }

  requested_days <- split(
    requested_minutes,
    format(requested_minutes, "%Y-%m-%d", tz = "UTC")
  )

  bundles <- lapply(
    names(requested_days),
    function(day_chr) {
      downloadECCCwxMinute_get_day_bundle(
        location = location,
        station_type = station_type,
        day = as.Date(day_chr),
        requested_minutes = requested_days[[day_chr]]
      )
    }
  )

  all_rows <- data.table::rbindlist(
    lapply(bundles, `[[`, "data"),
    use.names = TRUE,
    fill = TRUE
  )
  if (nrow(all_rows) == 0) {
    return(downloadECCCwxMinute_empty_result())
  }

  units <- unlist(lapply(bundles, `[[`, "units"), use.names = TRUE)
  if (length(units) > 0) {
    units <- units[!duplicated(names(units), fromLast = TRUE)]
  }

  if (!(parameter_swob %in% names(all_rows))) {
    stop(
      "downloadECCCwxMinute: The parameter '",
      parameter_requested,
      "' is not available in the downloaded data."
    )
  }

  data <- data.table::data.table(
    datetime = all_rows$datetime,
    value = suppressWarnings(as.numeric(all_rows[[parameter_swob]]))
  )
  data <- data[
    datetime >= start_datetime &
      datetime <= end_datetime &
      !is.na(value)
  ]
  if (nrow(data) == 0) {
    return(downloadECCCwxMinute_empty_result())
  }

  data$value <- downloadECCCwxMinute_convert_values(
    values = data$value,
    parameter = parameter_swob,
    unit = downloadECCCwxMinute_unit_lookup(
      units = units,
      parameter = parameter_swob
    )
  )

  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
  }

  defaults <- downloadECCCwxMinute_db_defaults(con = con)
  data$grade <- defaults$grade
  data$approval <- defaults$approval
  data$qualifier <- defaults$qualifier
  data$owner <- defaults$organization
  data$contributor <- defaults$organization
  data <- data[order(datetime)]

  data[]
}

downloadECCCwxMinute_normalize_datetimes <- function(
  start_datetime,
  end_datetime
) {
  list(
    start_datetime = downloadECCCwxMinute_as_utc_datetime(
      x = start_datetime,
      is_end = FALSE
    ),
    end_datetime = downloadECCCwxMinute_as_utc_datetime(
      x = end_datetime,
      is_end = TRUE
    )
  )
}

downloadECCCwxMinute_as_utc_datetime <- function(x, is_end = FALSE) {
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

downloadECCCwxMinute_requested_minutes <- function(
  start_datetime,
  end_datetime
) {
  start_minute <- as.POSIXct(
    strftime(start_datetime, "%Y-%m-%d %H:%M:00", tz = "UTC"),
    tz = "UTC"
  )
  if (start_datetime > start_minute) {
    start_minute <- start_minute + 60
  }

  end_minute <- as.POSIXct(
    strftime(end_datetime, "%Y-%m-%d %H:%M:00", tz = "UTC"),
    tz = "UTC"
  )

  if (start_minute > end_minute) {
    return(as.POSIXct(character(), tz = "UTC"))
  }

  seq(start_minute, end_minute, by = "1 min")
}

downloadECCCwxMinute_empty_result <- function() {
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

downloadECCCwxMinute_empty_bundle <- function() {
  list(
    data = data.table::data.table(
      datetime = as.POSIXct(character(), tz = "UTC")
    ),
    units = structure(character(), names = character())
  )
}

downloadECCCwxMinute_cache_dir <- function() {
  file.path(tempdir(), "downloadECCCwxMinute")
}

downloadECCCwxMinute_cache_file <- function(location, station_type, day) {
  file.path(
    downloadECCCwxMinute_cache_dir(),
    paste0(location, "_", station_type, "_", format(day, "%Y-%m-%d"), ".rds")
  )
}

downloadECCCwxMinute_read_cache <- function(location, station_type, day) {
  cache_file <- downloadECCCwxMinute_cache_file(
    location = location,
    station_type = station_type,
    day = day
  )
  if (!file.exists(cache_file)) {
    return(downloadECCCwxMinute_empty_bundle())
  }

  cache <- readRDS(cache_file)
  if (data.table::is.data.table(cache) || inherits(cache, "data.frame")) {
    cache <- list(data = data.table::as.data.table(cache), units = character())
  }
  if (!is.list(cache) || !("data" %in% names(cache))) {
    stop("downloadECCCwxMinute: Cached minute data has an unexpected format.")
  }

  cache$data <- data.table::as.data.table(cache$data)
  if (is.null(cache$units)) {
    cache$units <- character()
  }

  cache
}

downloadECCCwxMinute_write_cache <- function(
  location,
  station_type,
  day,
  cache
) {
  dir.create(
    downloadECCCwxMinute_cache_dir(),
    recursive = TRUE,
    showWarnings = FALSE
  )
  saveRDS(
    cache,
    downloadECCCwxMinute_cache_file(
      location = location,
      station_type = station_type,
      day = day
    )
  )
}

downloadECCCwxMinute_get_day_bundle <- function(
  location,
  station_type,
  day,
  requested_minutes
) {
  cache <- downloadECCCwxMinute_read_cache(
    location = location,
    station_type = station_type,
    day = day
  )

  requested_minutes <- as.POSIXct(
    requested_minutes,
    tz = "UTC",
    origin = "1970-01-01"
  )
  if (nrow(cache$data) > 0) {
    missing_minutes <- requested_minutes[
      !(requested_minutes %in% cache$data$datetime)
    ]
  } else {
    missing_minutes <- requested_minutes
  }

  if (length(missing_minutes) > 0) {
    available_files <- downloadECCCwxMinute_list_day_files(
      location = location,
      station_type = station_type,
      day = day
    )

    to_fetch <- available_files[datetime %in% missing_minutes]
    if (nrow(to_fetch) > 0) {
      fetched <- lapply(
        to_fetch$filename,
        function(filename) {
          xml_txt <- downloadECCCwxMinute_fetch_xml(
            downloadECCCwxMinute_file_url(
              location = location,
              day = day,
              filename = filename
            )
          )
          if (is.null(xml_txt)) {
            return(NULL)
          }

          downloadECCCwxMinute_parse_xml(xml_txt)
        }
      )
      fetched <- Filter(Negate(is.null), fetched)

      if (length(fetched) > 0) {
        cache$data <- data.table::rbindlist(
          c(list(cache$data), lapply(fetched, `[[`, "data")),
          use.names = TRUE,
          fill = TRUE
        )
        data.table::setorderv(cache$data, "datetime")
        cache$data <- cache$data[!duplicated(datetime, fromLast = TRUE)]

        fetched_units <- unlist(
          lapply(fetched, `[[`, "units"),
          use.names = TRUE
        )
        if (length(fetched_units) > 0) {
          cache$units <- c(cache$units, fetched_units)
          cache$units <- cache$units[
            !duplicated(names(cache$units), fromLast = TRUE)
          ]
        }

        downloadECCCwxMinute_write_cache(
          location = location,
          station_type = station_type,
          day = day,
          cache = cache
        )
      }
    }
  }

  list(
    data = cache$data[datetime %in% requested_minutes],
    units = cache$units
  )
}

downloadECCCwxMinute_day_url <- function(location, day) {
  day_path <- format(day, "%Y%m%d")
  paste0(
    "https://dd.weather.gc.ca/",
    day_path,
    "/WXO-DD/observations/swob-ml/",
    day_path,
    "/",
    location,
    "/"
  )
}

downloadECCCwxMinute_file_url <- function(location, day, filename) {
  paste0(downloadECCCwxMinute_day_url(location = location, day = day), filename)
}

downloadECCCwxMinute_filename_datetime <- function(filename) {
  as.POSIXct(
    strptime(substr(filename, 1, 16), format = "%Y-%m-%d-%H%M", tz = "UTC")
  )
}

downloadECCCwxMinute_list_day_files <- function(location, station_type, day) {
  response <- httr::GET(downloadECCCwxMinute_day_url(
    location = location,
    day = day
  ))
  status <- httr::status_code(response)
  if (status == 404) {
    return(data.table::data.table(
      filename = character(),
      datetime = as.POSIXct(character(), tz = "UTC")
    ))
  }

  httr::stop_for_status(response)
  listing <- httr::content(response, as = "text", encoding = "UTF-8")
  listing <- xml2::read_html(listing)

  hrefs <- xml2::xml_attr(
    xml2::xml_find_all(
      listing,
      ".//a[contains(@href, '-minute-swob.xml')]"
    ),
    "href"
  )
  pattern <- paste0(
    "^\\d{4}-\\d{2}-\\d{2}-\\d{4}-",
    location,
    "-",
    station_type,
    "-minute-swob\\.xml$"
  )
  hrefs <- unique(hrefs[grepl(pattern, hrefs)])

  if (length(hrefs) == 0) {
    return(data.table::data.table(
      filename = character(),
      datetime = as.POSIXct(character(), tz = "UTC")
    ))
  }

  data.table::data.table(
    filename = hrefs,
    datetime = as.POSIXct(
      unlist(lapply(hrefs, downloadECCCwxMinute_filename_datetime)),
      origin = "1970-01-01",
      tz = "UTC"
    )
  )
}

downloadECCCwxMinute_fetch_xml <- function(url) {
  response <- httr::GET(url)
  status <- httr::status_code(response)
  if (status == 404) {
    return(NULL)
  }

  httr::stop_for_status(response)
  httr::content(response, as = "text", encoding = "UTF-8")
}

downloadECCCwxMinute_parse_xml <- function(xml_txt) {
  doc <- xml2::read_xml(xml_txt)
  datetime_node <- xml2::xml_find_first(
    doc,
    ".//*[local-name()='samplingTime']//*[local-name()='timePosition']"
  )
  if (inherits(datetime_node, "xml_missing")) {
    stop(
      "downloadECCCwxMinute: Could not find a sampling time in the SWOB XML."
    )
  }

  datetime <- as.POSIXct(
    xml2::xml_text(datetime_node),
    format = "%Y-%m-%dT%H:%M:%OSZ",
    tz = "UTC"
  )

  element_nodes <- xml2::xml_find_all(
    doc,
    ".//*[local-name()='result']//*[local-name()='element']"
  )
  if (length(element_nodes) == 0) {
    return(list(
      data = data.table::data.table(datetime = datetime),
      units = structure(character(), names = character())
    ))
  }

  parsed <- data.table::data.table(
    name = xml2::xml_attr(element_nodes, "name"),
    unit = xml2::xml_attr(element_nodes, "uom"),
    value = xml2::xml_attr(element_nodes, "value")
  )
  parsed <- parsed[!is.na(name) & name != ""]
  if (nrow(parsed) == 0) {
    return(list(
      data = data.table::data.table(datetime = datetime),
      units = structure(character(), names = character())
    ))
  }

  parsed$value[
    parsed$value %in% c("", "MSNG", "MISSING", "NULL", "NA")
  ] <- NA_character_
  parsed <- parsed[!duplicated(name, fromLast = TRUE)]

  data <- data.table::as.data.table(
    as.list(stats::setNames(
      suppressWarnings(as.numeric(parsed$value)),
      parsed$name
    ))
  )
  data[, datetime := datetime]
  data.table::setcolorder(data, "datetime")

  list(
    data = data,
    units = stats::setNames(parsed$unit, parsed$name)
  )
}

downloadECCCwxMinute_parameter_aliases <- function() {
  c(
    temp = "air_temp",
    wind_spd = "avg_wnd_spd_10m_pst1mt",
    wind_dir = "avg_wnd_dir_10m_pst1mt",
    wind_gust = "max_wnd_spd_10m_pst1mt",
    stn_press = "stn_pres",
    dew_point = "dwpt_temp"
  )
}

downloadECCCwxMinute_resolve_parameter <- function(parameter) {
  parameter <- tolower(parameter)
  aliases <- downloadECCCwxMinute_parameter_aliases()
  if (parameter %in% names(aliases)) {
    return(unname(aliases[[parameter]]))
  }

  parameter
}

downloadECCCwxMinute_convert_values <- function(values, parameter, unit) {
  if (
    !is.null(unit) &&
      identical(unit, "km/h") &&
      grepl("wnd_spd", parameter, fixed = TRUE)
  ) {
    return(values / 3.6)
  }

  values
}

downloadECCCwxMinute_unit_lookup <- function(units, parameter) {
  unit <- units[parameter]
  if (length(unit) == 0 || is.na(unit)) {
    return(NULL)
  }

  unname(unit[[1]])
}

downloadECCCwxMinute_db_defaults <- function(con) {
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
