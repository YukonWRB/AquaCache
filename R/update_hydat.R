#' Update HYDAT-related timeseries
#'
#' @description
#'
#' Checks the local version of HYDAT using [hydat_check()] and updates WSC flow
#' and water-level timeseries when the local HYDAT copy has changed or when
#' `force_update = TRUE`. HYDAT daily means are stored as one-day-period rows in
#' `measurements_continuous` on days that do not already have higher-frequency
#' measurements. Database triggers maintain `measurements_calculated_daily`.
#'
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [AquaConnect()].
#' @param timeseries_id Character vector of timeseries_ids for which to look for updates. "all" will attempt to update all timeseries where the import function is downloadWSC.
#' @param force_update Set TRUE to force a check of each location against the local copy of HYDAT.
#'
#' @return TRUE if a new HYDAT version was found, otherwise FALSE.
#' @export

update_hydat <- function(
  con = AquaConnect(silent = TRUE),
  timeseries_id = "all",
  force_update = FALSE
) {
  hydat_check(silent = FALSE)
  hydat_path <- tidyhydat::hy_downloaded_db()

  local_hydat <- tidyhydat::hy_version(hydat_path)$Date
  DB_hydat <- DBI::dbGetQuery(
    con,
    "SELECT value FROM internal_status WHERE event = 'HYDAT_version'"
  )[1, 1]
  new_hydat <- is.na(DB_hydat) || !identical(as.character(DB_hydat), as.character(local_hydat))

  if (!new_hydat && !force_update) {
    message(
      "No updates were made because the last HYDAT version referenced in the database is the same as the current HYDAT, and you didn't specify force_update = TRUE."
    )
    return(new_hydat)
  }

  if (!new_hydat && force_update) {
    message(
      "Local version of HYDAT is current, updating related timeseries because force_update is set to TRUE."
    )
  }

  if (timeseries_id[1] == "all") {
    all_timeseries <- DBI::dbGetQuery(
      con,
      "SELECT
         t.parameter_id,
         t.timeseries_id,
         t.source_fx_args,
         l.location_id
       FROM timeseries t
       JOIN locations l
         ON t.location_id = l.location_id
       WHERE source_fx = 'downloadWSC';"
    )
  } else {
    all_timeseries <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT
           t.parameter_id,
           t.timeseries_id,
           t.source_fx_args,
           l.location_id
         FROM timeseries t
         JOIN locations l
           ON t.location_id = l.location_id
         WHERE t.timeseries_id IN ('",
        paste(timeseries_id, collapse = "', '"),
        "') AND source_fx = 'downloadWSC';"
      )
    )
    if (length(timeseries_id) != nrow(all_timeseries)) {
      fail <- timeseries_id[!(timeseries_id %in% all_timeseries$timeseries_id)]
      if (length(fail) == 1) {
        warning(
          "update_hydat: Could not find one of the timeseries_ids that you specified: ID ",
          fail,
          " is missing from the database."
        )
      } else {
        warning(
          "update_hydat: Could not find some of the timeseries_ids that you specified: IDs ",
          paste(fail, collapse = ", "),
          " are missing from the database."
        )
      }
    }
  }

  if (nrow(all_timeseries) == 0) {
    message("No WSC timeseries were found to update.")
    return(new_hydat)
  }

  all_timeseries$location <- vapply(
    all_timeseries$source_fx_args,
    function(x) {
      tryCatch(jsonlite::fromJSON(x)$location, error = function(e) NA_character_)
    },
    character(1)
  )
  all_timeseries <- all_timeseries[!is.na(all_timeseries$location), , drop = FALSE]

  organization_id <- DBI::dbGetQuery(
    con,
    "SELECT organization_id FROM organizations WHERE name = 'Water Survey of Canada'"
  )[1, 1]
  if (is.na(organization_id)) {
    organization_id <- DBI::dbGetQuery(
      con,
      "INSERT INTO organizations (name, name_fr) VALUES ($1, $2) RETURNING organization_id;",
      params = list(
        "Water Survey of Canada",
        "Releve hydrometrique du Canada"
      )
    )[1, 1]
  }

  grade_unspecified <- DBI::dbGetQuery(
    con,
    "SELECT grade_type_id FROM grade_types WHERE grade_type_code = 'UNS'"
  )[1, 1]
  if (is.na(grade_unspecified)) {
    stop(
      "Grade type 'Unspecified' (column grade_type_description) not found in the database. Please add it before proceeding."
    )
  }
  approval_approved <- DBI::dbGetQuery(
    con,
    "SELECT approval_type_id FROM approval_types WHERE approval_type_code = 'A'"
  )[1, 1]
  if (is.na(approval_approved)) {
    stop(
      "Approval type 'Approved' (column approval_type_description) not found in the database. Please add it before proceeding."
    )
  }

  qualifiers <- DBI::dbGetQuery(con, "SELECT * FROM qualifier_types")
  qualifier_mapping <- c(
    "B" = qualifiers[
      qualifiers$qualifier_type_code == "ICE",
      "qualifier_type_id"
    ],
    "E" = qualifiers[
      qualifiers$qualifier_type_code == "EST",
      "qualifier_type_id"
    ],
    "D" = qualifiers[
      qualifiers$qualifier_type_code == "DRY",
      "qualifier_type_id"
    ],
    "A" = qualifiers[
      qualifiers$qualifier_type_code == "UNK",
      "qualifier_type_id"
    ]
  )
  qualifier_unknown <- qualifiers[
    qualifiers$qualifier_type_code == "UNK",
    "qualifier_type_id"
  ]

  format_hydat_daily <- function(raw) {
    if (nrow(raw) == 0) {
      return(data.frame())
    }

    raw <- raw[, c("Date", "Value", "Symbol")]
    names(raw) <- c("date", "value", "qualifier")
    raw <- raw[!is.na(raw$value), , drop = FALSE]
    if (nrow(raw) == 0) {
      return(raw)
    }

    raw$qualifier <- ifelse(
      raw$qualifier %in% names(qualifier_mapping),
      qualifier_mapping[raw$qualifier],
      qualifier_unknown
    )
    raw$qualifier <- as.integer(raw$qualifier)
    raw$owner <- organization_id
    raw$contributor <- organization_id
    raw$approval <- approval_approved
    raw$grade <- grade_unspecified
    raw$imputed <- FALSE
    raw$date <- as.Date(raw$date)
    raw
  }

  fetch_hydat_daily <- function(location, param_name) {
    tryCatch(
      {
        if (param_name == "water flow") {
          format_hydat_daily(as.data.frame(tidyhydat::hy_daily_flows(location)))
        } else {
          format_hydat_daily(as.data.frame(tidyhydat::hy_daily_levels(location)))
        }
      },
      error = function(e) data.frame()
    )
  }

  ensure_wsc_timeseries <- function(location, location_id, param_name) {
    parameter_id <- DBI::dbGetQuery(
      con,
      "SELECT parameter_id FROM parameters WHERE param_name = $1",
      params = list(param_name)
    )[1, 1]
    media_id <- DBI::dbGetQuery(
      con,
      "SELECT media_id FROM media_types WHERE media_type = 'surface water'"
    )[1, 1]

    tsid <- DBI::dbGetQuery(
      con,
      "SELECT timeseries_id
       FROM timeseries
       WHERE parameter_id = $1
         AND location_id = $2
         AND source_fx = 'downloadWSC'
       LIMIT 1;",
      params = list(parameter_id, location_id)
    )[1, 1]

    if (length(tsid) == 0 || is.na(tsid)) {
      new_entry <- data.frame(
        location_id = location_id,
        parameter_id = parameter_id,
        category = "continuous",
        aggregation_type = "instantaneous",
        record_rate = "5 minutes",
        media_id = media_id,
        last_new_data = .POSIXct(Sys.time(), tz = "UTC"),
        share_with = "{public_reader}",
        owner = 1,
        source_fx = "downloadWSC",
        source_fx_args = jsonlite::toJSON(
          list(location = location),
          auto_unbox = TRUE
        )
      )
      dbAppendTableRLS(con, "timeseries", new_entry)
      tsid <- DBI::dbGetQuery(
        con,
        "SELECT timeseries_id
         FROM timeseries
         WHERE location_id = $1
           AND parameter_id = $2
           AND source_fx = 'downloadWSC'
         LIMIT 1;",
        params = list(location_id, parameter_id)
      )[1, 1]
    }

    tsid
  }

  write_hydat_continuous <- function(tsid, data) {
    if (nrow(data) == 0) {
      return(FALSE)
    }

    offset <- DBI::dbGetQuery(
      con,
      "SELECT timezone_daily_calc FROM timeseries WHERE timeseries_id = $1",
      params = list(tsid)
    )[1, 1]
    if (is.na(offset)) {
      offset <- 0L
    }

    data <- data[order(data$date), , drop = FALSE]
    data$datetime <- daily_datetime_utc(data$date, offset)
    data$period <- "P1D"
    data$timeseries_id <- as.integer(tsid)

    start_ts <- as.POSIXct(min(data$date), tz = "UTC") -
      as.integer(offset) * 3600
    end_ts <- as.POSIXct(max(data$date) + 1, tz = "UTC") -
      as.integer(offset) * 3600

    high_frequency_dates <- DBI::dbGetQuery(
      con,
      "SELECT local_date AS date
       FROM (
         SELECT
           (mc.datetime + make_interval(hours => $2))::date AS local_date,
           mc.datetime,
           mc.period
         FROM measurements_continuous mc
         WHERE mc.timeseries_id = $1
           AND mc.datetime >= $3
           AND mc.datetime < $4
       ) x
       GROUP BY local_date
       HAVING BOOL_OR(
         x.period IS DISTINCT FROM interval '1 day'
         OR x.datetime IS DISTINCT FROM continuous.local_noon_to_utc(
           x.local_date,
           $2
         )
       );",
      params = list(tsid, as.integer(offset), start_ts, end_ts)
    )$date
    high_frequency_dates <- as.Date(high_frequency_dates)

    existing_daily <- DBI::dbGetQuery(
      con,
      "SELECT
         (mc.datetime + make_interval(hours => $2))::date AS date,
         mc.value
       FROM measurements_continuous mc
       WHERE mc.timeseries_id = $1
         AND mc.datetime >= $3
         AND mc.datetime < $4
         AND mc.period = interval '1 day'
         AND mc.datetime = continuous.local_noon_to_utc(
           (mc.datetime + make_interval(hours => $2))::date,
           $2
         );",
      params = list(tsid, as.integer(offset), start_ts, end_ts)
    )
    existing_daily$date <- as.Date(existing_daily$date)

    compare <- merge(
      data[, c("date", "value")],
      existing_daily,
      by = "date",
      all.x = TRUE,
      suffixes = c("_hydat", "_db")
    )
    compare <- compare[
      !(compare$date %in% high_frequency_dates),
      ,
      drop = FALSE
    ]
    mismatch <- compare[
      is.na(compare$value_db) |
        is.na(compare$value_hydat) != is.na(compare$value_db) |
        compare$value_hydat != as.numeric(compare$value_db),
      ,
      drop = FALSE
    ]

    if (nrow(mismatch) == 0) {
      return(FALSE)
    }

    start_update <- min(mismatch$date)
    data <- data[data$date >= start_update, , drop = FALSE]
    to_write <- data[
      !(data$date %in% high_frequency_dates),
      ,
      drop = FALSE
    ]
    delete_start <- as.POSIXct(start_update, tz = "UTC") -
      as.integer(offset) * 3600

    active <- dbTransBegin(con)
    ok <- FALSE
    tryCatch(
      {
        deleted <- DBI::dbExecute(
          con,
          "DELETE FROM measurements_continuous mc
           WHERE mc.timeseries_id = $1
             AND mc.datetime >= $2
             AND mc.datetime < $3
             AND mc.period = interval '1 day'
             AND mc.datetime = continuous.local_noon_to_utc(
               (mc.datetime + make_interval(hours => $4))::date,
               $4
             );",
          params = list(tsid, delete_start, end_ts, as.integer(offset))
        )

        if (nrow(to_write) > 0) {
          dbAppendTableRLS(
            con,
            "measurements_continuous",
            to_write[, c(
              "datetime",
              "value",
              "period",
              "timeseries_id",
              "imputed"
            )]
          )

          adjust_approval(
            con,
            tsid,
            to_write[, c("datetime", "approval")]
          )
          adjust_qualifier(
            con,
            tsid,
            to_write[, c("datetime", "qualifier")]
          )
          adjust_owner(
            con,
            tsid,
            to_write[, c("datetime", "owner")]
          )
          adjust_grade(
            con,
            tsid,
            to_write[, c("datetime", "grade")]
          )
          adjust_contributor(
            con,
            tsid,
            to_write[, c("datetime", "contributor")]
          )
        }

        DBI::dbExecute(
          con,
          "UPDATE timeseries SET last_new_data = NOW() WHERE timeseries_id = $1",
          params = list(tsid)
        )
        ok <- deleted > 0 || nrow(to_write) > 0

        if (active) {
          DBI::dbExecute(con, "COMMIT;")
        }
      },
      error = function(e) {
        if (active) {
          DBI::dbExecute(con, "ROLLBACK;")
        }
        stop(e)
      }
    )

    ok
  }

  message(
    "Updating database with information in HYDAT due to new HYDAT version or request to force update..."
  )

  for (location in unique(all_timeseries$location)) {
    location_id <- all_timeseries[
      all_timeseries$location == location,
      "location_id"
    ][1]

    for (param_name in c("water flow", "water level")) {
      tryCatch(
        {
          hydat_data <- fetch_hydat_daily(location, param_name)
          if (nrow(hydat_data) == 0) {
            next
          }

          tsid <- ensure_wsc_timeseries(location, location_id, param_name)
          changed <- write_hydat_continuous(tsid, hydat_data)
          if (changed) {
            message(
              "Updated HYDAT ",
              param_name,
              " daily means for WSC location ",
              location,
              " in measurements_continuous."
            )
          }
        },
        error = function(e) {
          warning(
            "update_hydat: Something went wrong when trying to add ",
            param_name,
            " data for location ",
            location,
            ". Returned error: ",
            e$message
          )
        }
      )
    }
  }

  try(
    {
      DBI::dbExecute(
        con,
        "UPDATE internal_status SET value = $1 WHERE event = 'HYDAT_version'",
        params = list(as.character(local_hydat))
      )
    },
    silent = TRUE
  )

  message("Completed update of HYDAT related data.")
  new_hydat
}
