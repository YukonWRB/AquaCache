#' Weekly update of hydro database
#'
#' The weekly update function pulls and replaces a large tranche of Water Survey of Canada and Aquarius data to ensure incorporation of any edits. By default, all real-time water survey data currently available online is replaced, as is any Aquarius data not previously labelled as "approved". Daily means and statistics are recalculated for any potentially affected days in the daily tables.
#'
#' @param path The path to the local hydro SQLite database, with extension.
#' @param WSC_range The starting date from which to pull real-time WSC data from the web and replace in the local database. Default is max possible days.
#' @param aquarius TRUE if you are fetching data from Aquarius, in which case you should also check the next six parameters. FALSE will only populate with WSC data.
#' @param aquarius_range Should only unapproved (locked) data be replaced, or all available data? Select from "all" or "unapproved". Default is "unapproved".
#' @param stage The name of the stage(level) timeseries as it appears in Aquarius, if it exists, in the form Parameter.Label. All stations must have the same names. !This DOES NOT apply to WSC stations mirrored in Aquarius.
#' @param discharge The name of the discharge(flow) timeseries as it appears in Aquarius, if it exists, in the form Parameter.Label. All stations must have the same names. !This DOES NOT apply to WSC stations mirrored in Aquarius.
#' @param SWE The name of the snow water equivalent timeseries as it appears in Aquarius, if it exists, in the form Parameter.Label. All stations must have the same names.
#' @param depth The name of the snow depth timeseries as it appears in Aquarius, if it exists, in the form Parameter.Label. All stations must have the same names.
#' @param server The URL to your Aquarius server, if needed. Note that your credentials must be in your .Renviron profile: see ?WRBtools::aq_download.
#'
#' @return Updated entries in the hydro database.
#' @import tidyhydat.ws
#' @export
#'
#'
#'

hydro_update_weekly <- function(path, WSC_range = Sys.Date()-577, aquarius = TRUE, aquarius_range = "unapproved", stage = "Stage.Publish", discharge = "Discharge.Publish", SWE = "SWE.Corrected", depth = "Snow Depth.TempCompensated.Corrected", server = "https://yukon.aquaticinformatics.net/AQUARIUS")
{
  library(tidyhydat.ws) #This needs to be removed once tidyhydat.ws is updated with properly formated package data. Same for "require" call in Description and @import in function headers.
  on.exit(detach("package:tidyhydat.ws", unload= TRUE))

  if (!(aquarius_range %in% c("all", "unapproved"))){
    stop("The parameter aquarius_range must be either 'all' or 'unapproved'")
  }

  if (aquarius){
    if (is.null(Sys.getenv("AQPASS"))){
      stop("Your Aquarius password must be available in the .Renviron file in the form AQPASS='yourpassword'")
    }
    if (is.null(Sys.getenv("AQUSER"))){
      stop("Your Aquarius user name must be available in the .Renviron file in the form AQUSER='yourusername'")
    }
  }
  if (is.null(Sys.getenv("WS_USRNM"))){
    stop("Your WSC user name must be available in the .Renviron file in the form WS_USRNM='yourusername'")
  }
  if (is.null(Sys.getenv("WS_PWD"))){
    stop("Your WSC password must be available in the .Renviron file in the form WS_PWD='yourpassword'")
  }

  hydro <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(hydro))


  recalculate <- data.frame()
  locations <- DBI::dbGetQuery(hydro, "SELECT * FROM locations")
  for (i in 1:nrow(locations)){
    loc <- locations$location[i]
    type <- locations$data_type[i]
    table_name <- if (type == "SWE") "snow_pillow_SWE" else if (type == "depth") "snow_pillow_depth" else type
    operator <- locations$operator[i]
    units <- if (type == "SWE") "mm SWE" else if (type == "depth") "cm" else if (type == "level") "m" else if (type == "flow") "m3/s"

    tryCatch({
      if (operator == "WRB" & aquarius){
        if (aquarius_range == "unapproved"){
          first_unapproved <- DBI::dbGetQuery(hydro, paste0("SELECT MIN(datetime_UTC) FROM ", table_name, "_realtime WHERE location = '", locations$location[i], "' AND NOT approval = 'approved'"))[1,]
          data <- WRBtools::aq_download(loc_id = locations$location[i], ts_name = SWE, start = first_unapproved, server = server)
        } else if (aquarius_range == "all"){
          data <- WRBtools::aq_download(loc_id = locations$location[i], ts_name = SWE, server = server)
        }
        ts <- data.frame("location" = locations$location[i], "datetime_UTC" = as.character(data$timeseries$timestamp_UTC), "value" = data$timeseries$value, "units" = units, "grade" = data$timeseries$grade_description, "approval" = data$timeseries$approval_description)

      } else if (operator == "WSC"){
        token <- suppressMessages(tidyhydat.ws::token_ws())
        data <- suppressMessages(tidyhydat.ws::realtime_ws(locations$location[i], if (type == "flow") 47 else if (type == "level") 46, start_date = WSC_range,  token = token))
        data <- data[,c(2,4,1)]
        names(data) <- c("datetime_UTC", "value", "location")
        data$datetime_UTC <- as.character(data$datetime_UTC)
        data$approval <- "preliminary"
        data$units <- units
        ts <- data
      }

      if (nrow(ts) > 0){
        DBI::dbExecute(hydro, paste0("DELETE FROM ", table_name, "_realtime WHERE datetime_UTC BETWEEN '", min(ts$datetime_UTC), "' AND '", max(ts$datetime_UTC), "'"))
        DBI::dbAppendTable(hydro, paste0(table_name, "_realtime"), ts)
        #make the new entry into table locations
        DBI::dbExecute(hydro, paste0("UPDATE locations SET end_datetime = '", as.character(max(ts$datetime_UTC)),"' WHERE location = '", locations$location[i], "' AND data_type = '", type, "'"))

        recalc <- data.frame("start" = substr(min(ts$datetime_UTC), 1, 10), "end" = substr(max(ts$datetime_UTC), 1, 10), "location" = locations$location[i], "data_type" = locations$data_type[i], "operator" = operator)
        recalculate <- rbind(recalculate, recalc)
      }
    }, error = function(e) {
      print(paste0("Failed on location ", locations$location[i], " and data type ", locations$data_type[i]))
    }
    )
  }

    leap_list <- (seq(1800, 2100, by = 4))
    for (i in 1:nrow(recalculate)){
      loc <- recalculate$location[i]
      type <- recalculate$data_type[i]
      table_name <- if (type == "SWE") "snow_pillow_SWE" else if (type == "depth") "snow_pillow_depth" else type
      operator <- recalculate$operator[i]
      if (operator == "WSC"){
        hy_range <- tidyhydat::hy_stn_data_range("09EA004")
        Data_type <- if (type == "flow") "Q" else "H"
        hy_max <- hy_range[hy_range$DATA_TYPE == Data_type, ]$Year_to
        hy_max <- as.Date(paste0(hy_max, "-12-31"))+1
        start <- as.character(min(c(hy_max, recalculate$start[i])))
      } else {
        start <- recalculate$start[i]
      }
      end <- recalculate$end[i]


      gap_realtime <- DBI::dbGetQuery(hydro, paste0("SELECT * FROM ", table_name, "_realtime WHERE location = '", loc, "' AND datetime_UTC BETWEEN '", start, " 00:00:00' AND '", end, " 23:59:59:999'"))
      if (nrow(gap_realtime) > 0){
        gap_realtime <- gap_realtime %>%
          dplyr::group_by(lubridate::year(.data$datetime_UTC), lubridate::yday(.data$datetime_UTC)) %>%
          dplyr::summarize(date = mean(lubridate::date(.data$datetime_UTC)),
                           value = mean(.data$value),
                           grade = sort(.data$grade,decreasing=TRUE)[1],
                           approval = sort(.data$approval, decreasing=TRUE)[1],
                           .groups = "drop")
        gap_realtime <- gap_realtime[,c(3:6)]
        names(gap_realtime) <- c("date", "value", "grade", "approval")
        gap_realtime <- fasstr::fill_missing_dates(gap_realtime, "date", pad_ends = FALSE)
        gap_realtime$units <- if (type == "level") "m" else if (type == "flow") "m3/s" else if (type == "SWE") "mm SWE" else if (type == "depth") "cm"
        gap_realtime$location <- loc
        gap_realtime$date <- as.character(gap_realtime$date)
        DBI::dbExecute(hydro, paste0("DELETE FROM ", table_name, "_daily WHERE date >= '", min(gap_realtime$date), "' AND location = '", loc, "'"))
        DBI::dbAppendTable(hydro, paste0(table_name, "_daily"), gap_realtime)
      }

      # Now calculate stats where they are missing
      all_stats <- DBI::dbGetQuery(hydro, paste0("SELECT date, value FROM ", table_name, "_daily WHERE location = '", loc, "'"))
      missing_stats <- DBI::dbGetQuery(hydro, paste0("SELECT * FROM ", table_name, "_daily WHERE location = '", loc, "' AND max IS NULL"))

      # Remove Feb. 29 data as it would mess with the percentiles; save the missing_stats ones and add them back in later. This is also important as it prevents deleting Feb 29 data in the daily table without replacing it.
      feb_29 <- missing_stats[(lubridate::month(missing_stats$date) == "2" & lubridate::mday(missing_stats$date) == "29"), , drop = FALSE]
      missing_stats <- missing_stats[!(lubridate::month(missing_stats$date) == "2" & lubridate::mday(missing_stats$date) == "29"), , drop = FALSE]
      all_stats <- all_stats[!(lubridate::month(all_stats$date) == "2" & lubridate::mday(all_stats$date) == "29"), , drop = FALSE]
      # Create a dayofyear column that pretends Feb 29 doesn't exist; all years have 365 days
      missing_stats <- missing_stats %>% dplyr::mutate(dayofyear = ifelse(lubridate::year(.data$date) %in% leap_list,
                                                                          ifelse(lubridate::month(.data$date) <= 2,
                                                                                 lubridate::yday(.data$date),
                                                                                 lubridate::yday(.data$date) - 1),
                                                                          lubridate::yday(.data$date)))
      all_stats <- all_stats %>% dplyr::mutate(dayofyear = ifelse(lubridate::year(.data$date) %in% leap_list,
                                                                  ifelse(lubridate::month(.data$date) <= 2,
                                                                         lubridate::yday(.data$date),
                                                                         lubridate::yday(.data$date) - 1),
                                                                  lubridate::yday(.data$date)))

      #selects only records beginning with the second dayofyear and having values for the second time from all_stats (those for which stats can be calculated)
      missing_stats <- missing_stats[order(missing_stats[ , "date"]) , ]
      all_stats <- all_stats[order(all_stats[ , "date"]) , ]
      all_stats <- all_stats[!(is.na(all_stats$value)), ]
      duplicated <- all_stats[duplicated(all_stats$dayofyear),]
      missing_stats <- missing_stats[missing_stats$date %in% duplicated$date , ]

      if (nrow(missing_stats) > 0){
        for (j in 1:nrow(missing_stats)){
          date <- missing_stats$date[j]
          doy <- missing_stats$dayofyear[j]
          current <- missing_stats$value[j]
          past <- all_stats[all_stats$dayofyear == doy & all_stats$date < date , ]$value #Importantly, does NOT include the current measurement. A current measure greater than past maximum will rank > 100%
          past <- past[!is.na(past)]
          if (length(past) >= 1){
            missing_stats$max[j] <- max(past) #again, NOT including current measurement
            missing_stats$min[j] <- min(past)
            missing_stats$QP90[j] <- stats::quantile(past, 0.90)
            missing_stats$QP75[j] <- stats::quantile(past, 0.75)
            missing_stats$QP50[j] <- stats::quantile(past, 0.50)
            missing_stats$QP25[j] <- stats::quantile(past, 0.25)
            missing_stats$QP10[j] <- stats::quantile(past, 0.10)
            if (length(past) > 1 & !is.na(current)){ #need at least 2 measurements to calculate a percent historic!
              missing_stats$percent_historic_range[j] <- ((current - min(past)) / (max(past) - min(past))) * 100
            }
          }
        }
        missing_stats <- subset(missing_stats, select=-c(dayofyear)) #remove column not in database table

        #Assign values to Feb 29 that are between Feb 28 and March 1. Doesn't run on the 29, 1st, or 2nd to wait for complete stats on the 1st. Unfortunately this means that initial setups done on those days will not calculate Feb 29!
        if (nrow(feb_29) > 0 & !(substr(as.character(as.Date(.POSIXct(Sys.time(), "UTC"))), 6, 10) %in% c("02-29", "03-01,", "03-02"))) {
          for (k in 1:nrow(feb_29)){
            date <- as.Date(feb_29$date[k])
            before <- missing_stats[missing_stats$date == date - 1 , ]
            after <- missing_stats[missing_stats$date == date + 1 , ]
            feb_29$percent_historic_range[k] <- mean(c(before$percent_historic_range, after$percent_historic_range))
            feb_29$max[k] <- mean(c(before$max, after$max))
            feb_29$min[k] <- mean(c(before$min, after$min))
            feb_29$QP90[k] <- mean(c(before$QP90, after$QP90))
            feb_29$QP75[k] <- mean(c(before$QP75, after$QP75))
            feb_29$QP50[k] <- mean(c(before$QP50, after$QP50))
            feb_29$QP25[k] <- mean(c(before$QP25, after$QP25))
            feb_29$QP10[k] <- mean(c(before$QP10, after$QP10))
          }
        }
        missing_stats <- rbind(missing_stats, feb_29)
        dates <- missing_stats$date

        DBI::dbExecute(hydro, paste0("DELETE FROM ", table_name, "_daily WHERE location = '", loc, "' AND date BETWEEN '", min(missing_stats$date), "' AND '", max(missing_stats$date), "' AND max IS NULL")) #The AND max IS NULL part prevents deleting entries within the time range that have not been recalculated, as would happen if, say, a Feb 29 is calculated on March 3rd. Without that condition, March 1 and 2 would also be deleted but are not part of missing_stats due to initial selection criteria of missing_stats.
        DBI::dbAppendTable(hydro, paste0(table_name, "_daily"), missing_stats)
      }
    } # End of for loop calculating means and stats for each station in locations table

} #End of function
