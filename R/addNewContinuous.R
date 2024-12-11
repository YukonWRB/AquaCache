#' Add new data to the measurements_continuous or measurements_calculated_daily tables
#'
#' @param tsid The timeseries_id to which the data will be appended. This is a required parameter.
#' @param df A data.frame containing the data. Must have columns named 'datetime' OR 'date', and 'value' at minimum. If 'datetime' is present data will be appended to measurements_continuous, otherwise 'date' will be used to append to measurements_calculated_daily. Other optional columns are 'owner', 'contributor', 'approval', 'grade', 'qualifier', 'imputed'; see the `adjust_` series of functions to see how these are used.
#' @param target One of 'continuous' or 'daily'. Default is 'continuous'. You would only want to append directly to the 'daily' table if adding pre-calculated daily means with the aim of adding higher frequency data to the 'continuous' table later. As an extra check, the data.frame passed in argument 'df' must contain a column named 'datetime' or 'date' to match this parameter.
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [AquaConnect()]. If left NULL, a connection will be attempted using AquaConnect() and closed afterwards.
#'
#' @return Nothing; data is added to the database silently.
#' @export
#'

addNewContinuous <- function(tsid, df, target = "continuous", con = NULL) {
  
  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }

  # Check that the timeseries_id is valid
  check <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id FROM timeseries WHERE timeseries_id = ", tsid))[1,1]
  if (is.na(check)) {
    stop("The timeseries_id you specified does not exist.")
  }
  
  # Check if we have a 'date' or 'datetime' column
  if (!("datetime" %in% names(df) || "date" %in% names(df))) {
    stop("The data.frame must contain a column named 'datetime' or 'date'.")
  }
  if (!("value" %in% names(df))) {
    stop("The data.frame must contain a column named 'value'.")
  }
  
  grade_unknown <- DBI::dbGetQuery(con, "SELECT grade_type_id FROM grade_types WHERE grade_type_code = 'UNK';")[1,1]
  if (is.na(grade_unknown)) {
    stop("addNewContinuous: Could not find grade type 'Unknown' in the database.")
  }
  approval_unknown <- DBI::dbGetQuery(con, "SELECT approval_type_id FROM approval_types WHERE approval_type_code = 'UNK';")[1,1]
  if (is.na(approval_unknown)) {
    stop("addNewContinuous: Could not find approval type 'Unknown' in the database.")
  }
  qualifier_unknown <- DBI::dbGetQuery(con, "SELECT qualifier_type_id FROM qualifier_types WHERE qualifier_type_code = 'UNK';")[1,1]
  if (is.na(qualifier_unknown)) {
    stop("addNewContinuous: Could not find qualifier type 'Unknown' in the database.")
  }
  
  
  info <- DBI::dbGetQuery(con, paste0("SELECT period_type, record_rate, owner, active, end_datetime FROM timeseries WHERE timeseries_id = ", tsid, ";"))
  if (is.na(info$end_datetime)) {
    last_data_point <- NA
  } else {
    last_data_point = info$end_datetime + 1
  }
  df <- df[!is.na(df$value) , ]
  
  if ("owner" %in% names(df)) {
    if (!is.na(info$owner)) {
      df$owner[is.na(df$owner)] <- info$owner
    }
  } else {
    if (!is.na(info$owner)) {
      df$owner <- info$owner
    }
  }
  
  if (!("approval" %in% names(df))) {
    df$approval <- approval_unknown
  }
  
  if (!("grade" %in% names(df))) {
    df$grade <- grade_unknown
  }
  
  if (!("qualifier" %in% names(df))) {
    df$qualifier <- qualifier_unknown
  }
  
  if (!("imputed" %in% names(df))) {
    df$imputed <- FALSE
  }
  
  # Append the data ##########################################################
  ## Append for measurements_continuous #####
  if ('datetime' %in% names(df)) {
    if (!inherits(df$datetime, "POSIXct")) {
      stop("The 'datetime' column must be in POSIXct format.")
    }
    if (target == "daily") {
      stop("The 'target' parameter is set to 'daily' but the data.frame contains a 'datetime' column. Please set 'target' to 'continuous'.")
    }
    
    if (is.na(last_data_point)) {
      last_data_point <- DBI::dbGetQuery(con, paste0("SELECT MAX(datetime) FROM measurements_continuous WHERE timeseries_id = ", tsid, ";"))[1,1]
    }
    if (is.na(last_data_point)) {
      last_data_point <- min(df$datetime)  + 1
    }
    
    commit_fx <- function(con, df, last_data_point, tsid) {
      
      adjust_grade(con, tsid, df[, c("datetime", "grade")])
      adjust_approval(con, tsid, df[, c("datetime", "approval")])
      adjust_qualifier(con, tsid, df[, c("datetime", "qualifier")])
      if ("owner" %in% names(df)) {
        adjust_owner(con, tsid, df[, c("datetime", "owner")])
      }
      if ("contributor" %in% names(df)) {
        adjust_contributor(con, tsid, df[, c("datetime", "contributor")])
      }
      df$timeseries_id <- tsid
      # Drop columns no longer necessary
      df <- df[, c("datetime", "value", "timeseries_id", "imputed")]
      
      #assign a period to the data
      if (info$period_type == "instantaneous") { #Period is always 0 for instantaneous data
        df$period <- "00:00:00"
      } else if ((info$period_type != "instantaneous") & !("period" %in% names(df))) { #period_types of mean, median, min, max should all have a period
        df <- calculate_period(data = df, timeseries_id = tsid, con = con)
      } else { #Check to make sure that the supplied period can actually be coerced to a period
        check <- lubridate::period(unique(df$period))
        if (NA %in% check) {
          df$period <- NA
        }
      }
      
      if (min(df$datetime) < last_data_point - 1) {
        DBI::dbExecute(con, paste0("DELETE FROM measurements_continuous WHERE datetime >= '", min(df$datetime), "' AND timeseries_id = ", tsid, ";"))
      }
      DBI::dbAppendTable(con, "measurements_continuous", df)
      #make the new entry into table timeseries
      DBI::dbExecute(con, paste0("UPDATE timeseries SET end_datetime = '", max(df$datetime),"', last_new_data = '", .POSIXct(Sys.time(), "UTC"), "' WHERE timeseries_id = ", tsid, ";"))
    }
    
    if (!attr(con, "active_transaction")) {
      DBI::dbBegin(con)
      attr(con, "active_transaction") <- TRUE
      tryCatch({
        commit_fx(con, df, last_data_point, tsid)
        DBI::dbCommit(con)
        attr(con, "active_transaction") <- FALSE
      }, error = function(e) {
        DBI::dbRollback(con)
        attr(con, "active_transaction") <<- FALSE
        warning("addNewContinuous: Failed to append new data. Returned error '", e$message, "'.")
      })
      
    } else {
      commit_fx(con, df, last_data_point, tsid)
    }
    
    ## Append for measurements_calculated_daily #####
  } else if ('date' %in% names(df)) {
    if (!inherits(df$date, "Date")) {
      stop("The 'date' column must be in Date format.")
    }
    if (target == "continuous") {
      stop("The 'target' parameter is set to 'continuous' but the data.frame contains a 'date' column. Please set 'target' to 'daily'.")
    }
    
    if (is.na(last_data_point)) {
      last_data_point <- DBI::dbGetQuery(con, paste0("SELECT MAX(date) FROM measurements_calculated_daily WHERE timeseries_id = ", tsid, ";"))[1,1]
    }
    if (is.na(last_data_point)) {
      last_data_point <- min(df$date)  + 1
    }
    
    commit_fx <- function(con, df, last_data_point, tsid) {
      
      adjust_grade(con, tsid, df[, c("date", "grade")])
      adjust_approval(con, tsid, df[, c("date", "approval")])
      adjust_qualifier(con, tsid, df[, c("date", "qualifier")])
      if ("owner" %in% names(df)) {
        adjust_owner(con, tsid, df[, c("date", "owner")])
      }
      if ("contributor" %in% names(df)) {
        adjust_contributor(con, tsid, df[, c("date", "contributor")])
      }
      
      df$timeseries_id <- tsid
      # Drop columns no longer necessary
      df <- df[, c("date", "value", "timeseries_id", "imputed")]
      
      if (min(df$date) < last_data_point - 1) {
        DBI::dbExecute(con, paste0("DELETE FROM measurements_calculated_daily WHERE date >= '", min(df$date), "' AND timeseries_id = ", tsid, ";"))
      }
      DBI::dbAppendTable(con, "measurements_calculated_daily", df)
      #make the new entry into table timeseries
      if (max(df$date) > last_data_point) {
        DBI::dbExecute(con, paste0("UPDATE timeseries SET end_datetime = '", max(df$date),"', last_new_data = '", .POSIXct(Sys.time(), "UTC"), "' WHERE timeseries_id = ", tsid, ";"))
      } else {
      DBI::dbExecute(con, paste0("UPDATE timeseries SET last_new_data = '", .POSIXct(Sys.time(), "UTC"), "' WHERE timeseries_id = ", tsid, ";"))
      }
    }
    
    if (!attr(con, "active_transaction")) {
      DBI::dbBegin(con)
      attr(con, "active_transaction") <- TRUE
      tryCatch({
        commit_fx(con, df, last_data_point, tsid)
        DBI::dbCommit(con)
        attr(con, "active_transaction") <- FALSE
      }, error = function(e) {
        DBI::dbRollback(con)
        attr(con, "active_transaction") <<- FALSE
        warning("addNewContinuous: Failed to append new data. Returned error '", e$message, "'.")
      })
      
    } else {
      commit_fx(con, df, last_data_point, tsid)
    }
    
  } # End of appending to 'continuous' table
  
}

