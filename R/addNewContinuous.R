#' Add new data to the measurements_continuous or measurements_calculated_daily tables
#' 
#' This function can be used to append new contiuous type data directly to the database without downloading it from a remote source. Differs from [getNewContinuous()] as the later is used to pull data from a remote before append.
#'
#' @param tsid The timeseries_id to which the data will be appended. This is a required parameter.
#' @param df A data.frame containing the data. Must have columns named 'datetime' and 'value' at minimum. Other optional columns are 'owner', 'contributor', 'approval', 'grade', 'qualifier', 'imputed'; see the `adjust_` series of functions to see how these are used.
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [AquaConnect()]. If left NULL, a connection will be attempted using AquaConnect() and closed afterwards.
#'
#' @return Nothing; data is added to the database silently.
#' @export
#'

addNewContinuous <- function(tsid, df, con = NULL) {
  
  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }
  
  # Check that the timeseries_id is valid
  check <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id FROM timeseries WHERE timeseries_id = ", tsid))[1,1]
  if (is.na(check)) {
    stop("The timeseries_id you specified does not exist.")
  }
  
  # # Check if we have a 'date' or 'datetime' column
  # if (!("datetime" %in% names(df) || "date" %in% names(df))) {
  #   stop("The data.frame must contain a column named 'datetime' or 'date'.")
  # }
  # Ensure there's a 'datetime' column
  if (!("datetime" %in% names(df))) {
    stop("The data.frame must contain a column named 'datetime'.")
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
  
  
  info <- DBI::dbGetQuery(con, paste0("SELECT at.aggregation_type, t.default_owner AS owner, t.active, t.end_datetime FROM timeseries AS t JOIN aggregation_types at ON at.aggregation_type_id = t.aggregation_type_id WHERE timeseries_id = ", tsid, ";"))
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
  if (!inherits(df$datetime, "POSIXct")) {
    stop("The 'datetime' column must be in POSIXct format.")
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
    if (info$aggregation_type == "instantaneous") { #Period is always 0 for instantaneous data
      df$period <- "00:00:00"
    } else if ((info$aggregation_type != "instantaneous") & !("period" %in% names(df))) { #aggregation_types of mean, median, min, max should all have a period
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
    
    calculate_stats(con = con, timeseries_id = tsid, start_recalc = min(df$datetime))
    
    #make the new entry into table timeseries
    DBI::dbExecute(con, paste0("UPDATE timeseries SET end_datetime = '", max(df$datetime),"', last_new_data = '", .POSIXct(Sys.time(), "UTC"), "' WHERE timeseries_id = ", tsid, ";"))
  }
  
  activeTrans <- dbTransBegin(con) # returns TRUE if a transaction is not already in progress and was set up, otherwise commit will happen in the original calling function.
  if (activeTrans) {
    tryCatch({
      commit_fx(con, df, last_data_point, tsid)
      DBI::dbExecute(con, "COMMIT;")
    }, error = function(e) {
      DBI::dbExecute(con, "ROLLBACK;")
      warning("addNewContinuous: Failed to append new data. Returned error '", e$message, "'.")
    })
    
  } else {
    commit_fx(con, df, last_data_point, tsid)
  }
  
}

