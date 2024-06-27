#' Impute missing values
#'
#' @description
#' ## Note
#' See function [imputeMissing()] for imputation using a nearby timeseries or directly using linear or spline interpolation. Using nearby timeseries is most accurate if in near enough proximity, while spline interpolation is most accurate for filling in small gaps.
#' 
#' Uses random forest modelling to predict long periods of missing data using one or more user-selected timeseries or any parameter. User will be shown timeseries in the within the selected radius of the target timeseries and will be prompted to make groups on which to rapidly train the model(s). The best performing group and model combination will be automatically selected and used to predict the missing values.
#'
#' @param tsid The target timeseries_id.
#' @param radius The radius in kilometers within which to search for similar timeseries.
#' @param predict_range A vector of length two (date or POSIXct), comprised of the start and end date/datetime between which to predict missing values.
#' @param train_range A vector of length two (date or POSIXct), comprised of the start and end date/datetime for model training data. If left NULL values present in predict_range will be used. If specified to a datetime range outside of predict_range, existing values in predict_range will still be used to improve predictions if available.
#' @param extra_params Optional extra parameters to consider when imputing values. For example, if predicting flow you may also want to consider level locations.
#' @param imputed Should already imputed data be imputed again?
#' @param daily Should the imputation be done on a the daily table? Even if set to TRUE this will only apply if there are no entries in table measurements_continuous prior to the end of the predict_range.
#' @param mice Should [mice::mice()] be used to impute missing values within the training dataset? If set to TRUE, additional models will be trained on the actual AND imputed values, and shown to the user for selection. This can add significant time to the process, so use judiciously.
#' @param con A connection to the database.
#'
#' @return Imputed values added to the database.
#' @export
#'

predictMissing_mice <- function(tsid, radius, predict_range, train_range = NULL, extra_params = NULL, imputed = TRUE, daily = FALSE, mice = FALSE, con = AquaCacheCon(silent = TRUE)) {
  
  on.exit(DBI::dbDisconnect(con))
  
  rlang::check_installed("randomForest", reason = "to impute missing data using random forests.")
  if (mice) {
    rlang::check_installed("mice", reason = "to impute missing data using multiple imputation.")
  }
  
  returns <- list() #holds the objects to return
  
  # parameter checks and formatting ############################################################
  if (length(predict_range) != 2 || !(length(train_range) %in% c(0, 2))) {
    stop("predict_range must be a vector of length 2, date, character, or posixct. Same for train_range unless you leave it to NULL.")
  }
  if (is.null(train_range)) {
    train_range <- predict_range
    no_extra <- TRUE
  } else {
    no_extra <- FALSE
  }
  
  if (inherits(predict_range, "date") || inherits(predict_range, "character")) {
    predict_range <- as.POSIXct(predict_range, tz = "UTC")
  } else if (inherits(predict_range, "POSIXct")) {
    attr(predict_range, "tzone") <- "UTC"
  } else {
    stop("predict_range must be a vector of length 2, date, character, or posixct.")
  }
  
  if (inherits(train_range, "date") || inherits(train_range, "character")) {
    train_range <- as.POSIXct(train_range, tz = "UTC")
  } else if (inherits(train_range, "POSIXct")) {
    attr(train_range, "tzone") <- "UTC"
  } else {
    stop("train_range must be a vector of length 2, date, character, or posixct, or left NULL.")
  }
  
  start_predict <- predict_range[1]
  end_predict <- predict_range[2]
  start_train <- train_range[1]
  end_train <- train_range[2]
  
  
  entry <- DBI::dbGetQuery(con, paste0("SELECT location, parameter, category, period_type, record_rate FROM timeseries WHERE timeseries_id = ", tsid, ";"))
  if (entry$category != "continuous") {
    stop("This function is not designed to work with discrete category timeseries.")
  }
  returns[["target_timeseries"]] <- entry

  # Determine if can use daily or not ############################################################
  # Check if there are any entries in the measurements_continuous table at any time before the end of the predict_range. If so, don't use the calculated_daily table even if the user wants to give a warning).  
  if (!daily) {
    if (imputed) {
      exist.cont <- DBI::dbGetQuery(con, paste0("SELECT datetime, value, period FROM measurements_continuous WHERE timeseries_id = ", tsid, " AND datetime >= '", start_predict, "' AND datetime <= '", end_predict, "';"))
    } else {
      exist.cont <- DBI::dbGetQuery(con, paste0("SELECT datetime, value, period FROM measurements_continuous WHERE timeseries_id = ", tsid, " AND datetime >= '", start_predict, "' AND datetime <= '", end_predict, "' AND imputed = FALSE;"))
    }
  } else { # daily is TRUE. Check if the earliest entry in the measurements_continuous table is after the end of the predict_range.
    check <- DBI::dbGetQuery(con, paste0("SELECT min(datetime) FROM measurements_continuous WHERE timeseries_id = ", tsid, ";"))
    if (check$min < end_predict) {
      daily <- FALSE
      message("There are entries in the measurements_continuous table before the end of the predict_range. Imputation will be done on the continuous table.")
      if (imputed) {
        exist.cont <- DBI::dbGetQuery(con, paste0("SELECT datetime, value, period FROM measurements_continuous WHERE timeseries_id = ", tsid, " AND datetime >= '", start_predict, "' AND datetime <= '", end_predict, "';"))
      } else {
        exist.cont <- DBI::dbGetQuery(con, paste0("SELECT datetime, value, period FROM measurements_continuous WHERE timeseries_id = ", tsid, " AND datetime >= '", start_predict, "' AND datetime <= '", end_predict, "' AND imputed = FALSE;"))
      }
    }
  }
  
  # Get the outcome values for the training range and the values in the predict range ############################################################
  if (daily) {
    outcome_values <- DBI::dbGetQuery(con, paste0("SELECT date, value FROM calculated_daily WHERE timeseries_id = ", tsid, " AND date > '", as.Date(start_train), "' AND date < '", as.Date(end_train), "' AND value IS NOT NULL;"))
    if (nrow(outcome_values) == 0) {
      stop("There are no usable entries for the given training datetime range to use as outcome variables. Try a different train_range.")
    }
    if (nrow(outcome_values) < 500) { #expand the training range to include more data
      while (nrow(outcome_values) < 500) {
        start_train <- start_train - 7*24*60*60
        end_train <- end_train + 7*24*60*60
        outcome_values <- DBI::dbGetQuery(con, paste0("SELECT date, value FROM calculated_daily WHERE timeseries_id = ", tsid, " AND date >= '", as.Date(start_train), "' AND date <= '", as.Date(end_train), "' AND value IS NOT NULL;"))
      }
    }
    # Add in any existing values for the predicted timeseries within the predict_range
    if (!no_extra) {
      extra <- DBI::dbGetQuery(con, paste0("SELECT date, value FROM calculated_daily WHERE timeseries_id = ", tsid, " AND date >= '", as.Date(start_predict), "' AND date <= '", as.Date(end_predict), "' AND value IS NOT NULL;"))
      outcome_values <- unique(rbind(outcome_values, extra))
    }
    
    outcome_values$datetime <- as.POSIXct(outcome_values$date, tz = "UTC")
    outcome_values$date <- NULL
    
    if (imputed) {
      exist.values <- DBI::dbGetQuery(con, paste0("SELECT date, value FROM calculated_daily WHERE timeseries_id = ", tsid, " AND date >= '", as.Date(start_predict), "' AND date <= '", as.Date(end_predict), "';"))
      # Check to make sure the last value is not NA, since it must be anchored somehow.
      if (is.na(exist.values[exist.values$datetime == max(exist.values$datetime), "value"])) {
        # Find the next non-NA value and add data to that point
        goto_date <- DBI::dbGetQuery(con, paste0("SELECT min(date) FROM calculated_daily WHERE timeseries_id = ", tsid, " AND date > '", as.Date(end_predict), "' AND value IS NOT NULL;"))
        goto_data <- DBI::dbGetQuery(con, paste0("SELECT date, value FROM calculated_daily WHERE timeseries_id = ", tsid, " AND date <= '", as.Date(goto_date), "' AND date > '", as.Date(end_predict), "';"))
        rbind(exist.values, goto_data)
      }
      # Check to make sure the first value is not NA UNLESS the start_predict is prior to the entry in timeseries table
      if (is.na(exist.values[exist.values$datetime == min(exist.values$datetime), "value"])) {
        gofrom_date <- DBI::dbGetQuery(con, "SELECT min(date) FROM calculated_daily WHERE timeseries_id = ", tsid, " AND value IS NOT NULL;")
        if (gofrom_date < start_predict) {
          # Find the next non-NA value and add data to that point
          gofrom_data <- DBI::dbGetQuery(con, paste0("SELECT date, value FROM calculated_daily WHERE timeseries_id = ", tsid, " AND date >= '", as.Date(gofrom_date), "' AND date < '", as.Date(start_predict), "';"))
          rbind(gofrom_data, exist.values)
        }
      }
    } else {
      exist.values <- DBI::dbGetQuery(con, paste0("SELECT date, value FROM calculated_daily WHERE timeseries_id = ", tsid, " AND date >= '", as.Date(start_predict), "' AND date <= '", as.Date(end_predict), "' AND imputed = FALSE;"))
      # Check to make sure the last value is not NA, since it must be anchored somehow.
      if (is.na(exist.values[exist.values$datetime == max(exist.values$datetime), "value"])) {
        # Find the next non-NA value and add data to that point
        goto_date <- DBI::dbGetQuery(con, paste0("SELECT min(date) FROM calculated_daily WHERE timeseries_id = ", tsid, " AND date > '", as.Date(end_predict), "' AND value IS NOT NULL AND imputed = FALSE;"))
        goto_data <- DBI::dbGetQuery(con, paste0("SELECT date, value FROM calculated_daily WHERE timeseries_id = ", tsid, " AND date <= '", as.Date(goto_date), "' AND date > '", as.Date(end_predict), "' AND imputed = FALSE;"))
        rbind(exist.values, goto_data)
      }
      # Check to make sure the first value is not NA UNLESS the start_predict is prior to the entry in timeseries table
      if (is.na(exist.values[exist.values$datetime == min(exist.values$datetime), "value"])) {
        gofrom_date <- DBI::dbGetQuery(con, "SELECT min(date) FROM calculated_daily WHERE timeseries_id = ", tsid, " AND value IS NOT NULL AND imputed = FALSE;")
        if (gofrom_date < start_predict) {
          # Find the next non-NA value and add data to that point
          gofrom_data <- DBI::dbGetQuery(con, paste0("SELECT date, value FROM calculated_daily WHERE timeseries_id = ", tsid, " AND date >= '", as.Date(gofrom_date), "' AND date < '", as.Date(start_predict), "' AND imputed = FALSE;"))
          rbind(gofrom_data, exist.values)
        }
      }
    }
    exist.values$datetime <- as.POSIXct(exist.values$date, tz = "UTC")
    exist.values$date <- NULL
    exist.values$period <- "P1D"
    entry$period_type <- "mean"
    entry$record_rate <- "1 day"
    period <- 86400
    
  } else { # daily is FALSE
    outcome_values <- DBI::dbGetQuery(con, paste0("SELECT datetime, value FROM measurements_continuous WHERE timeseries_id = ", tsid, " AND datetime > '", start_train, "' AND datetime < '", end_train, "';"))
    if (nrow(outcome_values) == 0) {
      stop("There are no usable entries for the given training datetime range to use as outcome variables. Try a different train_range.")
    }
    if (nrow(outcome_values) < 500) { #expand the training range to include more data
      while (nrow(outcome_values) < 500) {
        start_train <- start_train - 7*24*60*60
        end_train <- end_train + 7*24*60*60
        outcome_values <- DBI::dbGetQuery(con, paste0("SELECT datetime, value FROM measurements_continuous WHERE timeseries_id = ", tsid, " AND datetime >= '", start_train, "' AND datetime <= '", end_train, "';"))
      }
    }
    # Add in any existing values for the predicted timeseries within the predict_range
    if (!no_extra) {
      extra <- DBI::dbGetQuery(con, paste0("SELECT datetime, value FROM measurements_continuous WHERE timeseries_id = ", tsid, " AND datetime => '", start_predict, "' AND datetime <= '", end_predict, "';"))
      outcome_values <- unique(rbind(outcome_values, extra))
    }
    
    exist.values <- exist.cont
    # The continuous table can't hold NA values, so there's no need to check that the ts starts and ends with non-NA values
    
    # Find the period associated with the data
    if (nrow(exist.values) < 50) {
      # Expand the range so as to determine the period of the data
      tmp.start <- start_predict
      tmp.end <- end_predict
      while (nrow(exist.cont) < 50) { #expand by a week on either side until enough data exists
        tmp.start <- tmp.start - 7*24*60*60
        tmp.end <- tmp.end + 7*24*60*60
        exist.cont <- DBI::dbGetQuery(con, paste0("SELECT datetime, value period FROM measurements_continuous WHERE timeseries_id = ", tsid, " AND datetime >= '", tmp.start, "' AND datetime <= '", tmp.end, "';"))
      }
    }
    
    exist.cont <- calculate_period(exist.cont, tsid, con)
    periods <- unique(lubridate::period_to_seconds(lubridate::period(exist.cont$period)))
    if (length(periods) > 1) {
      period <- min(periods)
    } else {
      period <- periods
    }
  }
  
  exist.values <- exist.values[, -which(names(exist.values) == "period")]
  
  # Fill in the missing data points with NA. These are the points that will be predicted
  full_dt <- data.frame("datetime" = seq.POSIXt(min(exist.values$datetime), max(exist.values$datetime), by = period))
  exist.values <- merge(full_dt, exist.values, by = "datetime", all.x = TRUE)
  
  returns[["before_prediction"]] <- exist.values
  
  # Find data to use for training ################################################################
  # Find the same parameter at nearby locations, or at same location but with different record_rate
  nrby <- DBI::dbGetQuery(con, paste0("SELECT l.location, l.name, ST_Distance(ST_Transform(v.geom, 3857), ST_Transform((SELECT v2.geom FROM vectors v2 JOIN locations l2 ON v2.geom_id = l2.geom_id WHERE l2.location = '", entry$location, "'), 3857)) AS distance_meters FROM locations l JOIN vectors v ON l.geom_id = v.geom_id WHERE ST_DWithin(ST_Transform(v.geom, 3857), ST_Transform((SELECT v3.geom FROM vectors v3 JOIN locations l3 ON v3.geom_id = l3.geom_id WHERE l3.location = '", entry$location, "' AND v3.geom_type = 'ST_Point'), 3857), ", radius * 1000, ") AND v.geom_type = 'ST_Point';"))
  
  # look for timeseries within the radius (in table nrby) that might have data that can be used to impute the missing values. Check to see if they have data within the range of start_train to end_train, and only if their record_rate is the same of more frequent than the record_rate of the timeseries being imputed.
  duration_mapping <- c('< 1 day' = 1, '1 day' = 2, '1 week' = 3, '4 weeks' = 4, '1 month' = 5, 'year' = 6)
  entry$record_rate_numeric <- duration_mapping[entry$record_rate]
  record_rates <- names(duration_mapping[duration_mapping <= entry$record_rate_numeric])
  
  # Now determine which timeseries have data that can be used for training, and also for imputing the missing values
  if (!is.null(extra_params)) { # if there are extra parameters, look for them as well
    train.check <- DBI::dbGetQuery(con, paste0("SELECT location, timeseries_id, parameter, period_type, operator, record_rate FROM timeseries WHERE location IN ('", paste(nrby$location, collapse = "', '"), "') AND parameter IN ('", paste(entry$parameter, "', '", paste(extra_params, collapse = "', '"), collapse = "', '", sep = ""), "') AND timeseries_id != ", tsid, " AND start_datetime <= '", start_train, "' AND record_rate IN ('", paste(record_rates, collapse = "', '"), "');"))
    predict.check <- DBI::dbGetQuery(con, paste0("SELECT location, timeseries_id, parameter, period_type, operator, record_rate FROM timeseries WHERE location IN ('", paste(nrby$location, collapse = "', '"), "') AND parameter IN ('", paste(entry$parameter, "', '", paste(extra_params, collapse = "', '"), collapse = "', '", sep = ""), "') AND timeseries_id != ", tsid, " AND start_datetime <= '", start_predict, "' AND record_rate IN ('", paste(record_rates, collapse = "', '"), "');"))
  } else { # if there are no extra parameters, look for the same parameter at nearby locations
    train.check <- DBI::dbGetQuery(con, paste0("SELECT location, timeseries_id, location, parameter, period_type, operator, record_rate FROM timeseries WHERE location IN ('", paste(nrby$location, collapse = "', '"), "') AND parameter = '", entry$parameter, "' AND timeseries_id != ", tsid, " AND start_datetime <= '", start_train, "' AND record_rate IN ('", paste(record_rates, collapse = "', '"), "');"))
    predict.check <- DBI::dbGetQuery(con, paste0("SELECT location, timeseries_id, location, parameter, period_type, operator, record_rate FROM timeseries WHERE location IN ('", paste(nrby$location, collapse = "', '"), "') AND parameter = '", entry$parameter, "' AND timeseries_id != ", tsid, " AND start_datetime <= '", start_predict, "' AND record_rate IN ('", paste(record_rates, collapse = "', '"), "');"))
  }
  similar.check <- merge(train.check, predict.check)
  similar.check <- merge(similar.check, nrby, by = "location")
  
  # Now check if there is enough data within the train_range for each of the timeseries in similar. Calculate the percentage of data that exists within the train_range for each timeseries
  trainData <- list()
  predictData <- list()
  similar <- data.frame()
  similar.check$training_prct_complete <- NA
  similar.check$predict_prct_complete <- NA
  message("Aligning data points... this can take a while")
  
  # num_cores <- parallel::detectCores() - 1
  # doParallel::registerDoParallel(num_cores)
  
  for (i in 1:nrow(similar.check)) {  #for each timeseries, check if there is data within the train_range. if daily = TRUE look to calculated_daily table, otherwise to measurements_continuous. If there is data store it in the list object data
    if (daily) {
      train_dat <- DBI::dbGetQuery(con, paste0("SELECT date, value FROM calculated_daily WHERE timeseries_id = ", similar.check$timeseries_id[i], " AND date >= '", as.Date(min(outcome_values$datetime)), "' AND date <= '", as.Date(max(outcome_values$datetime)), "' AND value IS NOT NULL;"))
      train_dat$datetime <- as.POSIXct(train_dat$date, tz = "UTC")
      train_dat$date <- NULL
    } else {
      train_dat <- DBI::dbGetQuery(con, paste0("SELECT datetime, value FROM measurements_continuous WHERE timeseries_id = ", similar.check$timeseries_id[i], " AND datetime >= '", min(outcome_values$datetime), "' AND datetime <= '", max(outcome_values$datetime), "' AND value IS NOT NULL;"))
    }
    if (nrow(train_dat) > 50) {
      # add in data that might exist within the prediction window as well
      if (daily) {
        predict_dat <- DBI::dbGetQuery(con, paste0("SELECT date, value FROM calculated_daily WHERE timeseries_id = ", similar.check$timeseries_id[i], " AND date >= '", as.Date(start_predict), "' AND date <= '", as.Date(end_predict), "' AND value IS NOT NULL;"))
        predict_dat$datetime <- as.POSIXct(predict_dat$date, tz = "UTC")
        predict_dat$date <- NULL
        if (!no_extra) {
          train_dat <- unique(rbind(train_dat, predict_dat))
        }
      } else {
        predict_dat <- DBI::dbGetQuery(con, paste0("SELECT datetime, value FROM measurements_continuous WHERE timeseries_id = ", similar.check$timeseries_id[i], " AND datetime >= '", start_predict, "' AND datetime <= '", end_predict, "' AND value IS NOT NULL;"))
        if (!no_extra) {
          train_dat <- unique(rbind(train_dat, predict_dat))
        }
      }
      
      #Make into mean/max/min if necessary and match the period of the target ts. Work on both the training and prediction data, if they are different.
      calculated.train <- data.frame(datetime = seq.POSIXt(from = min(outcome_values$datetime), to = max(outcome_values$datetime), by = period), 
                                     value = NA)
      if (!no_extra) {
        calculated.predict <- data.frame(datetime = seq.POSIXt(from = min(exist.values$datetime), to = max(exist.values$datetime), by = period), 
                                         value = NA)
      }
      operation <- switch(entry$period_type,
                          "instantaneous" = mean,
                          "sum" = sum,
                          "min" = min,
                          "max" = max,
                          "mean" = mean,
                          "median" = stats::median,
                          "(min+max)/2" = function(x) mean(c(min(x), max(x)))
      )
      
      train.values <- sapply(calculated.train$datetime, function(dt) {
        subset <- train_dat$datetime > (dt - period) & train_dat$datetime <= dt
        operation(train_dat$value[subset], na.rm = TRUE)
      })
      calculated.train$value <- hablar::rationalize(train.values)
      calculated.train <- calculated.train[!is.na(calculated.train$value),]
      if (no_extra) {
        calculated.predict <- calculated.train[calculated.train$datetime >= min(exist.values$datetime) & calculated.train$datetime <= max(exist.values$datetime),]
      } else {
        predict.values <- sapply(calculated.predict$datetime, function(dt) {
          subset <- predict_dat$datetime > (dt - period) & predict_dat$datetime <= dt
          operation(predict_dat$value[subset], na.rm = TRUE)
        })
        calculated.predict$value <- hablar::rationalize(predict.values)
        calculated.predict <- calculated.predict[!is.na(calculated.predict$value),]
      }
      
      # calculate the percentage of the data that is complete compared to the outcome_values and exist.values that need to be predicted
      similar.check$training_prct_complete[i] <- round(nrow(calculated.train)/nrow(outcome_values), 2)
      tmp <- exist.values[is.na(exist.values$value) ,]
      similar.check$predict_prct_complete[i] <- round(nrow(calculated.predict[calculated.predict$datetime %in% tmp$datetime , ])/nrow(tmp), 2)
      if (nrow(similar) == 0) {
        similar <- similar.check[i,]
      } else {
        similar <- rbind(similar, similar.check[i,])
      }
      trainData[[as.character(similar.check$timeseries_id[i])]] <- calculated.train
      predictData[[as.character(similar.check$timeseries_id[i])]] <- calculated.predict
    }
  }
  
  if (nrow(similar) == 0) { #Stop here and alert the user to what else they could try
    message("No suitable timeseries were found. You could try broadening the radius, specifying extra parameters, or modifying the parameter train_range.")
    return(returns)
  }
  
  similar <- similar[order(similar$distance_meters, similar$predict_prct_complete, similar$training_prct_complete), ]
  similar$name <- substr(similar$name, 1, 30)
  
  # Ask the user to select groups of timeseries for training ########################################
  message("Working with location ", entry$location, ", parameter ", entry$parameter, ", period_type ", entry$period_type, ", and record rate of ", entry$record_rate, ". Look right to see what it looks like before prediction.")
  grey_indices <- which(is.na(exist.values$value))
  bot <- min(exist.values$value, na.rm = TRUE)
  top <- max(exist.values$value, na.rm = TRUE)
  plot(exist.values$datetime, exist.values$value, type = "l", col = "blue", xlab = "datetime", ylab = "value")
  for (i in grey_indices) {
    graphics::rect(exist.values[i - 1, "datetime"], bot, exist.values[i + 1, "datetime"], top, col = 'grey', border = NA)
  }
  
  # Make a function to ask the user to select groups of timeseries for imputation, of length between 1 and nrow(similar)
  select_for_impute <- function() {
    message("Timeseries that can be used for imputation:")
    print(similar[, -which(names(similar) %in% c("location", "operator", "record_rate"))], row.names = FALSE)
    choice <- readline(prompt =
                         writeLines(paste("\nChoose one or more timeseries_ids on which to build models. Specify these as a list, for example: list(c(10,38,77), c(92,101)). Each list element will be used to build a separate model, and you'll be asked to select the best performing one.",
                                          "\nHit Escape to exit this function."
                         )))
    choice <- try(eval(parse(text = choice)), silent = TRUE)
    if (!inherits(choice, "list")) { #chastise the user if they don't give a list
      choice <- readline(prompt =
                           writeLines(paste("\nThat's not a list. Try again."
                           )))
    } else { #check that each number in each list element corresponds to a timeseries_id in similar
      for (i in 1:length(choice)) {
        if (!all(choice[[i]] %in% similar$timeseries_id)) {
          choice[[i]] <- readline(prompt =
                                   writeLines(paste("\nAt least one of the timeseries_ids that you specified is not a valid choice. Try again."
                                   )))
        }
      }
    }
    return(choice)
  }
  
  selected <- select_for_impute()
  
  #  Get the data for the selected timeseries ########################################################
  message("Training and testing models... this can take a while.")
  models <- list(models = list(), predicted = list())
  combined_predictDataList <- list()
  model.perf <- data.frame(Model_no = NA,
                           tsids = NA,
                           MICE = NA,
                           mean_abs_error = NA,
                           mean_sqared_error = NA,
                           root_mean_square_error = NA,
                           r_squared = NA)
  names(outcome_values)[names(outcome_values) == "value"] <- "outcome"
  for (i in 1:length(selected)) {
    selected_group <- selected[[i]]
    if (mice) {
      #non-mice model
      model.perf[(i * 2) - 1, "tsids"] <- paste(selected_group, collapse = ", ")
      model.perf[(i * 2) - 1, "MICE"] <- FALSE
      #mice model
      model.perf[i * 2, "tsids"] <- paste(selected_group, collapse = ", ")
      model.perf[i * 2, "MICE"] <- TRUE
    } else {
      model.perf[i, "tsids"] <- paste(selected_group, collapse = ", ")
      model.perf[i, "MICE"] <- FALSE
    }
    
    for (j in 1:length(selected_group)) {
      selected_data <- trainData[[as.character(selected_group[j])]]
      selected_predictData <- predictData[[as.character(selected_group[j])]]
      if (j == 1) {
        names(selected_data) <- c("datetime", "value1")
        names(selected_predictData) <- c("datetime", "value1")
        combined_data <- selected_data
        combined_predictData <- selected_predictData
      } else {
        names(selected_data) <- c("datetime", paste0("value", j))
        names(selected_predictData) <- c("datetime", paste0("value", j))
        combined_data <- merge(combined_data, selected_data, by = "datetime", all = TRUE)
        combined_predictData <- merge(combined_predictData, selected_predictData, by = "datetime", all = TRUE)
      }
    }
    combined_data <- merge(combined_data, outcome_values, by = "datetime", all.x = TRUE)
    
    # Add in rows for missing datetimes in combined_predictData, using the datetimes in exist.values
    full <- exist.values
    full$value <- NA
    combined_predictData <- merge(combined_predictData, exist.values, by = "datetime", all.y = TRUE)
    combined_predictData$value <- NULL
    # Now use cubic spline interpolation to fill in missing values on each column of combined_data and combined_predictData where the datetime gap is <= 3 days (if daily = TRUE) or <= 1 day if daily is FALSE
    gap <- 5 * 86400/period  # 3 days, regardless of the period
    for (j in 2:ncol(combined_data)) { #skip the datetime column
      combined_data[, j] <- zoo::na.spline(combined_data[, j], na.rm = TRUE, maxgap = gap)
    }
    for (j in 2:ncol(combined_predictData)) { #skip the datetime column
      combined_predictData[, j] <- zoo::na.spline(combined_predictData[, j], na.rm = TRUE, maxgap = gap)
    }
    
    if (mice) {
      # MICE model
      mice.data <- mice::mice(combined_data, m = 5, maxit = 10, method = "cart", printFlag = FALSE)
      mice.data <-  mice::complete(mice.data)
      
      mice.data.predict <- mice::mice(combined_predictData, m = 5, maxit = 10, method = "cart", printFlag = FALSE)
      mice.data.predict <-  mice::complete(mice.data.predict)
      combined_predictDataList[[i * 2]] <- mice.data.predict
      
      message("Training model for selection ", i, " of ", length(selected), " on MICE-imputed data")
      model <- randomForest::randomForest(outcome ~ ., data = mice.data[, -which(names(mice.data) == "datetime")], ntree = 100)
      # model <- ranger::ranger(outcome ~ ., data = complete.cases(mice.data), num.trees = 100, mtry = 2)
      models$models[[i * 2]] <- model
      message("Testing the model...")
      mice.data$predicted <- stats::predict(model, newdata = mice.data)
      models$predicted[[i * 2]] <- mice.data[ , which(names(mice.data) %in% c("datetime", "predicted"))]
      # Calculate evaluation metrics for regression
      mean_abs_error <- mean(abs(mice.data$outcome - mice.data$predicted))
      mean_sqared_error <- mean((mice.data$outcome - mice.data$predicted)^2)
      root_mean_square_error <- sqrt(mean_sqared_error)
      r_squared <- cor(mice.data$outcome, mice.data$predicted)^2
      # Store evaluation metrics in the model.perf dataframe
      model.perf[i * 2, c("mean_abs_error", "mean_sqared_error", "root_mean_square_error", "r_squared")] <- c(mean_abs_error, mean_sqared_error, root_mean_square_error, r_squared)
      model.perf[i * 2, "Model_no"] <- paste0(i, " MICE")
      
      #non-MICE model (prediction data still gets MICE'd)
      combined_data <- combined_data[complete.cases(combined_data),]
      combined_predictDataList[[i * 2 - 1]] <- mice.data.predict
      
      # Train and test the model
      message("Training model for selection ", i, " of ", length(selected) , " on non-imputed data")
      model <- randomForest::randomForest(outcome ~ ., data = combined_data[, -which(names(combined_data) == "datetime")], ntree = 100)
      # model <- ranger::ranger(outcome ~ ., data = combined_data[complete.cases(combined_data), ], num.trees = 100, mtry = 2)
      models$models[[i * 2 - 1]] <- model
      message("Testing the model...")
      combined_data$predicted <- stats::predict(model, newdata = combined_data)
      models$predicted[[i * 2 - 1]] <- combined_data[, which(names(combined_data) %in% c("datetime", "predicted"))]
      # Calculate evaluation metrics for regression
      mean_abs_error <- mean(abs(combined_data$outcome - combined_data$predicted))
      mean_sqared_error <- mean((combined_data$outcome - combined_data$predicted)^2)
      root_mean_square_error <- sqrt(mean_sqared_error)
      r_squared <- cor(combined_data$outcome, combined_data$predicted)^2
      # Store evaluation metrics in the model.perf dataframe
      model.perf[(i * 2) - 1, c("mean_abs_error", "mean_sqared_error", "root_mean_square_error", "r_squared")] <- c(mean_abs_error, mean_sqared_error, root_mean_square_error, r_squared)
      model.perf[i * 2 - 1, "Model_no"] <- i
    } else { # MICE is FALSE
      combined_data <- combined_data[complete.cases(combined_data),]
      
      # Still fill in missing values in the prediction data so as to get a complete data set
      mice.data.predict <- mice::mice(combined_predictData, m = 5, maxit = 10, method = "cart")
      mice.data.predict <-  mice::complete(mice.data.predict)
      combined_predictDataList[[i]] <- mice.data.predict

      # Train and test the model
      message("Training model for selection ", i, " of ", length(selected))
      model <- randomForest::randomForest(outcome ~ ., data = combined_data[, -which(names(combined_data) == "datetime")], ntree = 100)
      # model <- ranger::ranger(outcome ~ ., data = combined_data[complete.cases(combined_data), ], num.trees = 100, mtry = 2)
      models$models[[i]] <- model
      message("Testing the model...")
      combined_data$predicted <- stats::predict(model, newdata = combined_data)
      models$predicted[[i]] <- combined_data[, which(names(combined_data) %in% c("datetime", "predicted"))]
      # Calculate evaluation metrics for regression
      mean_abs_error <- mean(abs(combined_data$outcome - combined_data$predicted))
      mean_sqared_error <- mean((combined_data$outcome - combined_data$predicted)^2)
      root_mean_square_error <- sqrt(mean_sqared_error)
      r_squared <- cor(combined_data$outcome, combined_data$predicted)^2
      # Store evaluation metrics in the model.perf dataframe
      model.perf[i, c("mean_abs_error", "mean_sqared_error", "root_mean_square_error", "r_squared")] <- c(mean_abs_error, mean_sqared_error, root_mean_square_error, r_squared)
      model.perf[i, "Model_no"] <- i
    }
  }
  message("Training and testing models complete.")
  
  
  # Make predictions, align with data at start and/or end of missing time period, and ask user to select best model
  missing_indices <- which(is.na(exist.values$value))
  missing_datetimes <- exist.values$datetime[missing_indices]
  prediction <- list()
  for (i in 1:length(combined_predictDataList)) {
    model <- models$models[[i]]
    data <- combined_predictDataList[[i]]
    data$predicted <- stats::predict(model, newdata = data)
    # data[!(data$datetime %in% missing_datetimes), "predicted"] <- NA
    prediction[[i]] <- data
  }
  # Now show the user the data.frame model.perf and plot the various model traces. Ask them to choose one by row number.
  bot <- min(exist.values$value, na.rm = TRUE)
  top <- max(exist.values$value, na.rm = TRUE)
  plot(exist.values$datetime, exist.values$value, type = "l", lwd = 2, col = "black", xlab = "datetime", ylab = "value")
  for (i in missing_indices) {
    graphics::rect(exist.values[i - 1, "datetime"], bot, exist.values[i + 1, "datetime"], top, col = 'grey', border = NA)
  }
  # Make a vector of colors of length equal to the number of models
  colors <- c("red", "blue", "green", "purple", "orange", "yellow", "pink", "brown", "grey", "cyan")
  for (i in 1:length(prediction)) {
    lines(prediction[[i]]$datetime, prediction[[i]]$predicted, col = colors[i], lwd = 1)
  }
  if (mice) {
    legend.text <- c("Observed", "Model 1", "Model 1 (MICE)", "Model 2", "Model 2 (MICE)", "Model 3", "Model 3 (MICE)", "Model 4", "Model 4 (MICE)", "Model 5", "Model 5 (MICE)")
  } else {
    legend.text <- c("Observed", "Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6", "Model 7", "Model 8", "Model 9", "Model 10")
  }
  
  legend("topright", legend = legend.text[c(1, 1:length(prediction) + 1)], col = c("black", colors[1:length(prediction)]), lwd = c(2, 1), cex = 0.8)
  
  print(model.perf)
  
  
  return(returns)
}
