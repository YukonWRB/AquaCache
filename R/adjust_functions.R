# Functions to adjust the grade, qualifier, approval, owner, and contributor of continuous-type data as it's appended to the database.

#' Adjust the grade of a timeseries in the database
#'
#' @param con A connection to the database with write privileges to the 'approvals' and 'measurements_continuous' tables.
#' @param timeseries_id The target timeseries_id
#' @param data A data.frame with columns for 'datetime' and 'approval'. 'datetime' should be POSIXct and 'approval' should either character (in which case it must refer to entries in column 'approval_type_code' of table 'approval_types' or integer/numeric, in which case it must refer to column 'approval_type_id' of the same table.
#'
#' @return Modifies the 'approvals' table in the database.
#' @export
#'

adjust_grade <- function(con, timeseries_id, data) {
  
  # If a column 'date' and no column 'datetime' is present, rename 'date' to 'datetime' and convert to POSIXct
  if ("date" %in% names(data) & !"datetime" %in% names(data)) {
    data$datetime <- as.POSIXct(data$date, tz = "UTC")
    data <- data[, !names(data) == "date"]
  }
  
  # Make sure that column 'grade' is not all NA
  if (all(is.na(data$grade))) {
    return(message("adjust_grade: column 'grade' was all NA, skipped. Applies to timeseries_id ", timeseries_id, "."))
  }
  
  # Check if 'grade' is character, if so match those characters to 'grade_type_code' in the 'grades' table
  if (inherits(data$grade[1], "character")) {
    grade_table <- DBI::dbGetQuery(con, "SELECT grade_type_id, grade_type_code FROM grade_types;")
    data$grade <- grade_table$grade_type_id[match(data$grade, grade_table$grade_type_code)]
  }
  
  
  # Ensure that 'datetime' is POSIXct
  if (!inherits(data$datetime[1], "POSIXct")) {
    stop("Column 'datetime' must be of class POSIXct.")
  }
  
  unknown_grade <- DBI::dbGetQuery(con, "SELECT grade_type_id FROM grade_types WHERE grade_type_code = 'UNK'")[1,1]
  
  data$grade[is.na(data$grade)] <- unknown_grade
  
  # Get the data where at least one of the following is true:
  # has an end datetime within the range of the data
  # has a start datetime within the range of the data
  # has a start datetime before the range of the data and an end datetime after the range of the data
  # This leaves out entries that are entirely before or after the range of the data.
  exist <- DBI::dbGetQuery(con, paste0(
    "SELECT grade_id, timeseries_id, grade_type_id, start_dt, end_dt 
    FROM grades 
    WHERE timeseries_id = ", timeseries_id, " 
     AND (
            (
              end_dt >= '", min(data$datetime), "'
              AND end_dt <= '", max(data$datetime), "'
            )
         OR (
              start_dt >= '", min(data$datetime), "'
              AND start_dt <= '", max(data$datetime), "'
             )
         OR (
              start_dt <= '", min(data$datetime), "'
              AND end_dt >= '", max(data$datetime), "'
             )
          )
    ORDER BY start_dt ASC;
    "))
  
  
  original_exist_rows <- nrow(exist)
  
  if (original_exist_rows == 0) {
    exist <- data.frame(grade_id = NA,
                        timeseries_id = timeseries_id,
                        grade_type_id = data$grade[1],
                        start_dt = data$datetime[1],
                        end_dt = data$datetime[1])
  }
  
  # Collapse consecutive rows with the same grade using run-length encoding
  data <- data[order(data$datetime), ]
  runs <- rle(data$grade)
  ends <- cumsum(runs$lengths)
  starts <- c(1, head(ends, -1) + 1)
  new_segments <- data.frame(
    grade_id      = NA,
    timeseries_id     = timeseries_id,  # assume defined in your environment
    grade_type_id = runs$values,
    start_dt          = data$datetime[starts],
    end_dt            = data$datetime[ends],
    stringsAsFactors  = FALSE
  )
  
  current <- if (original_exist_rows > 0) exist$grade_type_id[1] else new_segments$grade_type_id[1]
  
  index <- 1 # keeps track of the row we should be modifying in 'exist'
  
  # Now loop through the data to find where the grade_type_id changes
  for (i in 1:nrow(new_segments)) {
    if (new_segments$grade_type_id[i] != current) {
      if (index <= original_exist_rows) { # Modify rows in 'exist'
        if (index == 1) { # If still on first row, check if its start_dt needs to be modified
          if (exist$start_dt[index] > new_segments$start_dt[i]) { # If the start_dt of the first row in 'exist' is later than the start_dt of the first row in 'new_segments', adjust it
            exist$start_dt[index] <- new_segments$start_dt[i]
          } # it's possible that the user has provided data that starts *after* the full record, so don't adjust the start_dt of the first row in that case
        } else {
          exist$start_dt[index] <- new_segments$start_dt[i]
        }
        exist$grade_type_id[index] <- new_segments$grade_type_id[i]
        if (index != nrow(exist)) { # Adjust the end_dt of this grade_type_id if it's not the last row of 'exist' (otherwise we risk truncating a time period if the provided data does not go to the end of the timeseries)
          exist$end_dt[index] <- new_segments$end_dt[i] 
        }
        if (i < nrow(new_segments)) {
          index <- index + 1
          current <- new_segments$grade_type_id[i]
        }
      } else { # Create new rows with no grade_id
        # Modify the last row in 'exist' and add a new row
        if (index > 1) {
          exist$end_dt[nrow(exist)] <- new_segments$end_dt[i - 1]
        }
        to_append <- data.frame(grade_id = NA,
                                timeseries_id = timeseries_id,
                                grade_type_id = new_segments$grade_type_id[i],
                                start_dt = new_segments$start_dt[i],
                                end_dt = new_segments$end_dt[i])
        exist <- rbind(exist, to_append)
        index <- nrow(exist)
      }
      current <- new_segments$grade_type_id[i]
    } else { # If the grade_type_id is the same as the last one, check and adjust datetimes
      if (index <= original_exist_rows) { # Modify rows in 'exist'
        if (index == 1) { # If still on first row, check if its start_dt needs to be modified
          if (exist$start_dt[index] > new_segments$start_dt[i]) { # If the start_dt of the first row in 'exist' is later than the start_dt of the first row in 'new_segments', adjust it
            exist$start_dt[index] <- new_segments$start_dt[i]
          } # it's possible that the user has provided data that starts *after* the full record, so don't adjust the start_dt of the first row in that case
        } else {
          exist$start_dt[index] <- new_segments$start_dt[i]
        }
        if (index != nrow(exist)) { # Adjust the end_dt of this grade_type_id if it's not the last row of 'exist' (otherwise we risk truncating a time period if the provided data does not go to the end of the timeseries)
          exist$end_dt[index] <- new_segments$end_dt[i] 
        }
        if (i < nrow(new_segments)) {
          index <- index + 1
        }
      } else { # Create new rows with no grade_id
        # Modify the last row in 'exist' and add a new row
        if (index > 1) {
          exist$end_dt[nrow(exist)] <- new_segments$end_dt[i - 1]
        }
        to_append <- data.frame(grade_id = NA,
                                timeseries_id = timeseries_id,
                                grade_type_id = new_segments$grade_type_id[i],
                                start_dt = new_segments$start_dt[i],
                                end_dt = new_segments$end_dt[i])
        exist <- rbind(exist, to_append)
        index <- nrow(exist)
      }
    }
    
    if (i == nrow(new_segments)) {
      exist$end_dt[index] <- new_segments$end_dt[i] # Adjust the end_dt of this grade
    }
  }  # End of for loop
  
  # if there are untouched rows in exist check if they interfere with the last end_dt. If they do, they either need adjustments to their start_dt or need to be remove form the database
  if (index < nrow(exist)) {
    # Pull out the rows that are left over
    leftovers <- exist[c((index + 1):nrow(exist)), ]
    exist <- exist[c(1:index), ]
    # Find entries that now have a start_dt and end_dt entirely captured by other rows. Label them with timeseries_id = -1 to remove later
    leftovers[leftovers$start_dt <= exist$end_dt[index] & leftovers$end_dt <= exist$end_dt[index], "timeseries_id"] <- -1
    # Find entries that have a start_dt before the end_dt of the last row, but end_dt after the end_dt of the last row. Adjust their start_dt to be the same as the end_dt of the last row. These won't be removed. This *should* only ever be a single row because of DB constraints.
    leftovers[leftovers$start_dt < exist$end_dt[index] & leftovers$end_dt > exist$end_dt[index], "start_dt"] <- exist$end_dt[index]
    
    exist <- rbind(exist, leftovers)
  }
  
  # Now commit the changes to the database
  commit_fx <- function(con, exist) {
    remove <- exist[exist$timeseries_id == -1, "grade_id"]
    exist <- exist[exist$timeseries_id != -1, ]
    if (length(remove) > 0) {
      DBI::dbExecute(con, paste0("DELETE FROM grades WHERE grade_id IN (", paste(remove, collapse = ", "), ");"))
    }
    for (i in 1:nrow(exist)) {
      if (!is.na(exist$grade_id[i])) {  # Means that we need to update rows
        DBI::dbExecute(con, paste0("UPDATE grades SET grade_type_id = ", exist$grade_type_id[i], ", start_dt = '", exist$start_dt[i], "', end_dt = '", exist$end_dt[i], "' WHERE grade_id = ", exist$grade_id[i], ";"))
      } else { # Means that we need to insert new rows
        DBI::dbAppendTable(con, "grades", exist[i, -which(names(exist) == "grade_id")])
      }
    }
  }
  
  if (!attr(con, "active_transaction")) {
    DBI::dbBegin(con)
    attr(con, "active_transaction") <- TRUE
    tryCatch({
      commit_fx(con, exist)
      DBI::dbCommit(con)
      attr(con, "active_transaction") <- FALSE
    }, error = function(e) {
      DBI::dbRollback(con)
      attr(con, "active_transaction") <<- FALSE
      warning("adjust_grade: Failed to commit changes to the database with error ", e$message)
    })
  } else { # we're already in a transaction
    commit_fx(con, exist)
  }
  
} # End of adjust_grade function


#' Adjust the qualifier of a timeseries in the database
#'
#' @param con A connection to the database with write privileges to the 'qualifiers' and 'measurements_continuous' tables.
#' @param timeseries_id The target timeseries_id
#' @param data A data.frame with columns for 'datetime' and 'qualifier'. 'datetime' should be POSIXct and 'qualifier' should either character (in which case it must refer to entries in column 'qualifier_type_code' of table 'qualifiers' or integer/numeric, in which case it must refer to column 'qualifier_type_id' of the same table.
#'
#' @return Modifies the 'qualifiers' table in the database.
#' @export

adjust_qualifier <- function(con, timeseries_id, data) {
  
  # If a column 'date' and no column 'datetime' is present, rename 'date' to 'datetime' and convert to POSIXct
  if ("date" %in% names(data) & !"datetime" %in% names(data)) {
    data$datetime <- as.POSIXct(data$date, tz = "UTC")
    data <- data[, !names(data) == "date"]
  }
  
  # Make sure that column 'qualifier' is not all NA
  if (all(is.na(data$qualifier))) {
    return(message("adjust_qualifier: column 'qualifier' was all NA, skipped. Applies to timeseries_id ", timeseries_id, "."))
  }
  
  # Check if 'qualifier' is character, if so match those characters to 'qualifier_type_code' in the 'qualifiers' table
  if (inherits(data$qualifier[1], "character")) {
    qualifier_table <- DBI::dbGetQuery(con, "SELECT qualifier_type_id, qualifier_type_code FROM qualifier_types;")
    data$qualifier <- qualifier_table$qualifier_type_id[match(data$qualifier, qualifier_table$qualifier_type_code)]
  }
  
  # Ensure that 'datetime' is POSIXct
  if (!inherits(data$datetime[1], "POSIXct")) {
    stop("Column 'datetime' must be of class POSIXct.")
  }
  
  unknown_qualifier <- DBI::dbGetQuery(con, "SELECT qualifier_type_id FROM qualifier_types WHERE qualifier_type_code = 'UNK'")[1,1]
  
  data$qualifier[is.na(data$qualifier)] <- unknown_qualifier
  
  # Get the data where at least one of the following is true:
  # has an end datetime within the range of the data
  # has a start datetime within the range of the data
  # has a start datetime before the range of the data and an end datetime after the range of the data
  # This leaves out entries that are entirely before or after the range of the data.
  exist <- DBI::dbGetQuery(con, paste0(
    "SELECT qualifier_id, timeseries_id, qualifier_type_id, start_dt, end_dt 
    FROM qualifiers 
    WHERE timeseries_id = ", timeseries_id, " 
      AND (
            (
              end_dt >= '", min(data$datetime), "'
              AND end_dt <= '", max(data$datetime), "'
            )
         OR (
              start_dt >= '", min(data$datetime), "'
              AND start_dt <= '", max(data$datetime), "'
             )
         OR (
             start_dt <= '", min(data$datetime), "'
             AND end_dt >= '", max(data$datetime), "'
            )
          )
          ORDER BY start_dt ASC;
    "))
  
  original_exist_rows <- nrow(exist)
  
  if (original_exist_rows == 0) {
    exist <- data.frame(qualifier_id = NA,
                        timeseries_id = timeseries_id,
                        qualifier_type_id = data$qualifier[1],
                        start_dt = data$datetime[1],
                        end_dt = data$datetime[1])
  }
  
  # Collapse consecutive rows with the same qualifier using run-length encoding
  data <- data[order(data$datetime), ]
  runs <- rle(data$qualifier)
  ends <- cumsum(runs$lengths)
  starts <- c(1, head(ends, -1) + 1)
  new_segments <- data.frame(
    qualifier_id      = NA,
    timeseries_id     = timeseries_id,  # assume defined in your environment
    qualifier_type_id = runs$values,
    start_dt          = data$datetime[starts],
    end_dt            = data$datetime[ends],
    stringsAsFactors  = FALSE
  )
  
  current <- if (original_exist_rows > 0) exist$qualifier_type_id[1] else new_segments$qualifier_type_id[1]
  
  index <- 1 # keeps track of the row we should be modifying in 'exist'
  
  # Now loop through the data to find where the qualifier_type_id changes
  for (i in 1:nrow(new_segments)) {
    if (new_segments$qualifier_type_id[i] != current) {
      if (index <= original_exist_rows) { # Modify rows in 'exist'
        if (index == 1) { # If still on first row, check if its start_dt needs to be modified
          if (exist$start_dt[index] > new_segments$start_dt[i]) { # If the start_dt of the first row in 'exist' is later than the start_dt of the first row in 'new_segments', adjust it
            exist$start_dt[index] <- new_segments$start_dt[i]
          } # it's possible that the user has provided data that starts *after* the full record, so don't adjust the start_dt of the first row in that case
        } else {
          exist$start_dt[index] <- new_segments$start_dt[i]
        }
        exist$qualifier_type_id[index] <- new_segments$qualifier_type_id[i]
        if (index != nrow(exist)) { # Adjust the end_dt of this qualifier_type_id if it's not the last row of 'exist' (otherwise we risk truncating a time period if the provided data does not go to the end of the timeseries)
          exist$end_dt[index] <- new_segments$end_dt[i] 
        }
        if (i < nrow(new_segments)) {
          index <- index + 1
          current <- new_segments$qualifier_type_id[i]
        }
      } else { # Create new rows with no qualifier_id
        # Modify the last row in 'exist' and add a new row
        if (index > 1) {
          exist$end_dt[nrow(exist)] <- new_segments$end_dt[i - 1]
        }
        to_append <- data.frame(qualifier_id = NA,
                                timeseries_id = timeseries_id,
                                qualifier_type_id = new_segments$qualifier_type_id[i],
                                start_dt = new_segments$start_dt[i],
                                end_dt = new_segments$end_dt[i])
        exist <- rbind(exist, to_append)
        index <- nrow(exist)
      }
      current <- new_segments$qualifier_type_id[i]
    } else { # If the qualifier_type_id is the same as the last one, check and adjust datetimes
      if (index <= original_exist_rows) { # Modify rows in 'exist'
        if (index == 1) { # If still on first row, check if its start_dt needs to be modified
          if (exist$start_dt[index] > new_segments$start_dt[i]) { # If the start_dt of the first row in 'exist' is later than the start_dt of the first row in 'new_segments', adjust it
            exist$start_dt[index] <- new_segments$start_dt[i]
          } # it's possible that the user has provided data that starts *after* the full record, so don't adjust the start_dt of the first row in that case
        } else {
          exist$start_dt[index] <- new_segments$start_dt[i]
        }
        if (index != nrow(exist)) { # Adjust the end_dt of this qualifier_type_id if it's not the last row of 'exist' (otherwise we risk truncating a time period if the provided data does not go to the end of the timeseries)
          exist$end_dt[index] <- new_segments$end_dt[i] 
        }
        if (i < nrow(new_segments)) {
          index <- index + 1
        }
      } else { # Create new rows with no qualifier_id
        # Modify the last row in 'exist' and add a new row
        if (index > 1) {
          exist$end_dt[nrow(exist)] <- new_segments$end_dt[i - 1]
        }
        to_append <- data.frame(qualifier_id = NA,
                                timeseries_id = timeseries_id,
                                qualifier_type_id = new_segments$qualifier_type_id[i],
                                start_dt = new_segments$start_dt[i],
                                end_dt = new_segments$end_dt[i])
        exist <- rbind(exist, to_append)
        index <- nrow(exist)
      }
    }
    
    if (i == nrow(new_segments)) {
      exist$end_dt[index] <- new_segments$end_dt[i] # Adjust the end_dt of this qualifier
    }
  }  # End of for loop
  
  # if there are untouched rows in exist check if they interfere with the last end_dt. If they do, they either need adjustments to their start_dt or need to be remove form the database
  if (index < nrow(exist)) {
    # Pull out the rows that are left over
    leftovers <- exist[c((index + 1):nrow(exist)), ]
    exist <- exist[c(1:index), ]
    # Find entries that now have a start_dt and end_dt entirely captured by other rows. Label them with timeseries_id = -1 to remove later
    leftovers[leftovers$start_dt <= exist$end_dt[index] & leftovers$end_dt <= exist$end_dt[index], "timeseries_id"] <- -1
    # Find entries that have a start_dt before the end_dt of the last row, but end_dt after the end_dt of the last row. Adjust their start_dt to be the same as the end_dt of the last row. These won't be removed. This *should* only ever be a single row because of DB constraints.
    leftovers[leftovers$start_dt < exist$end_dt[index] & leftovers$end_dt > exist$end_dt[index], "start_dt"] <- exist$end_dt[index]
    
    exist <- rbind(exist, leftovers)
  }
  
  
  
  # Now commit the changes to the database
  commit_fx <- function(con, exist) {
    remove <- exist[exist$timeseries_id == -1, "qualifier_id"]
    exist <- exist[exist$timeseries_id != -1, ]
    if (length(remove) > 0) {
      DBI::dbExecute(con, paste0("DELETE FROM qualifiers WHERE qualifier_id IN (", paste(remove, collapse = ", "), ");"))
    }
    for (i in 2:nrow(exist)) {
      if (!is.na(exist$qualifier_id[i])) {  # Means that we need to update rows
        DBI::dbExecute(con, paste0("UPDATE qualifiers SET qualifier_type_id = ", exist$qualifier_type_id[i], ", start_dt = '", exist$start_dt[i], "', end_dt = '", exist$end_dt[i], "' WHERE qualifier_id = ", exist$qualifier_id[i], ";"))
      } else { # Means that we need to insert new rows
        DBI::dbAppendTable(con, "qualifiers", exist[i, -which(names(exist) == "qualifier_id")])
      }
    }
  }
  
  if (!attr(con, "active_transaction")) {
    DBI::dbBegin(con)
    attr(con, "active_transaction") <- TRUE
    tryCatch({
      commit_fx(con, exist)
      DBI::dbCommit(con)
      attr(con, "active_transaction") <- FALSE
    }, error = function(e) {
      DBI::dbRollback(con)
      attr(con, "active_transaction") <<- FALSE
      warning("adjust_qualifier: Failed to commit changes to the database with error ", e$message)
    })
  } else { # we're already in a transaction
    commit_fx(con, exist)
  }
} # End of adjust_qualifier function




#' Adjust the approval of a timeseries in the database
#' 
#' @param con A connection to the database with write privileges to the 'approvals' and 'measurements_continuous' tables.
#' @param timeseries_id The target timeseries_id
#' @param data A data.frame with columns for 'datetime' and 'approval'. 'datetime' should be POSIXct and 'approval' should either character (in which case it must refer to entries in column 'approval_type_code' of table 'approval_types' or integer/numeric, in which case it must refer to column 'approval_type_id' of the same table.
#'  
#' @return Modifies the 'approvals' table in the database.
#' @export

adjust_approval <- function(con, timeseries_id, data) {
  
  # If a column 'date' and no column 'datetime' is present, rename 'date' to 'datetime' and convert to POSIXct
  if ("date" %in% names(data) & !"datetime" %in% names(data)) {
    data$datetime <- as.POSIXct(data$date, tz = "UTC")
    data <- data[, !names(data) == "date"]
  }
  
  # Make sure that column 'approval' is not all NA
  if (all(is.na(data$approval))) {
    return(message("adjust_owner: column 'approval' was all NA, skipped. Applies to timeseries_id ", timeseries_id, "."))
  }
  
  # Check if 'approval' is character, if so match those characters to 'approval_type_code' in the 'approvals' table
  if (inherits(data$approval[1], "character")) {
    approval_table <- DBI::dbGetQuery(con, "SELECT approval_type_id, approval_type_code FROM approval_types;")
    data$approval <- approval_table$approval_type_id[match(data$approval, approval_table$approval_type_code)]
  }
  
  # Ensure that 'datetime' is POSIXct
  if (!inherits(data$datetime[1], "POSIXct")) {
    stop("Column 'datetime' must be of class POSIXct.")
  }
  
  unknown_approval <- DBI::dbGetQuery(con, "SELECT approval_type_id FROM approval_types WHERE approval_type_code = 'UNK'")[1,1]
  
  data$approval[is.na(data$approval)]  <- unknown_approval
  
  # Get the data where at least one of the following is true:
  # has an end datetime within the range of the data
  # has a start datetime within the range of the data
  # has a start datetime before the range of the data and an end datetime after the range of the data
  # This leaves out entries that are entirely before or after the range of the data.
  exist <- DBI::dbGetQuery(con, paste0(
    "SELECT approval_id, timeseries_id, approval_type_id, start_dt, end_dt 
    FROM approvals 
    WHERE timeseries_id = ", timeseries_id, " 
       AND (
            (
              end_dt >= '", min(data$datetime), "'
              AND end_dt <= '", max(data$datetime), "'
            )
         OR (
              start_dt >= '", min(data$datetime), "'
              AND start_dt <= '", max(data$datetime), "'
             )
         OR (
              start_dt <= '", min(data$datetime), "'
              AND end_dt >= '", max(data$datetime), "'
            )
          )
          ORDER BY start_dt ASC;
    "))
  
  original_exist_rows <- nrow(exist)
  
  if (original_exist_rows == 0) {
    exist <- data.frame(approval_id = NA,
                        timeseries_id = timeseries_id,
                        approval_type_id = data$approval[1],
                        start_dt = data$datetime[1],
                        end_dt = data$datetime[1])
  }
  
  # Collapse consecutive rows with the same approval using run-length encoding
  data <- data[order(data$datetime), ]
  runs <- rle(data$approval)
  ends <- cumsum(runs$lengths)
  starts <- c(1, head(ends, -1) + 1)
  new_segments <- data.frame(
    approval_id      = NA,
    timeseries_id     = timeseries_id,  # assume defined in your environment
    approval_type_id = runs$values,
    start_dt          = data$datetime[starts],
    end_dt            = data$datetime[ends],
    stringsAsFactors  = FALSE
  )
  
  current <- if (original_exist_rows > 0) exist$approval_type_id[1] else new_segments$approval_type_id[1]
  
  index <- 1 # keeps track of the row we should be modifying in 'exist'
  
  # Now loop through the data to find where the approval_type_id changes
  for (i in 1:nrow(new_segments)) {
    if (new_segments$approval_type_id[i] != current) {
      if (index <= original_exist_rows) { # Modify rows in 'exist'
        if (index == 1) { # If still on first row, check if its start_dt needs to be modified
          if (exist$start_dt[index] > new_segments$start_dt[i]) { # If the start_dt of the first row in 'exist' is later than the start_dt of the first row in 'new_segments', adjust it
            exist$start_dt[index] <- new_segments$start_dt[i]
          } # it's possible that the user has provided data that starts *after* the full record, so don't adjust the start_dt of the first row in that case
        } else {
          exist$start_dt[index] <- new_segments$start_dt[i]
        }
        exist$approval_type_id[index] <- new_segments$approval_type_id[i]
        if (index != nrow(exist)) { # Adjust the end_dt of this approval_type_id if it's not the last row of 'exist' (otherwise we risk truncating a time period if the provided data does not go to the end of the timeseries)
          exist$end_dt[index] <- new_segments$end_dt[i] 
        }
        if (i < nrow(new_segments)) {
          index <- index + 1
          current <- new_segments$approval_type_id[i]
        }
      } else { # Create new rows with no approval_id
        # Modify the last row in 'exist' and add a new row
        if (index > 1) {
          exist$end_dt[nrow(exist)] <- new_segments$end_dt[i - 1]
        }
        to_append <- data.frame(approval_id = NA,
                                timeseries_id = timeseries_id,
                                approval_type_id = new_segments$approval_type_id[i],
                                start_dt = new_segments$start_dt[i],
                                end_dt = new_segments$end_dt[i])
        exist <- rbind(exist, to_append)
        index <- nrow(exist)
      }
      current <- new_segments$approval_type_id[i]
    } else { # If the approval_type_id is the same as the last one, check and adjust datetimes
      if (index <= original_exist_rows) { # Modify rows in 'exist'
        if (index == 1) { # If still on first row, check if its start_dt needs to be modified
          if (exist$start_dt[index] > new_segments$start_dt[i]) { # If the start_dt of the first row in 'exist' is later than the start_dt of the first row in 'new_segments', adjust it
            exist$start_dt[index] <- new_segments$start_dt[i]
          } # it's possible that the user has provided data that starts *after* the full record, so don't adjust the start_dt of the first row in that case
        } else {
          exist$start_dt[index] <- new_segments$start_dt[i]
        }
        if (index != nrow(exist)) { # Adjust the end_dt of this approval_type_id if it's not the last row of 'exist' (otherwise we risk truncating a time period if the provided data does not go to the end of the timeseries)
          exist$end_dt[index] <- new_segments$end_dt[i] 
        }
        if (i < nrow(new_segments)) {
          index <- index + 1
        }
      } else { # Create new rows with no approval_id
        # Modify the last row in 'exist' and add a new row
        if (index > 1) {
          exist$end_dt[nrow(exist)] <- new_segments$end_dt[i - 1]
        }
        to_append <- data.frame(approval_id = NA,
                                timeseries_id = timeseries_id,
                                approval_type_id = new_segments$approval_type_id[i],
                                start_dt = new_segments$start_dt[i],
                                end_dt = new_segments$end_dt[i])
        exist <- rbind(exist, to_append)
        index <- nrow(exist)
      }
    }
    
    if (i == nrow(new_segments)) {
      exist$end_dt[index] <- new_segments$end_dt[i] # Adjust the end_dt of this approval
    }
  }  # End of for loop
  
  # if there are untouched rows in exist check if they interfere with the last end_dt. If they do, they either need adjustments to their start_dt or need to be remove form the database
  if (index < nrow(exist)) {
    # Pull out the rows that are left over
    leftovers <- exist[c((index + 1):nrow(exist)), ]
    exist <- exist[c(1:index), ]
    # Find entries that now have a start_dt and end_dt entirely captured by other rows. Label them with timeseries_id = -1 to remove later
    leftovers[leftovers$start_dt <= exist$end_dt[index] & leftovers$end_dt <= exist$end_dt[index], "timeseries_id"] <- -1
    # Find entries that have a start_dt before the end_dt of the last row, but end_dt after the end_dt of the last row. Adjust their start_dt to be the same as the end_dt of the last row. These won't be removed. This *should* only ever be a single row because of DB constraints.
    leftovers[leftovers$start_dt < exist$end_dt[index] & leftovers$end_dt > exist$end_dt[index], "start_dt"] <- exist$end_dt[index]
    
    exist <- rbind(exist, leftovers)
  }
  
  # Now commit the changes to the database
  commit_fx <- function(con, exist) {
    remove <- exist[exist$timeseries_id == -1, "approval_id"]
    exist <- exist[exist$timeseries_id != -1, ]
    if (length(remove) > 0) {
      DBI::dbExecute(con, paste0("DELETE FROM approvals WHERE approval_id IN (", paste(remove, collapse = ", "), ");"))
    }
    for (i in 1:nrow(exist)) {
      if (!is.na(exist$approval_id[i])) {  # Means that we need to update rows
        DBI::dbExecute(con, paste0("UPDATE approvals SET approval_type_id = ", exist$approval_type_id[i], ", start_dt = '", exist$start_dt[i], "', end_dt = '", exist$end_dt[i], "' WHERE approval_id = ", exist$approval_id[i], ";"))
      } else { # Means that we need to insert new rows
        DBI::dbAppendTable(con, "approvals", exist[i, -which(names(exist) == "approval_id")])
      }
    }
  }
  
  if (!attr(con, "active_transaction")) {
    DBI::dbBegin(con)
    attr(con, "active_transaction") <- TRUE
    tryCatch({
      commit_fx(con, exist)
      DBI::dbCommit(con)
      attr(con, "active_transaction") <- FALSE
    }, error = function(e) {
      DBI::dbRollback(con)
      attr(con, "active_transaction") <<- FALSE
      warning("adjust_approval: Failed to commit changes to the database with error ", e$message)
    })
  } else { # we're already in a transaction
    commit_fx(con, exist)
  }
} # End of adjust_approval function



#' Adjust the owner of a timeseries in the database
#' 
#' @param con A connection to the database with write privileges to the 'owners' and 'measurements_continuous' tables.
#' @param timeseries_id The target timeseries_id
#' @param data A data.frame with columns for 'datetime' and 'owner'. 'datetime' should be POSIXct and 'owner' should be either character (in which case it must refer to entries in column 'name' of table 'organizations' or integer/numeric, in which case it must refer to column 'organization_id' of the same table.
#'  
#' @return Modifies the 'owners' table in the database.
#' @export

adjust_owner <- function(con, timeseries_id, data) {
  
  # If a column 'date' and no column 'datetime' is present, rename 'date' to 'datetime' and convert to POSIXct
  if ("date" %in% names(data) & !"datetime" %in% names(data)) {
    data$datetime <- as.POSIXct(data$date, tz = "UTC")
    data <- data[, !names(data) == "date"]
  }
  
  # Make sure that column 'owner' is not all NA
  if (all(is.na(data$owner))) {
    return(message("adjust_owner: column 'owner' was all NA, skipped. Applies to timeseries_id ", timeseries_id, "."))
  }
  
  # Check if 'owner' is character, if so match those characters to 'name' in the 'organizations' table
  if (inherits(data$owner[1], "character")) {
    owner_table <- DBI::dbGetQuery(con, "SELECT organization_id, name FROM organizations;")
    data$owner <- owner_table$organization_id[match(data$owner, owner_table$name)]
  }
  
  # Ensure that 'datetime' is POSIXct
  if (!inherits(data$datetime[1], "POSIXct")) {
    stop("Column 'datetime' must be of class POSIXct.")
  }
  
  # Get the data where at least one of the following is true:
  # has an end datetime within the range of the data
  # has a start datetime within the range of the data
  # has a start datetime before the range of the data and an end datetime after the range of the data
  # This leaves out entries that are entirely before or after the range of the data.
  exist <- DBI::dbGetQuery(con, paste0(
    "SELECT owner_id, timeseries_id, organization_id, start_dt, end_dt 
    FROM owners 
    WHERE timeseries_id = ", timeseries_id, " 
       AND (
            (
              end_dt >= '", min(data$datetime), "'
              AND end_dt <= '", max(data$datetime), "'
            )
         OR (
              start_dt >= '", min(data$datetime), "'
              AND start_dt <= '", max(data$datetime), "'
             )
         OR (
              start_dt <= '", min(data$datetime), "'
              AND end_dt >= '", max(data$datetime), "'
             )
          )
          ORDER BY start_dt ASC;
    "))
  
  original_exist_rows <- nrow(exist)
  
  if (original_exist_rows == 0) {
    exist <- data.frame(owner_id = NA,
                        timeseries_id = timeseries_id,
                        organization_id = data$owner[1],
                        start_dt = data$datetime[1],
                        end_dt = data$datetime[1])
  }
  
  # Collapse consecutive rows with the same owner using run-length encoding
  data <- data[order(data$datetime), ]
  runs <- rle(data$owner)
  ends <- cumsum(runs$lengths)
  starts <- c(1, head(ends, -1) + 1)
  new_segments <- data.frame(
    owner_id      = NA,
    timeseries_id     = timeseries_id,  # assume defined in your environment
    organization_id = runs$values,
    start_dt          = data$datetime[starts],
    end_dt            = data$datetime[ends],
    stringsAsFactors  = FALSE
  )
  
  current <- if (original_exist_rows > 0) exist$organization_id[1] else new_segments$organization_id[1]
  
  index <- 1 # keeps track of the row we should be modifying in 'exist'
  
  # Now loop through the data to find where the organization_id changes
  for (i in 1:nrow(new_segments)) {
    if (new_segments$organization_id[i] != current) {
      if (index <= original_exist_rows) { # Modify rows in 'exist'
        if (index == 1) { # If still on first row, check if its start_dt needs to be modified
          if (exist$start_dt[index] > new_segments$start_dt[i]) { # If the start_dt of the first row in 'exist' is later than the start_dt of the first row in 'new_segments', adjust it
            exist$start_dt[index] <- new_segments$start_dt[i]
          } # it's possible that the user has provided data that starts *after* the full record, so don't adjust the start_dt of the first row in that case
        } else {
          exist$start_dt[index] <- new_segments$start_dt[i]
        }
        exist$organization_id[index] <- new_segments$organization_id[i]
        if (index != nrow(exist)) { # Adjust the end_dt of this organization_id if it's not the last row of 'exist' (otherwise we risk truncating a time period if the provided data does not go to the end of the timeseries)
          exist$end_dt[index] <- new_segments$end_dt[i] 
        }
        if (i < nrow(new_segments)) {
          index <- index + 1
          current <- new_segments$organization_id[i]
        }
      } else { # Create new rows with no owner_id
        # Modify the last row in 'exist' and add a new row
        if (index > 1) {
          exist$end_dt[nrow(exist)] <- new_segments$end_dt[i - 1]
        }
        to_append <- data.frame(owner_id = NA,
                                timeseries_id = timeseries_id,
                                organization_id = new_segments$organization_id[i],
                                start_dt = new_segments$start_dt[i],
                                end_dt = new_segments$end_dt[i])
        exist <- rbind(exist, to_append)
        index <- nrow(exist)
      }
      current <- new_segments$organization_id[i]
    } else { # If the organization_id is the same as the last one, check and adjust datetimes
      if (index <= original_exist_rows) { # Modify rows in 'exist'
        if (index == 1) { # If still on first row, check if its start_dt needs to be modified
          if (exist$start_dt[index] > new_segments$start_dt[i]) { # If the start_dt of the first row in 'exist' is later than the start_dt of the first row in 'new_segments', adjust it
            exist$start_dt[index] <- new_segments$start_dt[i]
          } # it's possible that the user has provided data that starts *after* the full record, so don't adjust the start_dt of the first row in that case
        } else {
          exist$start_dt[index] <- new_segments$start_dt[i]
        }
        if (index != nrow(exist)) { # Adjust the end_dt of this organization_id if it's not the last row of 'exist' (otherwise we risk truncating a time period if the provided data does not go to the end of the timeseries)
          exist$end_dt[index] <- new_segments$end_dt[i] 
        }
        if (i < nrow(new_segments)) {
          index <- index + 1
        }
      } else { # Create new rows with no owner_id
        # Modify the last row in 'exist' and add a new row
        if (index > 1) {
          exist$end_dt[nrow(exist)] <- new_segments$end_dt[i - 1]
        }
        to_append <- data.frame(owner_id = NA,
                                timeseries_id = timeseries_id,
                                organization_id = new_segments$organization_id[i],
                                start_dt = new_segments$start_dt[i],
                                end_dt = new_segments$end_dt[i])
        exist <- rbind(exist, to_append)
        index <- nrow(exist)
      }
    }
    
    if (i == nrow(new_segments)) {
      exist$end_dt[index] <- new_segments$end_dt[i] # Adjust the end_dt of this owner
    }
  }  # End of for loop
  
  # if there are untouched rows in exist check if they interfere with the last end_dt. If they do, they either need adjustments to their start_dt or need to be remove form the database
  if (index < nrow(exist)) {
    # Pull out the rows that are left over
    leftovers <- exist[c((index + 1):nrow(exist)), ]
    exist <- exist[c(1:index), ]
    # Find entries that now have a start_dt and end_dt entirely captured by other rows. Label them with timeseries_id = -1 to remove later
    leftovers[leftovers$start_dt <= exist$end_dt[index] & leftovers$end_dt <= exist$end_dt[index], "timeseries_id"] <- -1
    # Find entries that have a start_dt before the end_dt of the last row, but end_dt after the end_dt of the last row. Adjust their start_dt to be the same as the end_dt of the last row. These won't be removed. This *should* only ever be a single row because of DB constraints.
    leftovers[leftovers$start_dt < exist$end_dt[index] & leftovers$end_dt > exist$end_dt[index], "start_dt"] <- exist$end_dt[index]
    
    exist <- rbind(exist, leftovers)
  }
  
  # Now commit the changes to the database
  commit_fx <- function(con, exist) {
    remove <- exist[exist$timeseries_id == -1, "owner_id"]
    exist <- exist[exist$timeseries_id != -1, ]
    if (length(remove) > 0) {
      DBI::dbExecute(con, paste0("DELETE FROM owners WHERE owner_id IN (", paste(remove, collapse = ", "), ");"))
    }
    for (i in 1:nrow(exist)) {
      if (!is.na(exist$owner_id[i])) {  # Means that we need to update rows
        DBI::dbExecute(con, paste0("UPDATE owners SET organization_id = ", exist$organization_id[i], ", start_dt = '", exist$start_dt[i], "', end_dt = '", exist$end_dt[i], "' WHERE owner_id = ", exist$owner_id[i], ";"))
      } else { # Means that we need to insert new rows
        DBI::dbAppendTable(con, "owners", exist[i, -which(names(exist) == "owner_id")])
      }
    }
  }
  
  if (!attr(con, "active_transaction")) {
    DBI::dbBegin(con)
    attr(con, "active_transaction") <- TRUE
    tryCatch({
      commit_fx(con, exist)
      DBI::dbCommit(con)
      attr(con, "active_transaction") <- FALSE
    }, error = function(e) {
      DBI::dbRollback(con)
      attr(con, "active_transaction") <<- FALSE
      warning("adjust_owner: Failed to commit changes to the database with error ", e$message)
    })
  } else { # we're already in a transaction
    commit_fx(con, exist)
  }
  
} # End of adjust_owner function


#' Adjust the contributor of a timeseries in the database
#' 
#' @param con A connection to the database with write privileges to the 'contributors' and 'measurements_continuous' tables.
#' @param timeseries_id The target timeseries_id
#' @param data A data.frame with columns for 'datetime' and 'contributor'. 'datetime' should be POSIXct and 'contributor' should be either character (in which case it must refer to entries in column 'name' of table 'organizations' or integer/numeric, in which case it must refer to column 'organization_id' of the same table.
#'  
#' @return Modifies the 'contributors' table in the database.
#' @export

adjust_contributor <- function(con, timeseries_id, data) {
  
  # If a column 'date' and no column 'datetime' is present, rename 'date' to 'datetime' and convert to POSIXct
  if ("date" %in% names(data) & !"datetime" %in% names(data)) {
    data$datetime <- as.POSIXct(data$date, tz = "UTC")
    data <- data[, !names(data) == "date"]
  }
  
  # Make sure that column 'contributor' is not all NA
  if (all(is.na(data$contributor))) {
    return(message("adjust_contributor: column 'contributor' was all NA, skipped. Applies to timeseries_id ", timeseries_id, "."))
  }
  
  # Check if 'contributor' is character, if so match those characters to 'name' in the 'organizations' table
  if (inherits(data$contributor[1], "character")) {
    contributor_table <- DBI::dbGetQuery(con, "SELECT organization_id, name FROM organizations;")
    data$contributor <- contributor_table$organization_id[match(data$contributor, contributor_table$name)]
  }
  
  # Ensure that 'datetime' is POSIXct
  if (!inherits(data$datetime[1], "POSIXct")) {
    stop("Column 'datetime' must be of class POSIXct.")
  }
  
  # Get the data where at least one of the following is true:
  # has an end datetime within the range of the data
  # has a start datetime within the range of the data
  # has a start datetime before the range of the data and an end datetime after the range of the data
  # This leaves out entries that are entirely before or after the range of the data.
  exist <- DBI::dbGetQuery(con, paste0(
    "SELECT contributor_id, timeseries_id, organization_id, start_dt, end_dt 
    FROM contributors 
    WHERE timeseries_id = ", timeseries_id, " 
       AND (
            (
              end_dt >= '", min(data$datetime), "'
              AND end_dt <= '", max(data$datetime), "'
            )
         OR (
              start_dt >= '", min(data$datetime), "'
              AND start_dt <= '", max(data$datetime), "'
             )
         OR (
              start_dt <= '", min(data$datetime), "'
              AND end_dt >= '", max(data$datetime), "'
             )
          )
          ORDER BY start_dt ASC;
    "))
  
  original_exist_rows <- nrow(exist)
  
  if (original_exist_rows == 0) {
    exist <- data.frame(contributor_id = NA,
                        timeseries_id = timeseries_id,
                        organization_id = data$contributor[1],
                        start_dt = data$datetime[1],
                        end_dt = data$datetime[1])
  }
  
  # Collapse consecutive rows with the same contributor using run-length encoding
  data <- data[order(data$datetime), ]
  runs <- rle(data$contributor)
  ends <- cumsum(runs$lengths)
  starts <- c(1, head(ends, -1) + 1)
  new_segments <- data.frame(
    contributor_id      = NA,
    timeseries_id     = timeseries_id,  # assume defined in your environment
    organization_id = runs$values,
    start_dt          = data$datetime[starts],
    end_dt            = data$datetime[ends],
    stringsAsFactors  = FALSE
  )
  
  current <- if (original_exist_rows > 0) exist$organization_id[1] else new_segments$organization_id[1]
  
  index <- 1 # keeps track of the row we should be modifying in 'exist'
  
  # Now loop through the data to find where the organization_id changes
  for (i in 1:nrow(new_segments)) {
    if (new_segments$organization_id[i] != current) {
      if (index <= original_exist_rows) { # Modify rows in 'exist'
        if (index == 1) { # If still on first row, check if its start_dt needs to be modified
          if (exist$start_dt[index] > new_segments$start_dt[i]) { # If the start_dt of the first row in 'exist' is later than the start_dt of the first row in 'new_segments', adjust it
            exist$start_dt[index] <- new_segments$start_dt[i]
          } # it's possible that the user has provided data that starts *after* the full record, so don't adjust the start_dt of the first row in that case
        } else {
          exist$start_dt[index] <- new_segments$start_dt[i]
        }
        exist$organization_id[index] <- new_segments$organization_id[i]
        if (index != nrow(exist)) { # Adjust the end_dt of this organization_id if it's not the last row of 'exist' (otherwise we risk truncating a time period if the provided data does not go to the end of the timeseries)
          exist$end_dt[index] <- new_segments$end_dt[i] 
        }
        if (i < nrow(new_segments)) {
          index <- index + 1
          current <- new_segments$organization_id[i]
        }
      } else { # Create new rows with no contributor_id
        # Modify the last row in 'exist' and add a new row
        if (index > 1) {
          exist$end_dt[nrow(exist)] <- new_segments$end_dt[i - 1]
        }
        to_append <- data.frame(contributor_id = NA,
                                timeseries_id = timeseries_id,
                                organization_id = new_segments$organization_id[i],
                                start_dt = new_segments$start_dt[i],
                                end_dt = new_segments$end_dt[i])
        exist <- rbind(exist, to_append)
        index <- nrow(exist)
      }
      current <- new_segments$organization_id[i]
    } else { # If the organization_id is the same as the last one, check and adjust datetimes
      if (index <= original_exist_rows) { # Modify rows in 'exist'
        if (index == 1) { # If still on first row, check if its start_dt needs to be modified
          if (exist$start_dt[index] > new_segments$start_dt[i]) { # If the start_dt of the first row in 'exist' is later than the start_dt of the first row in 'new_segments', adjust it
            exist$start_dt[index] <- new_segments$start_dt[i]
          } # it's possible that the user has provided data that starts *after* the full record, so don't adjust the start_dt of the first row in that case
        } else {
          exist$start_dt[index] <- new_segments$start_dt[i]
        }
        if (index != nrow(exist)) { # Adjust the end_dt of this organization_id if it's not the last row of 'exist' (otherwise we risk truncating a time period if the provided data does not go to the end of the timeseries)
          exist$end_dt[index] <- new_segments$end_dt[i] 
        }
        if (i < nrow(new_segments)) {
          index <- index + 1
        }
      } else { # Create new rows with no contributor_id
        # Modify the last row in 'exist' and add a new row
        if (index > 1) {
          exist$end_dt[nrow(exist)] <- new_segments$end_dt[i - 1]
        }
        to_append <- data.frame(contributor_id = NA,
                                timeseries_id = timeseries_id,
                                organization_id = new_segments$organization_id[i],
                                start_dt = new_segments$start_dt[i],
                                end_dt = new_segments$end_dt[i])
        exist <- rbind(exist, to_append)
        index <- nrow(exist)
      }
    }
    
    if (i == nrow(new_segments)) {
      exist$end_dt[index] <- new_segments$end_dt[i] # Adjust the end_dt of this contributor
    }
  }  # End of for loop
  
  # if there are untouched rows in exist check if they interfere with the last end_dt. If they do, they either need adjustments to their start_dt or need to be remove form the database
  if (index < nrow(exist)) {
    # Pull out the rows that are left over
    leftovers <- exist[c((index + 1):nrow(exist)), ]
    exist <- exist[c(1:index), ]
    # Find entries that now have a start_dt and end_dt entirely captured by other rows. Label them with timeseries_id = -1 to remove later
    leftovers[leftovers$start_dt <= exist$end_dt[index] & leftovers$end_dt <= exist$end_dt[index], "timeseries_id"] <- -1
    # Find entries that have a start_dt before the end_dt of the last row, but end_dt after the end_dt of the last row. Adjust their start_dt to be the same as the end_dt of the last row. These won't be removed. This *should* only ever be a single row because of DB constraints.
    leftovers[leftovers$start_dt < exist$end_dt[index] & leftovers$end_dt > exist$end_dt[index], "start_dt"] <- exist$end_dt[index]
    
    exist <- rbind(exist, leftovers)
  }
  
  # Now commit the changes to the database
  commit_fx <- function(con, exist) {
    remove <- exist[exist$timeseries_id == -1, "contributor_id"]
    exist <- exist[exist$timeseries_id != -1, ]
    if (length(remove) > 0) {
      DBI::dbExecute(con, paste0("DELETE FROM contributors WHERE contributor_id IN (", paste(remove, collapse = ", "), ");"))
    }
    for (i in 1:nrow(exist)) {
      if (!is.na(exist$contributor_id[i])) {  # Means that we need to update rows
        DBI::dbExecute(con, paste0("UPDATE contributors SET organization_id = ", exist$organization_id[i], ", start_dt = '", exist$start_dt[i], "', end_dt = '", exist$end_dt[i], "' WHERE contributor_id = ", exist$contributor_id[i], ";"))
      } else { # Means that we need to insert new rows
        DBI::dbAppendTable(con, "contributors", exist[i, -which(names(exist) == "contributor_id")])
      }
    }
  }
  
  if (!attr(con, "active_transaction")) {
    DBI::dbBegin(con)
    attr(con, "active_transaction") <- TRUE
    tryCatch({
      commit_fx(con, exist)
      DBI::dbCommit(con)
      attr(con, "active_transaction") <- FALSE
    }, error = function(e) {
      DBI::dbRollback(con)
      attr(con, "active_transaction") <<- FALSE
      warning("adjust_contributor: Failed to commit changes to the database with error ", e$message)
    })
  } else { # we're already in a transaction
    commit_fx(con, exist)
  }
  
} # End of adjust_contributor function
