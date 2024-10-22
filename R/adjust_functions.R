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
  
  # Make sure that column 'grade' is not all NA
  if (all(is.na(data$grade))) {
    return(message("adjust_grade: column 'grade' was all NA, skipped."))
  }
  
  # Check if 'grade' is character, if so match those characters to 'grade_type_code' in the 'grades' table
  if (inherits(data$grade[1], "character")) {
    grade_table <- DBI::dbGetQuery(con, "SELECT grade_type_id, grade_type_code FROM grade_types;")
    data$grade <- grade_table$grade_type_id[match(data$grade, grade_table$grade_type_code)]
  }
  
  # Ensure that 'datetime' is POSIXct
  if (!inherits(data$datetime[1], "POSIXct")) {
    stop("Column'datetime' must be of class POSIXct.")
  }

  unknown_grade <- DBI::dbGetQuery(con, "SELECT grade_type_id FROM grade_types WHERE grade_type_code = 'UNK'")[1,1]
  
  data$grade[is.na(data$grade)] <- unknown_grade
  
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
          )
    ORDER BY start_dt ASC;
    "))
  
  
  data <- data[order(data$datetime), ]
  
  index <- 1
  original_exist_rows <- nrow(exist)
  current <- if (original_exist_rows > 0) exist$grade_type_id[1] else data$grade[1]
  
  if (original_exist_rows == 0) {
    exist <- data.frame(grade_id = NA,
                        timeseries_id = timeseries_id,
                        grade_type_id = data$grade[1],
                        start_dt = data$datetime[1],
                        end_dt = data$datetime[1])
  }
  
  # Now loop through the data to find where the grade changes
  for (i in 1:nrow(data)) {
    if (data$grade[i] != current) {
      if (index <= original_exist_rows) { # Modify rows in 'exist'
        exist$end_dt[index] <- data$datetime[i]
        
        index <- index + 1
        if (index <= original_exist_rows) { # Modify the next row in 'exist'
          exist$start_dt[index] <- data$datetime[i]
          exist$grade_type_id[index] <- data$grade[i]
          
          # Check if there are rows left in 'data'
          if ((i == nrow(data)) && ((index) < original_exist_rows)) {
            # In this case we may need to adjust things: the next row in 'exist' needs its start_dt to be the same as the end_dt of the last data point. However, if this results in start_dt to be later than end_dt, then the next row(s) of exist gets dropped.
            # Drop all rows where end_dt is before the last data point
            exist <- exist[exist$end_dt <= data$datetime[i], ]
            # See if the start_dt of the next row in 'exist' is earlier than data$datetime[i]; if so, adjust it to be the same as data$datetime[i]. There should not be any overlaps created by thins since rows where end_dt is before the last data point are dropped already.
            if (nrow(exist) > index) {
              if (exist$start_dt[index + 1] < data$datetime[i]) {
                exist$start_dt[index + 1] <- data$datetime[i]
              }
            }
          }
        } else { # Create a new row with no grade_id
          to_append <- data.frame(grade_id = NA,
                                  timeseries_id = timeseries_id,
                                  grade_type_id = data$grade[i],
                                  start_dt = data$datetime[i],
                                  end_dt = data$datetime[i])
          exist <- rbind(exist, to_append)
          # from here on we only create new rows in 'exist'
        }
      } else { # Create new rows with no grade_id
        # Modify the last row in 'exist' and add a new row
        exist$end_dt[nrow(exist)] <- data$datetime[i]
        to_append <- data.frame(grade_id = NA,
                                timeseries_id = timeseries_id,
                                grade_type_id = data$grade[i],
                                start_dt = data$datetime[i],
                                end_dt = data$datetime[i])
        exist <- rbind(exist, to_append)
        index <- nrow(exist)
      }
      current <- data$grade[i]
    }
    if (i == nrow(data)) {
      exist$end_dt[index] <- data$datetime[i]
    }
  }  # End of for loop
  
  # if there untouched rows in exist check if they interfere with the last end_dt. If they do, they either need adjustments to their start_dt or need to be removed.
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
    for (i in 1:nrow(exist)) {
      if (!is.na(exist$grade_id[i])) {  # Means that we need to update rows
        if (exist$timeseries_id[i] == -1) { # These need to be removed from the DB based on their grade_id
          DBI::dbExecute(con, paste0("DELETE FROM grades WHERE grade_id = ", exist$grade_id[i], ";"))
          next
        }
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
  
  # Make sure that column 'qualifier' is not all NA
  if (all(is.na(data$qualifier))) {
    return(message("adjust_qualifier: column 'qualifier' was all NA, skipped."))
  }
  
  # Check if 'qualifier' is character, if so match those characters to 'qualifier_type_code' in the 'qualifiers' table
  if (inherits(data$qualifier[1], "character")) {
    qualifier_table <- DBI::dbGetQuery(con, "SELECT qualifier_type_id, qualifier_type_code FROM qualifier_types;")
    data$qualifier <- qualifier_table$qualifier_type_id[match(data$qualifier, qualifier_table$qualifier_type_code)]
  }
  
  # Ensure that 'datetime' is POSIXct
  if (!inherits(data$datetime[1], "POSIXct")) {
    stop("Column'datetime' must be of class POSIXct.")
  }
  
  unknown_qualifier <- DBI::dbGetQuery(con, "SELECT qualifier_type_id FROM qualifier_types WHERE qualifier_type_code = 'UNK'")[1,1]
  
  data$qualifier[is.na(data$qualifier)] <- unknown_qualifier
  
  # Pull the qualifiers table entries for this timeseries_id where the datetime is within the range of the data
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
          )
          ORDER BY start_dt ASC;
    "))
  
  data <- data[order(data$datetime), ]
  
  index <- 1
  original_exist_rows <- nrow(exist)
  current <- if (original_exist_rows > 0) exist$qualifier_type_id[1] else data$qualifier[1]
  
  if (original_exist_rows == 0) {
    exist <- data.frame(qualifier_id = NA,
                        timeseries_id = timeseries_id,
                        qualifier_type_id = data$qualifier[1],
                        start_dt = data$datetime[1],
                        end_dt = data$datetime[1])
  }
  
  # Now loop through the data to find where the qualifier changes
  for (i in 1:nrow(data)) {
    if (data$qualifier[i] != current) {
      if (index <= original_exist_rows) { # Modify rows in 'exist'
        exist$end_dt[index] <- data$datetime[i]
        
        index <- index + 1
        if (index <= original_exist_rows) { # Modify the next row in 'exist'
          exist$start_dt[index] <- data$datetime[i]
          exist$qualifier_type_id[index] <- data$qualifier[i]
          
          # Check if there are rows left in 'data'
          if ((i == nrow(data)) && ((index) < original_exist_rows)) {
            # In this case we may need to adjust things: the next row in 'exist' needs its start_dt to be the same as the end_dt of the last data point. However, if this results in start_dt to be later than end_dt, then the next row(s) of exist gets dropped.
            # Drop all rows where end_dt is before the last data point
            exist <- exist[exist$end_dt <= data$datetime[i], ]
            # See if the start_dt of the next row in 'exist' is earlier than data$datetime[i]; if so, adjust it to be the same as data$datetime[i]. There should not be any overlaps created by thins since rows where end_dt is before the last data point are dropped already.
            if (nrow(exist) > index) {
              if (exist$start_dt[index + 1] < data$datetime[i]) {
                exist$start_dt[index + 1] <- data$datetime[i]
              }
            }
          }
        } else { # Create a new row with no qualifier_id
          to_append <- data.frame(qualifier_id = NA,
                                  timeseries_id = timeseries_id,
                                  qualifier_type_id = data$qualifier[i],
                                  start_dt = data$datetime[i],
                                  end_dt = data$datetime[i])
          exist <- rbind(exist, to_append)
          # from here on we only create new rows in 'exist'
        }
      } else { # Create new rows with no qualifier_id
        # Modify the last row in 'exist' and add a new row
        exist$end_dt[nrow(exist)] <- data$datetime[i]
        to_append <- data.frame(qualifier_id = NA,
                                timeseries_id = timeseries_id,
                                qualifier_type_id = data$qualifier[i],
                                start_dt = data$datetime[i],
                                end_dt = data$datetime[i])
        exist <- rbind(exist, to_append)
        index <- nrow(exist)
      }
      current <- data$qualifier[i]
    }
    if (i == nrow(data)) {
      exist$end_dt[index] <- data$datetime[i]
    }
  }  # End of for loop
  
  # if there untouched rows in exist check if they interfere with the last end_dt. If they do, they either need adjustments to their start_dt or need to be removed.
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
    for (i in 1:nrow(exist)) {
      if (!is.na(exist$qualifier_id[i])) {  # Means that we need to update rows
        if (exist$timeseries_id[i] == -1) { # These need to be removed from the DB based on their qualifier_id
          DBI::dbExecute(con, paste0("DELETE FROM qualifiers WHERE qualifier_id = ", exist$qualifier_id[i], ";"))
          next
        }
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
  
  # Make sure that column 'approval' is not all NA
  if (all(is.na(data$approval))) {
    return(message("adjust_owner: column 'approval' was all NA, skipped."))
  }
  
  # Check if 'approval' is character, if so match those characters to 'approval_type_code' in the 'approvals' table
  if (inherits(data$approval[1], "character")) {
    approval_table <- DBI::dbGetQuery(con, "SELECT approval_type_id, approval_type_code FROM approval_types;")
    data$approval <- approval_table$approval_type_id[match(data$approval, approval_table$approval_type_code)]
  }
  
  # Ensure that 'datetime' is POSIXct
  if (!inherits(data$datetime[1], "POSIXct")) {
    stop("Column'datetime' must be of class POSIXct.")
  }
  
  unknown_approval <- DBI::dbGetQuery(con, "SELECT approval_type_id FROM approval_types WHERE approval_type_code = 'UNK'")[1,1]
  
  data$approval[is.na(data$approval)]  <- unknown_approval
  
  # Pull the approvals table entries for this timeseries_id where the datetime is within the range of the data
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
          )
          ORDER BY start_dt ASC;
    "))
  
  data <- data[order(data$datetime), ]
  
  index <- 1
  original_exist_rows <- nrow(exist)
  current <- if (original_exist_rows > 0) exist$approval_type_id[1] else data$approval[1]
  
  if (original_exist_rows == 0) {
    exist <- data.frame(approval_id = NA,
                        timeseries_id = timeseries_id,
                        approval_type_id = data$approval[1],
                        start_dt = data$datetime[1],
                        end_dt = data$datetime[1])
  }
  
  # Now loop through the data to find where the approval changes
  for (i in 1:nrow(data)) {
    if (data$approval[i] != current) {
      if (index <= original_exist_rows) { # Modify rows in 'exist'
        exist$end_dt[index] <- data$datetime[i]
        
        index <- index + 1
        if (index <= original_exist_rows) { # Modify the next row in 'exist'
          exist$start_dt[index] <- data$datetime[i]
          exist$approval_type_id[index] <- data$approval[i]
          
          # Check if there are rows left in 'data'
          if ((i == nrow(data)) && ((index) < original_exist_rows)) {
            # In this case we may need to adjust things: the next row in 'exist' needs its start_dt to be the same as the end_dt of the last data point. However, if this results in start_dt to be later than end_dt, then the next row(s) of exist gets dropped.
            # Drop all rows where end_dt is before the last data point
            exist <- exist[exist$end_dt <= data$datetime[i], ]
            # See if the start_dt of the next row in 'exist' is earlier than data$datetime[i]; if so, adjust it to be the same as data$datetime[i]. There should not be any overlaps created by thins since rows where end_dt is before the last data point are dropped already.
            if (nrow(exist) > index) {
              if (exist$start_dt[index + 1] < data$datetime[i]) {
                exist$start_dt[index + 1] <- data$datetime[i]
              }
            }
          }
        } else { # Create a new row with no approval_id
          to_append <- data.frame(approval_id = NA,
                                  timeseries_id = timeseries_id,
                                  approval_type_id = data$approval[i],
                                  start_dt = data$datetime[i],
                                  end_dt = data$datetime[i])
          exist <- rbind(exist, to_append)
          # from here on we only create new rows in 'exist'
        }
      } else { # Create new rows with no approval_id
        # Modify the last row in 'exist' and add a new row
        exist$end_dt[nrow(exist)] <- data$datetime[i]
        to_append <- data.frame(approval_id = NA,
                                timeseries_id = timeseries_id,
                                approval_type_id = data$approval[i],
                                start_dt = data$datetime[i],
                                end_dt = data$datetime[i])
        exist <- rbind(exist, to_append)
        index <- nrow(exist)
      }
      current <- data$approval[i]
    }
    if (i == nrow(data)) {
      exist$end_dt[index] <- data$datetime[i]
    }
  }  # End of for loop
  
  # if there untouched rows in exist check if they interfere with the last end_dt. If they do, they either need adjustments to their start_dt or need to be removed.
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
    for (i in 1:nrow(exist)) {
      if (!is.na(exist$approval_id[i])) {  # Means that we need to update rows
        if (exist$timeseries_id[i] == -1) { # These need to be removed from the DB based on their approval_id
          DBI::dbExecute(con, paste0("DELETE FROM approvals WHERE approval_id = ", exist$approval_id[i], ";"))
          next
        }
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
#' @param data A data.frame with columns for 'datetime' and 'owner'. 'datetime' should be POSIXct and 'owner' should be either character (in which case it must refer to entries in column 'name' of table 'owners_contributors' or integer/numeric, in which case it must refer to column 'owner_contributor_id' of the same table.
#'  
#' @return Modifies the 'owners' table in the database.
#' @export

adjust_owner <- function(con, timeseries_id, data) {
  
  
  # Make sure that column 'owner' is not all NA
  if (all(is.na(data$owner))) {
    return(message("adjust_owner: column 'owner' was all NA, skipped."))
  }
  
  # Check if 'owner' is character, if so match those characters to 'name' in the 'owners_contributors' table
  if (inherits(data$owner[1], "character")) {
    owner_table <- DBI::dbGetQuery(con, "SELECT owner_contributor_id, name FROM owners_contributors;")
    data$owner <- owner_table$owner_contributor_id[match(data$owner, owner_table$name)]
  }
  
  # Ensure that 'datetime' is POSIXct
  if (!inherits(data$datetime[1], "POSIXct")) {
    stop("Column'datetime' must be of class POSIXct.")
  }
  
  # Pull the owners table entries for this timeseries_id where the datetime is within the range of the data
  exist <- DBI::dbGetQuery(con, paste0(
    "SELECT owner_id, timeseries_id, owner_contributor_id, start_dt, end_dt 
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
          )
          ORDER BY start_dt ASC;
    "))
  
  data <- data[order(data$datetime), ]
  
  index <- 1
  original_exist_rows <- nrow(exist)
  current <- if (original_exist_rows > 0) exist$owner_contributor_id[1] else data$owner[1]
  
  if (original_exist_rows == 0) {
    exist <- data.frame(owner_id = NA,
                        timeseries_id = timeseries_id,
                        owner_contributor_id = data$owner[1],
                        start_dt = data$datetime[1],
                        end_dt = data$datetime[1])
  }
  
  # Now loop through the data to find where the owner changes
  for (i in 1:nrow(data)) {
    if (data$owner[i] != current) {
      if (index <= original_exist_rows) { # Modify rows in 'exist'
        exist$end_dt[index] <- data$datetime[i]
        
        index <- index + 1
        if (index <= original_exist_rows) { # Modify the next row in 'exist'
          exist$start_dt[index] <- data$datetime[i]
          exist$owner_contributor_id[index] <- data$owner[i]
          
          # Check if there are rows left in 'data'
          if ((i == nrow(data)) && ((index) < original_exist_rows)) {
            # In this case we may need to adjust things: the next row in 'exist' needs its start_dt to be the same as the end_dt of the last data point. However, if this results in start_dt to be later than end_dt, then the next row(s) of exist gets dropped.
            # Drop all rows where end_dt is before the last data point
            exist <- exist[exist$end_dt <= data$datetime[i], ]
            # See if the start_dt of the next row in 'exist' is earlier than data$datetime[i]; if so, adjust it to be the same as data$datetime[i]. There should not be any overlaps created by thins since rows where end_dt is before the last data point are dropped already.
            if (nrow(exist) > index) {
              if (exist$start_dt[index + 1] < data$datetime[i]) {
                exist$start_dt[index + 1] <- data$datetime[i]
              }
            }
          }
        } else { # Create a new row with no owner_id
          to_append <- data.frame(owner_id = NA,
                                  timeseries_id = timeseries_id,
                                  owner_contributor_id = data$owner[i],
                                  start_dt = data$datetime[i],
                                  end_dt = data$datetime[i])
          exist <- rbind(exist, to_append)
          # from here on we only create new rows in 'exist'
        }
      } else { # Create new rows with no owner_id
        # Modify the last row in 'exist' and add a new row
        exist$end_dt[nrow(exist)] <- data$datetime[i]
        to_append <- data.frame(owner_id = NA,
                                timeseries_id = timeseries_id,
                                owner_contributor_id = data$owner[i],
                                start_dt = data$datetime[i],
                                end_dt = data$datetime[i])
        exist <- rbind(exist, to_append)
        index <- nrow(exist)
      }
      current <- data$owner[i]
    }
    if (i == nrow(data)) {
      exist$end_dt[index] <- data$datetime[i]
    }
  }  # End of for loop
  
  # if there untouched rows in exist check if they interfere with the last end_dt. If they do, they either need adjustments to their start_dt or need to be removed.
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
    for (i in 1:nrow(exist)) {
      if (!is.na(exist$owner_id[i])) {  # Means that we need to update rows
        if (exist$timeseries_id[i] == -1) { # These need to be removed from the DB based on their owner_id
          DBI::dbExecute(con, paste0("DELETE FROM owners WHERE owner_id = ", exist$owner_id[i], ";"))
          next
        }
        DBI::dbExecute(con, paste0("UPDATE owners SET owner_contributor_id = ", exist$owner_contributor_id[i], ", start_dt = '", exist$start_dt[i], "', end_dt = '", exist$end_dt[i], "' WHERE owner_id = ", exist$owner_id[i], ";"))
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
#' @param data A data.frame with columns for 'datetime' and 'contributor'. 'datetime' should be POSIXct and 'contributor' should be either character (in which case it must refer to entries in column 'name' of table 'owners_contributors' or integer/numeric, in which case it must refer to column 'owner_contributor_id' of the same table.
#'  
#' @return Modifies the 'contributors' table in the database.
#' @export

adjust_contributor <- function(con, timeseries_id, data) {
  
  # Make sure that column 'contributor' is not all NA
  if (all(is.na(data$contributor))) {
    return(message("adjust_contributor: column 'contributor' was all NA, skipped."))
  }
  
  # Check if 'contributor' is character, if so match those characters to 'name' in the 'owners_contributors' table
  if (inherits(data$contributor[1], "character")) {
    contributor_table <- DBI::dbGetQuery(con, "SELECT owner_contributor_id, name FROM owners_contributors;")
    data$contributor <- contributor_table$owner_contributor_id[match(data$contributor, contributor_table$name)]
  }
  
  # Ensure that 'datetime' is POSIXct
  if (!inherits(data$datetime[1], "POSIXct")) {
    stop("Column'datetime' must be of class POSIXct.")
  }
  
  # Pull the contributors table entries for this timeseries_id where the datetime is within the range of the data
  exist <- DBI::dbGetQuery(con, paste0(
    "SELECT contributor_id, timeseries_id, owner_contributor_id, start_dt, end_dt 
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
          )
          ORDER BY start_dt ASC;
    "))
  
  data <- data[order(data$datetime), ]
  
  index <- 1
  original_exist_rows <- nrow(exist)
  current <- if (original_exist_rows > 0) exist$owner_contributor_id[1] else data$contributor[1]
  
  if (original_exist_rows == 0) {
    exist <- data.frame(contributor_id = NA,
                        timeseries_id = timeseries_id,
                        owner_contributor_id = data$contributor[1],
                        start_dt = data$datetime[1],
                        end_dt = data$datetime[1])
  }
  
  # Now loop through the data to find where the contributor changes
  for (i in 1:nrow(data)) {
    if (data$contributor[i] != current) {
      if (index <= original_exist_rows) { # Modify rows in 'exist'
        exist$end_dt[index] <- data$datetime[i]
        
        index <- index + 1
        if (index <= original_exist_rows) { # Modify the next row in 'exist'
          exist$start_dt[index] <- data$datetime[i]
          exist$owner_contributor_id[index] <- data$contributor[i]
          
          # Check if there are rows left in 'data'
          if ((i == nrow(data)) && ((index) < original_exist_rows)) {
            # In this case we may need to adjust things: the next row in 'exist' needs its start_dt to be the same as the end_dt of the last data point. However, if this results in start_dt to be later than end_dt, then the next row(s) of exist gets dropped.
            # Drop all rows where end_dt is before the last data point
            exist <- exist[exist$end_dt <= data$datetime[i], ]
            # See if the start_dt of the next row in 'exist' is earlier than data$datetime[i]; if so, adjust it to be the same as data$datetime[i]. There should not be any overlaps created by thins since rows where end_dt is before the last data point are dropped already.
            if (nrow(exist) > index) {
              if (exist$start_dt[index + 1] < data$datetime[i]) {
                exist$start_dt[index + 1] <- data$datetime[i]
              }
            }
          }
        } else { # Create a new row with no contributor_id
          to_append <- data.frame(contributor_id = NA,
                                  timeseries_id = timeseries_id,
                                  owner_contributor_id = data$contributor[i],
                                  start_dt = data$datetime[i],
                                  end_dt = data$datetime[i])
          exist <- rbind(exist, to_append)
          # from here on we only create new rows in 'exist'
        }
      } else { # Create new rows with no contributor_id
        # Modify the last row in 'exist' and add a new row
        exist$end_dt[nrow(exist)] <- data$datetime[i]
        to_append <- data.frame(contributor_id = NA,
                                timeseries_id = timeseries_id,
                                owner_contributor_id = data$contributor[i],
                                start_dt = data$datetime[i],
                                end_dt = data$datetime[i])
        exist <- rbind(exist, to_append)
        index <- nrow(exist)
      }
      current <- data$contributor[i]
    }
    if (i == nrow(data)) {
      exist$end_dt[index] <- data$datetime[i]
    }
  }  # End of for loop
  
  # if there untouched rows in exist check if they interfere with the last end_dt. If they do, they either need adjustments to their start_dt or need to be removed.
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
    for (i in 1:nrow(exist)) {
      if (!is.na(exist$contributor_id[i])) {  # Means that we need to update rows
        if (exist$timeseries_id[i] == -1) { # These need to be removed from the DB based on their contributor_id
          DBI::dbExecute(con, paste0("DELETE FROM contributors WHERE contributor_id = ", exist$contributor_id[i], ";"))
          next
        }
        DBI::dbExecute(con, paste0("UPDATE contributors SET owner_contributor_id = ", exist$owner_contributor_id[i], ", start_dt = '", exist$start_dt[i], "', end_dt = '", exist$end_dt[i], "' WHERE contributor_id = ", exist$contributor_id[i], ";"))
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
