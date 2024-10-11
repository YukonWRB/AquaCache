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
  
  # Check if 'grade' is character, if so match those characters to 'grade_type_code' in the 'grades' table
  if (inherits(data$grade[1], "character")) {
    grade_table <- DBI::dbGetQuery(con, "SELECT grade_type_id, grade_type_code FROM grade_types;")
    data$grade <- grade_table$grade_type_id[match(data$grade, grade_table$grade_type_code)]
  }
  
  data$grade[is.na(data$grade)] <- 11
  
  # Pull the grade table entries for this timeseries_id where the datetime is within the range of the data
  exist <- DBI::dbGetQuery(con, paste0("SELECT grade_id, timeseries_id, grade_type_id, start_dt, end_dt FROM grades WHERE timeseries_id = ", timeseries_id, " AND end_dt >= '", min(data$datetime), "';"))
  
  index <- 1
  original_exist_rows <- nrow(exist)
  current <- if (nrow(exist) > 0) exist$grade_type_id[1] else data$grade[1]
  
  if (nrow(exist) == 0) {
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
        if (i > 1) {
          exist$end_dt[index] <- data$datetime[i - 1]
        }
        index <- index + 1
        if (index <= original_exist_rows) { # Modify the next row in 'exist'
          exist$start_dt[index] <- data$datetime[i]
          exist$end_dt[index] <- data$datetime[i]
          exist$grade_type_id[index] <- data$grade[i]
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
        if (nrow(exist) > 0) {
          # Modify the last row in 'exist'
          exist$end_dt[nrow(exist)] <- data$datetime[i - 1]
          to_append <- data.frame(grade_id = NA,
                                  timeseries_id = timeseries_id,
                                  grade_type_id = data$grade[i],
                                  start_dt = data$datetime[i],
                                  end_dt = data$datetime[i])
          exist <- rbind(exist, to_append)
        } else {
          exist <- data.frame(grade_id = NA,
                              timeseries_id = timeseries_id,
                              grade_type_id = data$grade[i],
                              start_dt = data$datetime[i],
                              end_dt = data$datetime[i])
        }
      }
      current <- data$grade[i]
    }
    if (i == nrow(data)) {
      exist$end_dt[nrow(exist)] <- data$datetime[i]
    }
  }  # End of for loop
  
  # Now commit the changes to the database
  DBI::dbWithTransaction(con, {
    for (i in 1:nrow(exist)) {
      if (!is.na(exist$grade_id[i])) {
        DBI::dbExecute(con, paste0("UPDATE grades SET grade_type_id = ", exist$grade_type_id[i], ", start_dt = '", exist$start_dt[i], "', end_dt = '", exist$end_dt[i], "' WHERE grade_id = ", exist$grade_id[i], ";"))
      } else {
        DBI::dbAppendTable(con, "grades", exist[i, -which(names(exist) == "grade_id")])
      }
    }
  })
  
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
  
  # Check if 'qualifier' is character, if so match those characters to 'qualifier_type_code' in the 'qualifiers' table
  if (inherits(data$qualifier[1], "character")) {
    qualifier_table <- DBI::dbGetQuery(con, "SELECT qualifier_type_id, qualifier_type_code FROM qualifier_types;")
    data$qualifier <- qualifier_table$qualifier_type_id[match(data$qualifier, qualifier_table$qualifier_type_code)]
  }
  
  data$qualifier[is.na(data$qualifier)] <- 8
  
  # Pull the qualifier table entries for this timeseries_id where the datetime is within the range of the data
  exist <- DBI::dbGetQuery(con, paste0("SELECT qualifier_id, timeseries_id, qualifier_type_id, start_dt, end_dt FROM qualifiers WHERE timeseries_id = ", timeseries_id, " AND end_dt >= '", min(data$datetime), "';"))
  
  index <- 1
  original_exist_rows <- nrow(exist)
  current <- if (nrow(exist) > 0) exist$qualifier_type_id[1] else data$qualifier[1]
  
  if (nrow(exist) == 0) {
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
        if (i > 1) {
          exist$end_dt[index] <- data$datetime[i - 1]
        }
        index <- index + 1
        if (index <= original_exist_rows) { # Modify the next row in 'exist'
          exist$start_dt[index] <- data$datetime[i]
          exist$end_dt[index] <- data$datetime[i]
          exist$qualifier_type_id[index] <- data$qualifier[i]
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
        if (nrow(exist) > 0) {
          # Modify the last row in 'exist'
          exist$end_dt[nrow(exist)] <- data$datetime[i - 1]
          to_append <- data.frame(qualifier_id = NA,
                                  timeseries_id = timeseries_id,
                                  qualifier_type_id = data$qualifier[i],
                                  start_dt = data$datetime[i],
                                  end_dt = data$datetime[i])
          exist <- rbind(exist, to_append)
        } else {
          exist <- data.frame(qualifier_id = NA,
                              timeseries_id = timeseries_id,
                              qualifier_type_id = data$qualifier[i],
                              start_dt = data$datetime[i],
                              end_dt = data$datetime[i])
        }
      }
      current <- data$qualifier[i]
    }
    if (i == nrow(data)) {
      exist$end_dt[nrow(exist)] <- data$datetime[i]
    }
  }  # End of for loop
  
  # Now commit the changes to the database
  DBI::dbWithTransaction(con, {
    for (i in 1:nrow(exist)) {
      if (!is.na(exist$qualifier_id[i])) {
        DBI::dbExecute(con, paste0("UPDATE qualifiers SET qualifier_type_id = ", exist$qualifier_type_id[i], ", start_dt = '", exist$start_dt[i], "', end_dt = '", exist$end_dt[i], "' WHERE qualifier_id = ", exist$qualifier_id[i], ";"))
      } else {
        DBI::dbAppendTable(con, "qualifiers", exist[i, -which(names(exist) == "qualifier_id")])
      }
    }
  })
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
  
  # Check if 'approval' is character, if so match those characters to 'approval_type_code' in the 'approvals' table
  if (inherits(data$approval[1], "character")) {
    approval_table <- DBI::dbGetQuery(con, "SELECT approval_type_id, approval_type_code FROM approval_types;")
    data$approval <- approval_table$approval_type_id[match(data$approval, approval_table$approval_type_code)]
  }
  
  data$approval[is.na(data$approval)] <- 6
  
  # Pull the approval table entries for this timeseries_id where the datetime is within the range of the data
  exist <- DBI::dbGetQuery(con, paste0("SELECT approval_id, timeseries_id, approval_type_id, start_dt, end_dt FROM approvals WHERE timeseries_id = ", timeseries_id, " AND end_dt >= '", min(data$datetime), "';"))
  
  index <- 1
  original_exist_rows <- nrow(exist)
  current <- if (nrow(exist) > 0) exist$approval_type_id[1] else data$approval[1]
  
  if (nrow(exist) == 0) {
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
        if (i > 1) {
          exist$end_dt[index] <- data$datetime[i - 1]
        }
        index <- index + 1
        if (index <= original_exist_rows) { # Modify the next row in 'exist'
          exist$start_dt[index] <- data$datetime[i]
          exist$end_dt[index] <- data$datetime[i]
          exist$approval_type_id[index] <- data$approval[i]
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
        if (nrow(exist) > 0) {
          # Modify the last row in 'exist'
          exist$end_dt[nrow(exist)] <- data$datetime[i - 1]
          to_append <- data.frame(approval_id = NA,
                                  timeseries_id = timeseries_id,
                                  approval_type_id = data$approval[i],
                                  start_dt = data$datetime[i],
                                  end_dt = data$datetime[i])
          exist <- rbind(exist, to_append)
        } else {
          exist <- data.frame(approval_id = NA,
                              timeseries_id = timeseries_id,
                              approval_type_id = data$approval[i],
                              start_dt = data$datetime[i],
                              end_dt = data$datetime[i])
        }
      }
      current <- data$approval[i]
    }
    if (i == nrow(data)) {
      exist$end_dt[nrow(exist)] <- data$datetime[i]
    }
  }  # End of for loop
  
  # Now commit the changes to the database
  DBI::dbWithTransaction(con, {
    for (i in 1:nrow(exist)) {
      if (!is.na(exist$approval_id[i])) {
        DBI::dbExecute(con, paste0("UPDATE approvals SET approval_type_id = ", exist$approval_type_id[i], ", start_dt = '", exist$start_dt[i], "', end_dt = '", exist$end_dt[i], "' WHERE approval_id = ", exist$approval_id[i], ";"))
      } else {
        DBI::dbAppendTable(con, "approvals", exist[i, -which(names(exist) == "approval_id")])
      }
    }
  })
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
  
  # Check if 'owner' is character, if so match those characters to 'name' in the 'owners_contributors' table
  if (inherits(data$owner[1], "character")) {
    owner_table <- DBI::dbGetQuery(con, "SELECT owner_contributor_id, name FROM owners_contributors;")
    data$owner <- owner_table$owner_contributor_id[match(data$owner, owner_table$name)]
  }
  
  data$owner[is.na(data$owner)] <- 6
  
  # Pull the owner table entries for this timeseries_id where the datetime is within the range of the data
  exist <- DBI::dbGetQuery(con, paste0("SELECT owner_id, timeseries_id, owner_type_id, start_dt, end_dt FROM owners WHERE timeseries_id = ", timeseries_id, " AND end_dt >= '", min(data$datetime), "';"))
  
  index <- 1
  original_exist_rows <- nrow(exist)
  current <- if (nrow(exist) > 0) exist$owner_type_id[1] else data$owner[1]
  
  if (nrow(exist) == 0) {
    exist <- data.frame(owner_id = NA,
                        timeseries_id = timeseries_id,
                        owner_type_id = data$owner[1],
                        start_dt = data$datetime[1],
                        end_dt = data$datetime[1])
  }
  
  # Now loop through the data to find where the owner changes
  for (i in 1:nrow(data)) {
    if (data$owner[i] != current) {
      if (index <= original_exist_rows) { # Modify rows in 'exist'
        if (i > 1) {
          exist$end_dt[index] <- data$datetime[i - 1]
        }
        index <- index + 1
        if (index <= original_exist_rows) { # Modify the next row in 'exist'
          exist$start_dt[index] <- data$datetime[i]
          exist$end_dt[index] <- data$datetime[i]
          exist$owner_type_id[index] <- data$owner[i]
        } else { # Create a new row with no owner_id
          to_append <- data.frame(owner_id = NA,
                                  timeseries_id = timeseries_id,
                                  owner_type_id = data$owner[i],
                                  start_dt = data$datetime[i],
                                  end_dt = data$datetime[i])
          exist <- rbind(exist, to_append)
          # from here on we only create new rows in 'exist'
        }
        
      } else { # Create new rows with no owner_id
        if (nrow(exist) > 0) {
          # Modify the last row in 'exist'
          exist$end_dt[nrow(exist)] <- data$datetime[i - 1]
          to_append <- data.frame(owner_id = NA,
                                  timeseries_id = timeseries_id,
                                  owner_type_id = data$owner[i],
                                  start_dt = data$datetime[i],
                                  end_dt = data$datetime[i])
          exist <- rbind(exist, to_append)
        } else {
          exist <- data.frame(owner_id = NA,
                              timeseries_id = timeseries_id,
                              owner_type_id = data$owner[i],
                              start_dt = data$datetime[i],
                              end_dt = data$datetime[i])
        }
      }
      current <- data$owner[i]
    }
    if (i == nrow(data)) {
      exist$end_dt[nrow(exist)] <- data$datetime[i]
    }
  }  # End of for loop
  
  # Now commit the changes to the database
  DBI::dbWithTransaction(con, {
    for (i in 1:nrow(exist)) {
      if (!is.na(exist$owner_id[i])) {
        DBI::dbExecute(con, paste0("UPDATE owners SET owner_type_id = ", exist$owner_type_id[i], ", start_dt = '", exist$start_dt[i], "', end_dt = '", exist$end_dt[i], "' WHERE owner_id = ", exist$owner_id[i], ";"))
      } else {
        DBI::dbAppendTable(con, "owners", exist[i, -which(names(exist) == "owner_id")])
      }
    }
  })
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
  
  # Check if 'contributor' is character, if so match those characters to 'name' in the 'owners_contributors' table
  if (inherits(data$contributor[1], "character")) {
    contributor_table <- DBI::dbGetQuery(con, "SELECT owner_contributor_id, name FROM owners_contributors;")
    data$contributor <- contributor_table$owner_contributor_id[match(data$contributor, contributor_table$name)]
  }
  
  data$contributor[is.na(data$contributor)] <- 6
  
  # Pull the contributor table entries for this timeseries_id where the datetime is within the range of the data
  exist <- DBI::dbGetQuery(con, paste0("SELECT contributor_id, timeseries_id, contributor_type_id, start_dt, end_dt FROM contributors WHERE timeseries_id = ", timeseries_id, " AND end_dt >= '", min(data$datetime), "';"))
  
  index <- 1
  original_exist_rows <- nrow(exist)
  current <- if (nrow(exist) > 0) exist$contributor_type_id[1] else data$contributor[1]
  
  if (nrow(exist) == 0) {
    exist <- data.frame(contributor_id = NA,
                        timeseries_id = timeseries_id,
                        contributor_type_id = data$contributor[1],
                        start_dt = data$datetime[1],
                        end_dt = data$datetime[1])
  }
  
  # Now loop through the data to find where the contributor changes
  for (i in 1:nrow(data)) {
    if (data$contributor[i] != current) {
      if (index <= original_exist_rows) { # Modify rows in 'exist'
        if (i > 1) {
          exist$end_dt[index] <- data$datetime[i - 1]
        }
        index <- index + 1
        if (index <= original_exist_rows) { # Modify the next row in 'exist'
          exist$start_dt[index] <- data$datetime[i]
          exist$end_dt[index] <- data$datetime[i]
          exist$contributor_type_id[index] <- data$contributor[i]
        } else { # Create a new row with no contributor_id
          to_append <- data.frame(contributor_id = NA,
                                  timeseries_id = timeseries_id,
                                  contributor_type_id = data$contributor[i],
                                  start_dt = data$datetime[i],
                                  end_dt = data$datetime[i])
          exist <- rbind(exist, to_append)
          # from here on we only create new rows in 'exist'
        }
        
      } else { # Create new rows with no contributor_id
        if (nrow(exist) > 0) {
          # Modify the last row in 'exist'
          exist$end_dt[nrow(exist)] <- data$datetime[i - 1]
          to_append <- data.frame(contributor_id = NA,
                                  timeseries_id = timeseries_id,
                                  contributor_type_id = data$contributor[i],
                                  start_dt = data$datetime[i],
                                  end_dt = data$datetime[i])
          exist <- rbind(exist, to_append)
        } else {
          exist <- data.frame(contributor_id = NA,
                              timeseries_id = timeseries_id,
                              contributor_type_id = data$contributor[i],
                              start_dt = data$datetime[i],
                              end_dt = data$datetime[i])
        }
      }
      current <- data$contributor[i]
    }
    if (i == nrow(data)) {
      exist$end_dt[nrow(exist)] <- data$datetime[i]
    }
  }  # End of for loop
  
  # Now commit the changes to the database
  DBI::dbWithTransaction(con, {
    for (i in 1:nrow(exist)) {
      if (!is.na(exist$contributor_id[i])) {
        DBI::dbExecute(con, paste0("UPDATE contributors SET contributor_type_id = ", exist$contributor_type_id[i], ", start_dt = '", exist$start_dt[i], "', end_dt = '", exist$end_dt[i], "' WHERE contributor_id = ", exist$contributor_id[i], ";"))
      } else {
        DBI::dbAppendTable(con, "contributors", exist[i, -which(names(exist) == "contributor_id")])
      }
    }
  })
} # End of adjust_contributor function
