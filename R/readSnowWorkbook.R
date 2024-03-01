#' Read snow workbook and import into Snow database
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Reads snow workbooks created with [YGwater::createSnowTemplate()], performs some QA/QC, and imports the data to the snow database. Designed with significant error catching and logging. As the function works through the workbook it may fail on any sheet but will continue to the next sheet until all have been processed. Warning messages will be shown alerting the user explaining the issue and the workbook sheet involved. These warning messages are designed primarily for error catching when this function is run programmatically, but are nevertheless useful for manual use as well.
#'
#' @param workbook The path to the workbook (.xlsx) containing the snow data. Default "choose" lets you pick the file interactively.
#' @param overwrite If `TRUE`, will overwrite existing data in the snow database if there's already an entry for the same survey date, target date, and location (regardless of parameters).
#' @param con A connection to the snow database.
#' @return Does not return any object. The function is designed to import data into the snow database.
#'
#' @export
#'

readSnowWorkbook <- function(workbook = "choose", overwrite = FALSE, con = snowConnect(silent = TRUE)) {
  
  on.exit(DBI::dbDisconnect(con))
  
  #initial checks
  rlang::check_installed("openxlsx", reason = "necessary to read workbooks")
  
  if (workbook == "choose") {
    if (!interactive()) {
      stop("You must specify a save path when running in non-interactive mode.")
    }
    rlang::check_installed("rstudioapi", reason = "necessary for interactive file selection")
    message("Select the path to the folder where you want this report saved.")
    workbook <- rstudioapi::selectFile(caption = "Select the target workbook", path = file.path(Sys.getenv("USERPROFILE"),"Desktop"), filter = "Excel files  (*.xlsx)")
  } else {
    if (!file.exists(workbook)) {
      stop("The workbook path points to a non-existent file.")
    }
  }
  
  
  
  workbook_names <- openxlsx::getSheetNames(workbook)
  
  # For each sheet (survey)
  for (s in 2:length(workbook_names)) { #first sheet is the summary sheet
    
    ##### --------------- Pull in all the data from workbook -------------- ####
    survey <- openxlsx::read.xlsx(xlsxFile = workbook, sheet = s, rows = c(5:11), cols = c(2:4), detectDates = TRUE, colNames = FALSE)
    measurement <- openxlsx::read.xlsx(xlsxFile = workbook, sheet = s, rows = c(12:22), cols = c(3, 7, 10, 11), colNames = TRUE)
    calculated <- openxlsx::read.xlsx(xlsxFile = workbook, sheet = s, rows = c(12,23,25), cols = c(2,3,7), colNames = TRUE)
    estavg <- openxlsx::read.xlsx(xlsxFile = workbook, sheet = s, rows = c(12,23), cols = c(11,12), colNames = FALSE)
    notes <- openxlsx::read.xlsx(xlsxFile = workbook, sheet = s, rows = c(27:53), cols = c(2:10), colNames = TRUE, skipEmptyRows = FALSE, skipEmptyCols = FALSE)
    maintenance <- openxlsx::read.xlsx(xlsxFile = workbook, sheet = s, rows = c(48:51), cols = c(2:9), colNames = TRUE, skipEmptyRows = FALSE, skipEmptyCols = TRUE)
    
    # Get location id for that sheet
    next_flag <- FALSE #Will be used to skip to next sheet if there is an error
    tryCatch({
      loc_id <- DBI::dbGetQuery(con, paste0("SELECT location FROM locations WHERE name = '", survey[1,2], "'"))[1,1]
      if (is.na(loc_id)) {
        next_flag <- TRUE
      }
    }, error = function(e) {
      warning("Unable to retrieve location id from the Snow DB using the following snow course name given in workbook: ", survey[1,2], ". This location name does not exist in the database. Check for typos in the name or request that location be added.")
      warning("FAILED: new snow course data for '", survey[1,2], "' was not imported")
      next_flag <<- TRUE
    }
    )
    if (next_flag) {
      warning("Failed to retrieve a location for ", survey[1,2], ". Skipping to next sheet. If this is a new location it must be entered into the database. If not, check the location name at the top of the sheet as it must match the database entry.")
      next
      }
    
    ##### --------------------- Create surveys table ---------------------- ####
    location <- loc_id
    target_date <- survey[2, 2]
    survey_date <- survey[3, 2]
    sampler_name <- survey[4, 2]
    
    if (is.na(estavg[2,2])) {
      method <- survey[5,2]
    } else {
      method <- "average"
    }
    
    ## Notes
    # Add ninth column if missing from notes
    if (ncol(notes) == 8) {notes$X9 <- NA}
    # Air temp
    airtemp <- notes[1, 9]
    if (!is.na(airtemp)) {
      airtemp <- paste0("Air temperature was ", airtemp)
    } else {airtemp <- NULL}
    
    # Weather
    weather <- unlist(c(notes[2,c(3,5,7,9)], notes[3,c(3,5,7,9)]))
    names(weather) <- unlist(c(notes[2,c(2,4,6,8)], notes[3,c(2,4,6,8)]))
    weather <- weather[!is.na(weather)]
    weather <- paste("Weather was", paste(tolower(names(weather)), collapse = " and "))
    if (weather == "Weather was ") {weather <- NULL}
    
    # Snow conditions
    snow <- unlist(c(notes[c(5,6,7,8), c(9)],
                     notes[c(9,10), c(5)],
                     notes[c(9,10), c(9)]))
    names(snow) <- unlist(c(notes[c(5,6,7,8), c(2)],
                            notes[c(9,10), c(2)],
                            notes[c(9,10), c(6)]))
    snow <- snow[!is.na(snow)]
    snow <- paste0(names(snow), collapse = ". ")
    if (snow == "") {snow <- NULL}
    snow_cm <- notes[16, 9]
    if (!is.na(snow_cm)) {
      snow_cm <- paste0(snow_cm, " cm of fresh snow on surface")
    } else {snow_cm <- NULL}
    
    # Ice layer
    ice <- unlist(c(notes[c(5,6), 9],
                    notes[c(11), c(5,9)]
    ))
    names(ice) <- unlist(c(notes[c(5,6), 2],
                           notes[c(11), c(2,6)]))
    ice <- ice[!is.na(ice)]
    
    ice_desc <- notes[13, 2]
    ice_desc <- sub("\n", " ", ice_desc)
    
    if (length(ice) == 0 && is.na(ice_desc) ) {
      ice_notes <- NULL
    } else if (length(ice) == 0 && !is.na(ice_desc)) {
      ice_notes <- ice_desc
    } else if (length(ice) != 0 && is.na(ice_desc)) {
      ice_notes <- paste0(names(ice), collapse = ". ")
    } else {ice_notes <- paste0(paste0(names(ice), collapse = ". "), ". ", ice_desc)}
    
    # Sampling conditions
    sampling <- notes[c(17,18,19), c(9)]
    names(sampling) <- notes[c(17,18,19), c(2)]
    sampling <- sampling[!is.na(sampling)]
    sampling <- paste0(names(sampling), collapse = ". ")
    if (sampling == "") { sampling <- NULL }
    
    # Remarks
    remarks <- notes[26,2]
    if (is.na(remarks)) { remarks = NULL }
    
    # Pull all notes together now
    notes <- paste(c(airtemp, weather, snow, snow_cm, sampling, remarks), collapse = ". ")
    if (notes == "") {notes <- NA
    } else {notes <- paste0("At time of sampling: ", notes)}
    
    
    ## CHECKS
    # Remove apostrophes in text.
    notes <- gsub("'", "", notes)
    sampler_name <- gsub("'", "", sampler_name)
    
    # Check that target_date and survey_date are not empty
    if (is.na(target_date) | length(target_date) == 0) {
      warning("FAILED for ", survey[1,2], "' (", loc_id, "), snow survey target date is missing and must be given.")
      next
    }
    if (is.na(survey_date) | length(survey_date) == 0) {
      warning("FAILED for ", survey[1,2], "' (", loc_id, "), snow survey sampling date is missing for and must be given.")
      next
    }
    
    ## Combine all together
    surveys <- c(location, target_date, survey_date, notes, sampler_name, method, ice_notes)
    
    ## Insert into surveys table
    next_flag <- FALSE #Will be used to skip to next sheet if there is an error
    tryCatch({
      # See if the survey has been entered already
      exists <- DBI::dbGetQuery(con, paste0("SELECT survey_id FROM surveys WHERE location = '", location, "' ",
                                            "AND target_date = '", target_date, "' ",
                                            "AND survey_date = '", survey_date, "'") )
      if (nrow(exists) == 0) {
        if (is.null(ice_notes)) {
          DBI::dbExecute(con, paste0("INSERT INTO surveys (location, target_date, survey_date, notes, sampler_name, method) VALUES ('", paste(surveys, collapse = "', '"), "')") )
        } else {
          DBI::dbExecute(con, paste0("INSERT INTO surveys (location, target_date, survey_date, notes, sampler_name, method, ice_notes) VALUES ('", paste(surveys, collapse = "', '"), "')") )
        }
        message(paste0("New survey for snow course '", survey[1,2], "' (", loc_id, ") and target date ", target_date, " inserted into surveys table."))
      } else if (nrow(exists) == 1 && !overwrite) {
        message("Survey already exists for servey at '", survey[1,2], "' for target date ", target_date, " and survey date ", survey_date, " and overwrite is FALSE. Skipping to next sheet.")
        next_flag <- TRUE
      } else if (nrow(exists) == 1 && overwrite) {
        # Update the survey entry
        if (is.null(ice_notes)) {
          DBI::dbExecute(con, paste0("UPDATE surveys SET notes = '", notes, "', sampler_name = '", sampler_name, "', method = '", method, "' WHERE location = '", location, "' AND target_date = '", target_date, "' AND survey_date = '", survey_date, "'"))
          } else {
          DBI::dbExecute(con, paste0("UPDATE surveys SET notes = '", notes, "', sampler_name = '", sampler_name, "', method = '", method, "', ice_notes = '", ice_notes, "' WHERE location = '", location, "' AND target_date = '", target_date, "' AND survey_date = '", survey_date, "'"))
          }
        message("Surveys table for servey at '", survey[1,2], "' for target date ", target_date, " and survey date ", survey_date, " updated.")
      } else {
        warning("FAILED to create new entry for servey at '", survey[1,2], "' for target date ", target_date, " and survey date ", survey_date, ".")
        next_flag <<- TRUE
      }
    }, error = function(e) {
      warning("FAILED to create new entry for servey at '", survey[1,2], "' for target date ", target_date, " and survey date ", survey_date, ".")
      next_flag <<- TRUE
    })
    if (next_flag) {next}
    
    # Get survey id
    surv_id <- DBI::dbGetQuery(con, paste0("SELECT survey_id FROM surveys WHERE location = '", location, "' ",
                                           "AND target_date = '", target_date, "' ",
                                           "AND survey_date = '", survey_date, "'") )[1,1]
    
    
    ##### ------------------ Create measurements table -------------------- ####
    # Create measurements table (survey_id, sample_datetime, estimate_flag, exclude_flag, swe, depth, notes)
    
    ### Standard
    if (method == "standard") {
      ## Sample_datetime
      # CHECK start time and end times
      # If no end time, set it to start time
      if (is.na(survey[7,2])) {
        survey[7,2] <- survey[6,2]
      }
      # Check that end time is after stat time
      if (survey[7,2] < survey[6,2]) {
        warning("FAILED: new snow course data for '", survey[1,2], "' (", loc_id, ") end time is before start time.")
        check <- DBI::dbGetQuery(con, paste0("SELECT SWE, depth FROM measurements WHERE survey_id = ", surv_id, ";"))
        if (nrow(check) == 0) {
          DBI::dbExecute(con, paste0("DELETE FROM surveys WHERE survey_id = ", surv_id, ";"))
        }
        next
      }
      
      # Create times vector
      times <- seq.int(from = as.numeric(survey[6,2]),
                       to = as.numeric(survey[7,2]),
                       length.out = length(measurement[,1]))
      # Create sample_datetime vector
      sample_datetime <- as.POSIXct(paste0(survey_date, " 00:00:00 Etc/GMT-7")) + times * 24 * 3600
      ## Estimate_flag
      estimate_flag <- rep(FALSE, times = length(sample_datetime))
      ## Exclude_flag, swe, depth, notes, survey_id
      exclude_flag <- !is.na(measurement$Exclude.flag)
      swe <- round(measurement$SWE * 10)
      depth <- round(measurement[,1])
      notes <- measurement$`Sample.notes.(see.details)`
      survey_id <- rep(surv_id, times = length(sample_datetime))
    } else if (method == "bulk") {  ### Bulk workflow
      ## Sample_datetime
      sample_datetime <- as.POSIXct(paste0(survey_date, " 00:00:00 Etc/GMT-7")) + as.numeric(survey[6,2]) * 24*3600
      ## Estimate_flag (Can only be given to averages)
      estimate_flag <- FALSE
      ## Exclude_flag, swe, depth, notes, survey_id
      exclude_flag <- FALSE
      swe <- round(calculated[1,3])
      depth <- round(calculated[2,2])
      if (all(is.na(measurement$`Sample.notes.(see.details)`))) {
        notes <- NA
      } else {notes <- paste0("Sample ", row.names(measurement[!is.na(measurement$`Sample.notes.(see.details)`),]), ": ", measurement[!is.na(measurement$`Sample.notes.(see.details)`),3], collapse = ". ")}
      survey_id <- surv_id
      ### Average
    } else if (method == "average") {
      ## Sample_datetime
      sample_datetime <- as.POSIXct(paste0(survey_date, " 00:00:00 Etc/GMT-7")) + as.numeric(survey[6,2]) * 24*3600
      ## Estimate_flag (Can only be given to averages)
      estimate_flag <- TRUE
      ## Exclude_flag, swe, depth, notes
      exclude_flag <- FALSE
      swe <- round(calculated[2,3] * 10)
      depth <- round(calculated[2,2])
      if (all(is.na(measurement$`Sample.notes.(see.details)`))) {
        notes <- NA
      } else {notes <- paste0("Sample ", row.names(measurement[!is.na(measurement$`Sample.notes.(see.details)`),]), ": ", measurement[!is.na(measurement$`Sample.notes.(see.details)`),3], collapse = ". ")}
      survey_id <- surv_id
    }
    
    ## CHECKS
    # Remove apostrophes in text.
    notes <- gsub("'", "", notes)
    # Check for empty SWE or depth
    if (method %in% c("average", "bulk")) {
      if (is.na(swe) | length(swe) == 0) {
        warning("FAILED: SWE is missing for snow coarse '", survey[1,2], "' (", loc_id, ") and must be given.")
        check <- DBI::dbGetQuery(con, paste0("SELECT SWE, depth FROM measurements WHERE survey_id = ", surv_id, ";"))
        if (nrow(check) == 0) {
          DBI::dbExecute(con, paste0("DELETE FROM surveys WHERE survey_id = ", surv_id, ";"))
        }
        next
      }
      if (is.na(depth) | length(depth) == 0) {
        warning("FAILED: now depth is missing for snow coarse '", survey[1,2], "' (", loc_id, ") and must be given.")
        check <- DBI::dbGetQuery(con, paste0("SELECT SWE, depth FROM measurements WHERE survey_id = ", surv_id, ";"))
        if (nrow(check) == 0) {
          DBI::dbExecute(con, paste0("DELETE FROM surveys WHERE survey_id = ", surv_id, ";"))
        }
        next
      }
    } else if (method == "standard") {
      if (length(swe) < length(sample_datetime)) {
        warning("FAILED: SWE is missing for 1 or more samples of snow course '", survey[1,2], "' (", loc_id, ") and must be given.")
        check <- DBI::dbGetQuery(con, paste0("SELECT SWE, depth FROM measurements WHERE survey_id = ", surv_id, ";"))
        if (nrow(check) == 0) {
          DBI::dbExecute(con, paste0("DELETE FROM surveys WHERE survey_id = ", surv_id, ";"))
        }
        next
      }
      if (length(depth) < length(sample_datetime)) {
        warning("FAILED: snow depth is missing for 1 or more samples of snow course '", survey[1,2], "' (", loc_id, ") and must be given.")
        check <- DBI::dbGetQuery(con, paste0("SELECT SWE, depth FROM measurements WHERE survey_id = ", surv_id, ";"))
        if (nrow(check) == 0) {
          DBI::dbExecute(con, paste0("DELETE FROM surveys WHERE survey_id = ", surv_id, ";"))
        }
        next
      }
    }
    
    
    ##### ------------------- Create maintenance table -------------------- ####
    # maintenance_id, location, date, maintenance, completed
    ## date
    date <- survey_date
    
    ## Changes to maintenance table
    if (ncol(maintenance) == 1) {
      maintenance$X2 <- NA
    }
    
    # x : needs to be completed
    # completed = TRUE : has been completed
    
    # For that location, pull maintenance table
    maint_db <- DBI::dbGetQuery(con, paste0("SELECT * FROM maintenance WHERE location = '", loc_id, "' and completed = FALSE"))
    
    
    ##### ------------------ Import into snow database -------------------- ####
    
    ## Begin db transaction
    DBI::dbBegin(con)
    
    tryCatch({
      ## Insert into measurements table
      if (method == "standard") {
        survey_id <- rep(surv_id, times = length(sample_datetime))
      } else if (method %in% c("average", "bulk")) {
        survey_id <- surv_id
      }
      if (overwrite) {
        DBI::dbExecute(con, paste0("DELETE FROM measurements WHERE survey_id = '", surv_id, "'"))
      }
      meas_statement <- sprintf("INSERT INTO measurements (survey_id, sample_datetime, estimate_flag, exclude_flag, swe, depth, notes) VALUES %s;", paste(sprintf("('%s', '%s', '%s', '%s', %d, %d, '%s')", survey_id, sample_datetime, estimate_flag, exclude_flag, swe, depth, notes), collapse = ", "))
      
      DBI::dbExecute(con, meas_statement)
      
      message(paste0("Snow course '", survey[1,2], "' (", loc_id, ") inserted into measurements table."))
      
      ## Changes to maintenance table
      # For those with completed = FALSE
      for (m in maint_db$maintenance) {
        # check if maintenance x has been removed in workbook
        if (is.na(maintenance[maintenance$`Maintenance.required.(x.if.applicable)` == m,][1,2]) ) {
          # If it has been removed, change to completed = TRUE and add date_completed
          DBI::dbExecute(con, paste0("UPDATE maintenance SET completed = TRUE, date_completed = '", date,
                                     "' WHERE location = '", loc_id,
                                     "' AND maintenance = '", m, "'"))
        }
        # If it has not been removed, then do not change anything
      }
      
      # For those with x
      for (m in which(!is.na(maintenance$X2)) ) {
        # check if maintenance db table has a completed = FALSE for that maintenance. If so, don't change anything. Otherwise add it.
        maint <- maintenance[m, 1]
        if ( length(maint_db[maint_db$maintenance == maint,]$completed) == 0 ) {
          mrow <- c(loc_id, date, maint, FALSE)
          DBI::dbExecute(con, paste0("INSERT INTO maintenance (location, date, maintenance, completed) VALUES ('",
                                     paste(mrow, collapse = "', '"), "')"))
        }
      }
      
      ## Commit import
      DBI::dbCommit(con)
      message("SUCCESS: new snow course data for '", survey[1,2], "' (", loc_id, ") imported.")
    }, error = function(e) {
      # Rollback transaction if any statement fails
      DBI::dbRollback(con)
      
      # Check if there are measurements for that survey_id. If not, delete the survey_id from surveys table.
      check <- DBI::dbGetQuery(con, paste0("SELECT SWE, depth FROM measurements WHERE survey_id = ", surv_id, ";"))
      if (nrow(check) == 0) {
        DBI::dbExecute(con, paste0("DELETE FROM surveys WHERE survey_id = ", surv_id, ";"))
      }
      warning("FAILED: new snow course data for '", survey[1,2], "' (", loc_id, "). Import rolled back")
    }
    )
  }
}
