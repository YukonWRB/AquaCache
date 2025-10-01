#' Read snow workbook and import into Snow database
#'
#' @description
#'
#' Reads snow workbooks created with [YGwater::createSnowTemplate()], performs QA/QC checks, and imports the data to the snow database. Designed with significant error catching and logging. As the function works through the workbook it may fail on any single sheet but will continue to the next sheet until all have been processed. Warning messages will be shown alerting the user explaining the issue and the workbook sheet involved. These warning messages are designed primarily for error catching when this function is run programmatically, but are nevertheless useful for manual use.
#'
#' @param workbook The path to the workbook (.xlsx) containing the snow data. Default "choose" lets you pick the file interactively.
#' @param overwrite If `TRUE`, will overwrite existing data in the snow database if there's already an entry for the same survey date, target date, and location (regardless of parameters).
#' @param con A connection to the snow database. Leave NULL to use function snowConnect() with defaults and close the connection after the function is done.
#' @return Does not return any object. The function is designed to import data into the snow database.
#'
#' @export
#'

readSnowWorkbook <- function(
  workbook = "choose",
  overwrite = FALSE,
  con = NULL
) {
  if (is.null(con)) {
    con <- snowConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }

  #initial checks
  rlang::check_installed("openxlsx", reason = "necessary to read workbooks")

  if (workbook == "choose") {
    if (!interactive()) {
      stop("You must specify a save path when running in non-interactive mode.")
    }
    rlang::check_installed(
      "rstudioapi",
      reason = "necessary for interactive file selection"
    )
    message("Select the path to the folder where you want this report saved.")
    workbook <- rstudioapi::selectFile(
      caption = "Select the target workbook",
      path = file.path(Sys.getenv("USERPROFILE"), "Desktop"),
      filter = "Excel files  (*.xlsx)"
    )
  } else {
    if (!file.exists(workbook)) {
      stop("The workbook path points to a non-existent file.")
    }
  }

  workbook_names <- openxlsx::getSheetNames(workbook)
  workbook_names <- workbook_names[!workbook_names %in% "Summary"]

  # For each sheet (survey)
  for (s in workbook_names) {
    #first sheet is the summary sheet

    ##### --------------- Pull in all the data from workbook -------------- ####
    survey <- openxlsx::read.xlsx(
      xlsxFile = workbook,
      sheet = s,
      rows = c(5:11),
      cols = c(2:4),
      detectDates = TRUE,
      colNames = FALSE
    )

    if (survey[5, 2] == "no sample") {
      message("Sheet ", s, " is marked as 'no sample'. Skipping to next sheet.")
      next
    }
    measurement <- openxlsx::read.xlsx(
      xlsxFile = workbook,
      sheet = s,
      rows = c(12:22),
      cols = c(3, 7, 10, 11),
      colNames = TRUE
    )
    calculated <- openxlsx::read.xlsx(
      xlsxFile = workbook,
      sheet = s,
      rows = c(12, 23, 25),
      cols = c(2, 3, 7),
      colNames = TRUE
    )
    estavg <- openxlsx::read.xlsx(
      xlsxFile = workbook,
      sheet = s,
      rows = c(12, 23),
      cols = c(11, 12),
      colNames = FALSE
    )
    notes <- openxlsx::read.xlsx(
      xlsxFile = workbook,
      sheet = s,
      rows = c(27:53),
      cols = c(2:10),
      colNames = TRUE,
      skipEmptyRows = FALSE,
      skipEmptyCols = FALSE
    )
    maintenance <- openxlsx::read.xlsx(
      xlsxFile = workbook,
      sheet = s,
      rows = c(48:51),
      cols = c(2:9),
      colNames = TRUE,
      skipEmptyRows = FALSE,
      skipEmptyCols = TRUE
    )

    # Remove empty rows in measurement, or rows where only a note is present without depth AND swe values
    measurement <- measurement[
      !(is.na(measurement[, 1]) & is.na(measurement[, 2])),
    ]

    # Remarks
    remarks <- notes[26, 2]
    if (is.na(remarks)) {
      remarks <- NULL
    }

    # Check for empty sheets and no remarks
    if (
      all(is.na(survey[c(3, 4, 6, 7), 2])) &
        nrow(measurement) == 0 &
        all(is.na(calculated[c(2, 3), c(2, 3)])) &
        all(is.na(notes[, c(3, 5, 7)])) &
        ncol(maintenance) == 1 &
        is.null(remarks)
    ) {
      message("Sheet ", s, " is empty. Skipping to next sheet.")
      next
    }
    # Check if only maintenance is filled out, as this might just be recording the maintenance required at the time the worksheet was created (everything else will be blank)
    if (
      all(is.na(survey[c(3, 4, 6, 7), 2])) &
        nrow(measurement) == 0 &
        all(is.na(calculated[c(2, 3), c(2, 3)])) &
        all(is.na(notes[, c(3, 5, 7)])) &
        ncol(maintenance) == 2 &
        is.null(remarks)
    ) {
      message(
        "Sheet ",
        s,
        " only has maintenance notes and doesn't appear to have had a field visit. Skipping to next sheet."
      )
      next
    }

    # If snow depth is 0, SWE must also be 0. It might be NA.
    measurement[measurement[, 1] == 0, 2] <- 0

    # Get location id for that sheet
    next_flag <- FALSE #Will be used to skip to next sheet if there is an error
    tryCatch(
      {
        loc_id <- DBI::dbGetQuery(
          con,
          paste0(
            "SELECT location FROM locations WHERE name = '",
            survey[1, 2],
            "'"
          )
        )[1, 1]
        if (is.na(loc_id)) {
          next_flag <- TRUE
        }
      },
      error = function(e) {
        warning(
          "Unable to retrieve location id from the Snow DB using the following snow course name given in workbook: ",
          survey[1, 2],
          ". This location name does not exist in the database. Check for typos in the name or request that location be added."
        )
        warning(
          "FAILED: new snow course data for '",
          survey[1, 2],
          "' was not imported"
        )
        next_flag <<- TRUE
      }
    )
    if (next_flag) {
      warning(
        "Failed to retrieve a location for ",
        survey[1, 2],
        ". Skipping to next sheet. If this is a new location it must be entered into the database. If not, check the location name at the top of the sheet as it must match the database entry."
      )
      next
    }

    ##### --------------------- Create surveys table ---------------------- ####
    location <- loc_id
    target_date <- survey[2, 2]
    survey_date <- survey[3, 2]
    sampler_name <- survey[4, 2]

    # Determine sampling method. Using only the presence of estimated average
    # values can wrongly assign "average" when the workbook indicates a bulk
    # sample. Start with the method recorded in the workbook and only change to
    # "average" if an estimated average exists and the method isn't a bulk
    # sample.
    method <- survey[5, 2]
    if (!is.na(estavg[2, 2]) && tolower(method) != "bulk sample") {
      method <- "average"
    }

    ## Notes
    # Add ninth column if missing from notes
    if (ncol(notes) == 8) {
      notes$X9 <- NA
    }
    # Air temp
    airtemp <- notes[1, 9]
    if (!is.na(airtemp)) {
      airtemp <- paste0("Air temperature was ", airtemp)
    } else {
      airtemp <- NULL
    }

    # Weather
    weather <- unlist(c(notes[2, c(3, 5, 7, 9)], notes[3, c(3, 5, 7, 9)]))
    names(weather) <- unlist(c(
      notes[2, c(2, 4, 6, 8)],
      notes[3, c(2, 4, 6, 8)]
    ))
    weather <- weather[!is.na(weather)]
    weather <- paste(
      "Weather was",
      paste(tolower(names(weather)), collapse = " and ")
    )
    if (weather == "Weather was ") {
      weather <- NULL
    }

    # Snow conditions
    snow <- unlist(c(
      notes[c(5, 6, 7, 8), c(9)],
      notes[c(9, 10), c(5)],
      notes[c(9, 10), c(9)]
    ))
    names(snow) <- unlist(c(
      notes[c(5, 6, 7, 8), c(2)],
      notes[c(9, 10), c(2)],
      notes[c(9, 10), c(6)]
    ))
    snow <- snow[!is.na(snow)]
    snow <- paste0(names(snow), collapse = ". ")
    if (snow == "") {
      snow <- NULL
    }
    snow_cm <- notes[16, 9]
    if (!is.na(snow_cm)) {
      snow_cm <- paste0(snow_cm, " cm of fresh snow on surface")
    } else {
      snow_cm <- NULL
    }

    # Ice layer
    ice <- unlist(c(notes[c(5, 6), 9], notes[c(11), c(5, 9)]))
    names(ice) <- unlist(c(notes[c(5, 6), 2], notes[c(11), c(2, 6)]))
    ice <- ice[!is.na(ice)]

    ice_desc <- notes[13, 2]
    ice_desc <- sub("\n", " ", ice_desc)

    if (length(ice) == 0 && is.na(ice_desc)) {
      ice_notes <- NULL
    } else if (length(ice) == 0 && !is.na(ice_desc)) {
      ice_notes <- ice_desc
    } else if (length(ice) != 0 && is.na(ice_desc)) {
      ice_notes <- paste0(names(ice), collapse = ". ")
    } else {
      ice_notes <- paste0(paste0(names(ice), collapse = ". "), ". ", ice_desc)
    }

    # Sampling conditions
    sampling <- notes[c(17, 18, 19), c(9)]
    names(sampling) <- notes[c(17, 18, 19), c(2)]
    sampling <- sampling[!is.na(sampling)]
    sampling <- paste0(names(sampling), collapse = ". ")
    if (sampling == "") {
      sampling <- NULL
    }

    # Pull all notes together now
    notes <- paste(
      c(airtemp, weather, snow, snow_cm, sampling, remarks),
      collapse = ". "
    )
    if (notes == "") {
      notes <- NA
    } else {
      notes <- paste0("At time of sampling: ", notes)
    }

    ## CHECKS
    # Remove apostrophes in text.
    notes <- gsub("'", "", notes)
    sampler_name <- gsub("'", "", sampler_name)

    # Check that target_date and survey_date are not empty
    if (is.na(target_date) | length(target_date) == 0) {
      warning(
        "FAILED for ",
        survey[1, 2],
        "' (",
        loc_id,
        "), snow survey target date is missing and must be given."
      )
      next
    }
    if (is.na(survey_date) | length(survey_date) == 0) {
      warning(
        "FAILED for ",
        survey[1, 2],
        "' (",
        loc_id,
        "), snow survey sampling date is missing and must be given."
      )
      next
    }

    # Change method to 'no sample' if there is nothing in the measurement table
    if (nrow(measurement) == 0 & method != "no sample") {
      method <- "no sample"
      message(
        "No measurements were found on sheet ",
        s,
        " but the method was set to something other than 'no sample'. Setting it to 'no sample'."
      )
    }
    if (nrow(measurement) > 0 & method == "no sample") {
      stop(
        "The measurement method for sheet ",
        s,
        " is set to 'no sample' but there *are* measurement values reported. Please fix this and try again."
      )
    }

    ## Combine all together
    surveys <- c(
      location,
      target_date,
      survey_date,
      notes,
      sampler_name,
      method,
      ice_notes
    )

    ## Insert into surveys table
    next_flag <- FALSE #Will be used to skip to next sheet if there is an error
    tryCatch(
      {
        # See if the survey has been entered already
        exists <- DBI::dbGetQuery(
          con,
          paste0(
            "SELECT survey_id FROM surveys WHERE location = '",
            location,
            "' ",
            "AND target_date = '",
            target_date,
            "' ",
            "AND survey_date = '",
            survey_date,
            "'"
          )
        )
        if (nrow(exists) == 0) {
          if (is.null(ice_notes)) {
            DBI::dbExecute(
              con,
              paste0(
                "INSERT INTO surveys (location, target_date, survey_date, notes, sampler_name, method) VALUES ('",
                paste(surveys, collapse = "', '"),
                "')"
              )
            )
          } else {
            DBI::dbExecute(
              con,
              paste0(
                "INSERT INTO surveys (location, target_date, survey_date, notes, sampler_name, method, ice_notes) VALUES ('",
                paste(surveys, collapse = "', '"),
                "')"
              )
            )
          }
          message(paste0(
            "New survey for snow course '",
            survey[1, 2],
            "' (",
            loc_id,
            ") and target date ",
            target_date,
            " inserted into surveys table."
          ))
        } else if (nrow(exists) == 1 && !overwrite) {
          message(
            "Survey already exists for survey at '",
            survey[1, 2],
            "' for target date ",
            target_date,
            " and survey date ",
            survey_date,
            " and overwrite is FALSE. Skipping to next sheet."
          )
          next_flag <- TRUE
        } else if (nrow(exists) == 1 && overwrite) {
          # Update the survey entry
          if (is.null(ice_notes)) {
            DBI::dbExecute(
              con,
              paste0(
                "UPDATE surveys SET notes = '",
                notes,
                "', sampler_name = '",
                sampler_name,
                "', method = '",
                method,
                "' WHERE location = '",
                location,
                "' AND target_date = '",
                target_date,
                "' AND survey_date = '",
                survey_date,
                "'"
              )
            )
          } else {
            DBI::dbExecute(
              con,
              paste0(
                "UPDATE surveys SET notes = '",
                notes,
                "', sampler_name = '",
                sampler_name,
                "', method = '",
                method,
                "', ice_notes = '",
                ice_notes,
                "' WHERE location = '",
                location,
                "' AND target_date = '",
                target_date,
                "' AND survey_date = '",
                survey_date,
                "'"
              )
            )
          }
          message(
            "Surveys table for survey at '",
            survey[1, 2],
            "' for target date ",
            target_date,
            " and survey date ",
            survey_date,
            " updated."
          )
        } else {
          warning(
            "FAILED to create new entry for survey at '",
            survey[1, 2],
            "' for target date ",
            target_date,
            " and survey date ",
            survey_date,
            "."
          )
          next_flag <<- TRUE
        }
      },
      error = function(e) {
        warning(
          "FAILED to create new entry for survey at '",
          survey[1, 2],
          "' for target date ",
          target_date,
          " and survey date ",
          survey_date,
          "."
        )
        next_flag <<- TRUE
      }
    )
    if (next_flag) {
      next
    }

    # Get survey id
    surv_id <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT survey_id FROM surveys WHERE location = '",
        location,
        "' ",
        "AND target_date = '",
        target_date,
        "' ",
        "AND survey_date = '",
        survey_date,
        "'"
      )
    )[1, 1]

    # CHECK start time and end times
    # If no start time, set it to noon
    if (is.na(survey[6, 2])) {
      survey[6, 2] <- "0.5"
    }
    # If no end time, set it to start time
    if (is.na(survey[7, 2])) {
      survey[7, 2] <- survey[6, 2]
    }

    # Deal with start/end datetime. There are lots of ways these can be specified: HH:mm, HHmm, HH:mm PM, etc. Also, the Excel representation might be in fractions of a day! These all need to be coerced to a fraction of a day for later work.
    survey[6, 2] <- gsub(" ", "", survey[6, 2])
    survey[6, 2] <- gsub(":", "", survey[6, 2])
    survey[7, 2] <- gsub(" ", "", survey[7, 2])
    survey[7, 2] <- gsub(":", "", survey[7, 2])

    # Remove AM/PM if exists and adjust for PM
    if (grepl("AM", survey[6, 2], ignore.case = TRUE)) {
      survey[6, 2] <- gsub("AM", "", survey[6, 2])
      survey[6, 2] <- as.numeric(survey[6, 2])
      if (survey[6, 2] > 1) {
        #Means we're dealing with HH:mm
        minutes <- substr(
          survey[6, 2],
          nchar(survey[6, 2] - 1, nchar(survey[6, 2]))
        )
        hours <- substr(survey[6, 2], 1, nchar(survey[6, 2] - 2))
        survey[6, 2] <- hours / 24 + minutes / 1440
      }
    } else if (grepl("PM", survey[6, 2], ignore.case = TRUE)) {
      survey[6, 2] <- gsub("PM", "", survey[6, 2])
      survey[6, 2] <- as.numeric(survey[6, 2])
      # Now we either have a fraction of a day (less than 1) or hour:minute notation
      if (survey[6, 2] > 1) {
        #fraction of a day
        minutes <- substr(
          survey[6, 2],
          nchar(survey[6, 2] - 1, nchar(survey[6, 2]))
        )
        hours <- substr(survey[6, 2], 1, nchar(survey[6, 2] - 2))
        survey[6, 2] <- hours / 24 + minutes / 1440
        if (survey[6, 2] < 0.5) {
          # Making sure it didn't get specified as something like 1400 PM
          survey[6, 2] <- survey[6, 2] + 0.5
        }
      }
    }
    if (!is.numeric(survey[6, 2])) {
      survey[6, 2] <- as.numeric(survey[6, 2])
    }
    if (survey[6, 2] > 1) {
      minutes <- as.numeric(substr(
        survey[6, 2],
        nchar(survey[6, 2]) - 1,
        nchar(survey[6, 2])
      ))
      hours <- as.numeric(substr(survey[6, 2], 1, nchar(survey[6, 2]) - 2))
      survey[6, 2] <- hours / 24 + minutes / 1440
    }

    if (grepl("AM", survey[7, 2], ignore.case = TRUE)) {
      survey[7, 2] <- gsub("AM", "", survey[7, 2])
      survey[7, 2] <- as.numeric(survey[7, 2])
      if (survey[7, 2] > 1) {
        #Means we're dealing with HH:mm
        minutes <- substr(
          survey[7, 2],
          nchar(survey[7, 2] - 1, nchar(survey[7, 2]))
        )
        hours <- substr(survey[7, 2], 1, nchar(survey[7, 2] - 2))
        survey[7, 2] <- hours / 24 + minutes / 1440
      }
    } else if (grepl("PM", survey[7, 2], ignore.case = TRUE)) {
      survey[7, 2] <- gsub("PM", "", survey[7, 2])
      survey[7, 2] <- as.numeric(survey[7, 2])
      # Now we either have a fraction of a day (less than 1) or hour:minute notation
      if (survey[7, 2] > 1) {
        #fraction of a day
        minutes <- substr(
          survey[7, 2],
          nchar(survey[7, 2] - 1, nchar(survey[7, 2]))
        )
        hours <- substr(survey[7, 2], 1, nchar(survey[7, 2] - 2))
        survey[7, 2] <- hours / 24 + minutes / 1440
        if (survey[7, 2] < 0.5) {
          # Making sure it didn't get specified as something like 1400 PM
          survey[7, 2] <- survey[7, 2] + 0.5
        }
      }
    }
    if (!is.numeric(survey[7, 2])) {
      survey[7, 2] <- as.numeric(survey[7, 2])
    }
    if (survey[7, 2] > 1) {
      minutes <- as.numeric(substr(
        survey[7, 2],
        nchar(survey[7, 2]) - 1,
        nchar(survey[7, 2])
      ))
      hours <- as.numeric(substr(survey[7, 2], 1, nchar(survey[7, 2]) - 2))
      survey[7, 2] <- hours / 24 + minutes / 1440
    }

    # Check that end time is after start time
    if (survey[7, 2] < survey[6, 2]) {
      check <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT SWE, depth FROM measurements WHERE survey_id = ",
          surv_id,
          ";"
        )
      )
      if (nrow(check) == 0) {
        DBI::dbExecute(
          con,
          paste0("DELETE FROM surveys WHERE survey_id = ", surv_id, ";")
        )
      }
      warning(
        "FAILED: new snow course data for '",
        survey[1, 2],
        "' (",
        loc_id,
        ") end time is before start time. Removed the entry just added to the surveys table."
      )
      next
    }

    ##### ------------------ Create measurements table -------------------- ####

    if (nrow(measurement) > 0) {
      # Create measurements table (survey_id, sample_datetime, estimate_flag, exclude_flag, swe, depth, notes
      ### Standard
      if (method == "standard") {
        # Create times vector
        times <- seq.int(
          from = survey[6, 2],
          to = survey[7, 2],
          length.out = length(measurement[, 1])
        )
        # Create sample_datetime vector
        sample_datetime <- as.POSIXct(paste0(
          survey_date,
          " 00:00:00 Etc/GMT-7"
        )) +
          times * 24 * 3600
        ## Estimate_flag
        estimate_flag <- rep(FALSE, times = length(sample_datetime))
        ## Exclude_flag, swe, depth, notes, survey_id
        exclude_flag <- !is.na(measurement$Exclude.flag)
        swe <- round(measurement$SWE * 10) # Convert to mm SWE
        depth <- round(measurement[, 1]) # Leave as cm depth
        notes <- measurement$`Sample.notes.(see.details)`
        survey_id <- rep(surv_id, times = length(sample_datetime))
      } else if (method == "bulk") {
        ### Bulk workflow
        ## Sample_datetime
        sample_datetime <- as.POSIXct(paste0(
          survey_date,
          " 00:00:00 Etc/GMT-7"
        )) +
          as.numeric(survey[6, 2]) * 24 * 3600
        ## Estimate_flag (Can only be given to averages)
        estimate_flag <- FALSE
        ## Exclude_flag, swe, depth, notes, survey_id
        exclude_flag <- FALSE
        swe <- round(calculated[2, 3] * 10) # Convert to mm SWE
        depth <- round(calculated[2, 2]) # Leave as cm depth
        if (all(is.na(measurement$`Sample.notes.(see.details)`))) {
          notes <- NA
        } else {
          notes <- paste0(
            "Sample ",
            row.names(measurement[
              !is.na(measurement$`Sample.notes.(see.details)`),
            ]),
            ": ",
            measurement[!is.na(measurement$`Sample.notes.(see.details)`), 3],
            collapse = ". "
          )
        }
        survey_id <- surv_id
        ### Average
      } else if (method == "average") {
        ## Sample_datetime
        sample_datetime <- as.POSIXct(paste0(
          survey_date,
          " 00:00:00 Etc/GMT-7"
        )) +
          as.numeric(survey[6, 2]) * 24 * 3600
        ## Estimate_flag (Can only be given to averages)
        estimate_flag <- TRUE
        ## Exclude_flag, swe, depth, notes
        exclude_flag <- FALSE
        swe <- round(calculated[2, 3] * 10)
        depth <- round(calculated[2, 2])
        if (all(is.na(measurement$`Sample.notes.(see.details)`))) {
          notes <- NA
        } else {
          notes <- paste0(
            "Sample ",
            row.names(measurement[
              !is.na(measurement$`Sample.notes.(see.details)`),
            ]),
            ": ",
            measurement[!is.na(measurement$`Sample.notes.(see.details)`), 3],
            collapse = ". "
          )
        }
        survey_id <- surv_id
      }

      ## CHECKS
      # Check for empty SWE or depth
      if (method %in% c("average", "bulk")) {
        if (is.na(swe) | length(swe) == 0) {
          warning(
            "FAILED: SWE is missing for snow course '",
            survey[1, 2],
            "' (",
            loc_id,
            ") and must be given."
          )
          check <- DBI::dbGetQuery(
            con,
            paste0(
              "SELECT SWE, depth FROM measurements WHERE survey_id = ",
              surv_id,
              ";"
            )
          )
          if (nrow(check) == 0) {
            DBI::dbExecute(
              con,
              paste0("DELETE FROM surveys WHERE survey_id = ", surv_id, ";")
            )
          }
          next
        }
        if (is.na(depth) | length(depth) == 0) {
          warning(
            "FAILED: now depth is missing for snow course '",
            survey[1, 2],
            "' (",
            loc_id,
            ") and must be given."
          )
          check <- DBI::dbGetQuery(
            con,
            paste0(
              "SELECT SWE, depth FROM measurements WHERE survey_id = ",
              surv_id,
              ";"
            )
          )
          if (nrow(check) == 0) {
            DBI::dbExecute(
              con,
              paste0("DELETE FROM surveys WHERE survey_id = ", surv_id, ";")
            )
          }
          next
        }
      } else if (method == "standard") {
        if (length(swe) < length(sample_datetime)) {
          warning(
            "FAILED: SWE is missing for 1 or more samples of snow course '",
            survey[1, 2],
            "' (",
            loc_id,
            ") and must be given."
          )
          check <- DBI::dbGetQuery(
            con,
            paste0(
              "SELECT SWE, depth FROM measurements WHERE survey_id = ",
              surv_id,
              ";"
            )
          )
          if (nrow(check) == 0) {
            DBI::dbExecute(
              con,
              paste0("DELETE FROM surveys WHERE survey_id = ", surv_id, ";")
            )
          }
          next
        }
        if (length(depth) < length(sample_datetime)) {
          warning(
            "FAILED: snow depth is missing for 1 or more samples of snow course '",
            survey[1, 2],
            "' (",
            loc_id,
            ") and must be given."
          )
          check <- DBI::dbGetQuery(
            con,
            paste0(
              "SELECT SWE, depth FROM measurements WHERE survey_id = ",
              surv_id,
              ";"
            )
          )
          if (nrow(check) == 0) {
            DBI::dbExecute(
              con,
              paste0("DELETE FROM surveys WHERE survey_id = ", surv_id, ";")
            )
          }
          next
        }
      }
    } else if (ncol(maintenance) == 2) {
      message(
        "There were no measurements for survey ",
        survey[1, 2],
        " on sheet ",
        s,
        " but maintenance notes were present. Adding these notes to the maintenance table but nothing in the measurements table."
      )
    } else {
      message(
        "There were no measurements or maintenance notes for survey ",
        survey[1, 2],
        " on sheet ",
        s,
        "."
      )
      next
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
    maint_db <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT * FROM maintenance WHERE location = '",
        loc_id,
        "' and completed = FALSE"
      )
    )

    ##### ------------------ Import into snow database -------------------- ####

    ## Begin db transaction
    active <- dbTransBegin(con)

    tryCatch(
      {
        ## Insert into measurements table
        if (method == "standard") {
          survey_id <- rep(surv_id, times = length(sample_datetime))
        } else if (method %in% c("average", "bulk", "no sample")) {
          survey_id <- surv_id
        }
        if (overwrite) {
          DBI::dbExecute(
            con,
            paste0("DELETE FROM measurements WHERE survey_id = ", surv_id, ";")
          )
          exist_meas <- data.frame()
        } else {
          exist_meas <- DBI::dbGetQuery(
            con,
            paste0(
              "SELECT * FROM measurements WHERE survey_id = ",
              surv_id,
              ";"
            )
          )
        }
        if (nrow(measurement) > 0 & nrow(exist_meas) == 0) {
          meas_statement <- sprintf(
            "INSERT INTO measurements (survey_id, sample_datetime, estimate_flag, exclude_flag, swe, depth, notes) VALUES %s;",
            paste(
              sprintf(
                "(%s, '%s', '%s', '%s', %d, %d, '%s')",
                survey_id,
                sample_datetime,
                estimate_flag,
                exclude_flag,
                swe,
                depth,
                notes
              ),
              collapse = ", "
            )
          )
          DBI::dbExecute(con, meas_statement)
          if (overwrite) {
            message(paste0(
              "Measurements for snow course '",
              survey[1, 2],
              "' (",
              loc_id,
              ") updated."
            ))
          } else {
            message(paste0(
              "Measurements for snow course '",
              survey[1, 2],
              "' (",
              loc_id,
              ") inserted into measurements table."
            ))
          }
        } else if (nrow(measurement) > 0 & nrow(exist_meas) > 0) {
          message(
            "There are measurements in the worksheet for snow course '",
            survey[1, 2],
            "' (",
            loc_id,
            ") AND in the database, but you didn't ask to overwrite. No measurements were written or updated in the database."
          )
        }

        ## Changes to maintenance table
        # For those with completed = FALSE
        for (m in maint_db$maintenance) {
          # check if maintenance x has been removed in workbook
          if (
            is.na(maintenance[
              maintenance$`Maintenance.required.(x.if.applicable)` == m,
            ][1, 2])
          ) {
            # If it has been removed, change to completed = TRUE and add date_completed
            DBI::dbExecute(
              con,
              paste0(
                "UPDATE maintenance SET completed = TRUE, date_completed = '",
                date,
                "' WHERE location = '",
                loc_id,
                "' AND maintenance = '",
                m,
                "'"
              )
            )
          }
          # If it has not been removed, then do not change anything
        }

        # For those with x
        for (m in which(!is.na(maintenance$X2))) {
          # check if maintenance db table has a completed = FALSE for that maintenance. If so, don't change anything. Otherwise add it.
          maint <- maintenance[m, 1]
          if (
            length(maint_db[maint_db$maintenance == maint, ]$completed) == 0
          ) {
            mrow <- c(loc_id, date, maint, FALSE)
            DBI::dbExecute(
              con,
              paste0(
                "INSERT INTO maintenance (location, date, maintenance, completed) VALUES ('",
                paste(mrow, collapse = "', '"),
                "')"
              )
            )
          }
        }

        ## Commit import
        if (active) {
          DBI::dbExecute(con, "COMMIT;")
        }
        message(
          "SUCCESS: new snow course data for '",
          survey[1, 2],
          "' (",
          loc_id,
          ") imported."
        )
      },
      error = function(e) {
        # Rollback transaction if any statement fails
        if (active) {
          DBI::dbExecute(con, "ROLLBACK;")
        }

        # Check if there are measurements for that survey_id. If not, delete the survey_id from surveys table.
        check <- DBI::dbGetQuery(
          con,
          paste0(
            "SELECT SWE, depth FROM measurements WHERE survey_id = ",
            surv_id,
            ";"
          )
        )
        if (nrow(check) == 0) {
          DBI::dbExecute(
            con,
            paste0("DELETE FROM surveys WHERE survey_id = ", surv_id, ";")
          )
        }
        warning(
          "FAILED to add new snow course data for '",
          survey[1, 2],
          "' (",
          loc_id,
          "). Import rolled back."
        )
      }
    )
  }
  message("Done reading in workbook at ", workbook, ".")
}
