#' Read snow template and import into Snow database
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function exists to facilitate adding new snow survey data to the snow database. It reads the snow template workbook and imports it directly into the snow database.
#'
#' @param template The name of the template (excel workbook) containing the snow data.
#' @return Does not return any object.
#'
#' @export
#'

# template <- "C:/Users/estewart/Documents/R/Projects/Teslin_2023-04-01.xlsx"
# s <- 2

readSnowTemplate <- function(template) {
  # For each sheet (survey)
  for (s in 2:length(openxlsx::getSheetNames(template))) {

    # Pull in all the data
    survey <- openxlsx::read.xlsx(xlsxFile = template, sheet = s, rows = c(5:11), cols = c(2:4), detectDates=TRUE, colNames=FALSE)

    measurement <- openxlsx::read.xlsx(xlsxFile = template, sheet = s, rows = c(12:22), cols = c(3, 7, 10, 11), colNames=TRUE)

    notes <- openxlsx::read.xlsx(xlsxFile = template, sheet = s, rows = c(31:51), cols = c(2:10), colNames=TRUE, skipEmptyRows=FALSE, skipEmptyCols=FALSE)

    maintenance <- openxlsx::read.xlsx(xlsxFile = template, sheet = s, rows = c(27:30), cols = c(2:10), colNames=TRUE, skipEmptyRows=FALSE, skipEmptyCols=TRUE)

    ### Create surveys table (location, target_date, survey_date, notes, sampler_name)
    location <- survey[1, 2]
    target_date <- survey[2, 2]
    survey_date <- survey[3, 2]
    sampler_name <- survey[4, 2]

    ## Notes
    # Air temp
    airtemp <- notes[1, 9]
    if (!is.na(airtemp)) {
      airtemp <- paste0("Air temperature was ", airtemp)
    }
    # Weather
    weather <- unlist(c(notes[2,c(3,5,7,9)], notes[3,c(3,5,7,9)]))
    names(weather) <- unlist(c(notes[2,c(2,4,6,8)], notes[3,c(2,4,6,8)]))
    weather <- weather[!is.na(weather)]
    weather <- paste("Weather was", paste(tolower(names(weather)), collapse = " and "))
    # Snow conditions
    snow <- unlist(c(notes[c(5,6,7,8), c(9)],
                     notes[c(9,10), c(5)],
                     notes[c(9,10), c(9)]))
    names(snow) <- unlist(c(notes[c(5,6,7,8), c(2)],
                            notes[c(9,10), c(2)],
                            notes[c(9,10), c(6)]))
    snow <- snow[!is.na(snow)]
    snow <- paste0(names(snow), collapse=". ")
    snow_cm <- notes[16, 9]
    if (!is.na(snow_cm)) {
      snow_cm <- paste0(snow_cm, " cm of fresh snow on surface")
    } else {snow_cm <- ""}
    # Ground ice layer
    groundice <- notes[c(11), c(5,9)]
    names(groundice) <- unlist(notes[c(11), c(2,6)])

    if (!is.na(groundice[1]) && !is.na(groundice[2])) {
      groundice <- paste("The ground ice layer under the snow is", groundice[2], "cm thick")
    } else if (tolower(groundice[1]) == "x" && is.na(groundice[2])) {
      groundice <- "A ground ice layer is present under the snow"
    } else {
      groundice <- ""
    }

    # Sampling conditions
    sampling <- notes[c(13,14,15), c(9)]
    names(sampling) <- notes[c(13,14,15), c(2)]
    sampling <- sampling[!is.na(sampling)]
    sampling <- paste0(names(sampling), collapse=". ")

    # Remarks
    remarks <- notes[18,2]

    # Pull all notes together now
    paste0("At time of sampling: ", paste(c(airtemp, weather, snow, snow_cm, groundice, sampling, remarks), collapse = ". "))

    surveys <- data.frame()

    # Create measurements table (survey_id, sample_datetime, estimate_flag, exclude_flag, swe, depth, average, notes)
    # Create maintenance table
  }


}
