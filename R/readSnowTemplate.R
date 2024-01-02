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

    # Create surveys table (location, target_date, survey_date, notes, sampler_name)
    location <- survey[1, 2]
    target_date <- survey[2, 2]
    survey_date <- survey[3, 2]
    sampler_name <- survey[4, 2]
    # Notes
    airtemp <- notes[1, 9]
    weather <- c(notes[2,2:9], notes[3,2:9])
    weather <- c(notes[2,3], notes[2,5], notes[2, 7], notes[2,9], notes[3,3], notes[3,5], notes[3,7], notes[3,9])
    notes <- paste0("The ")


    surveys <- data.frame()


    # Create measurements table (survey_id, sample_datetime, estimate_flag, exclude_flag, swe, depth, average, notes)
    # Create maintenance table
  }


}
