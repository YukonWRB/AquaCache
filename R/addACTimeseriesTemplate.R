#' Templates to add timeseries to AquaCacheric database
#'
#' Additional arguments to pass to the function specified in source_fx should take the form of "\{param1 = arg1\}, \{param2 = 'arg2'\}", with each parameter:argument pair enclosed in curly brackets (which aren't being rendered in the help file). The data fetch function will separate out the parameter:argument pairs based on them being within curly brackets.
#'
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [AquaCacheCon()].
#' @param format "long" or "short", only applies to the timeseries table. If long, will return one row per unique parameter with the total data.frame length equal to the parameter with the highest count. If short will return a single row, with unique parameter values concatenated in a single cell per parameter.
#' @param save_path Specify a save path (folder) if you want an xlsx document with column headers as required by function [addACTimeseries()]. "choose" lets you choose interactively.
#' 
#' @return A list of three data.frames:
#' For the timeseries table, a data.frame with column names and options already in the DB for each required parameter to pass to the parameter `timeseries_df` of [addACTimeseries()].
#' For the spatial table, a data.frame with column names and options already in the DB for each required parameter to pass to the parameter `locations_df` of [addACTimeseries()].
#' For the settings table, a data.frame with column names and options already in the DB so you can check what already exists.
#' A data.frame is also produced listing the datums present in the database, facilitating your specification of the appropriate datum in the spatial table.
#' 
#' @export
#'

addACTimeseriesTemplate <- function(con = AquaCacheCon(silent = TRUE), format = "short", save_path = NULL) {


  if (!is.null(save_path)) {
    if (save_path == "choose") {
      message("Select the output folder for shapefiles...")
      save_path <- as.character(utils::choose.dir(caption = "Select Save Folder"))
    }
    if (!dir.exists(save_path)) {
      stop("The save path you're pointint to doesn't exist, R can't access it, or your selection via the interactive menu didn't work. You could try specifying a save path directly.")
    }
    rlang::check_installed("openxlsx", reason = "necessary to output xlsx document.") #This is here because openxlsx is not a 'depends' of this package; it is only necessary for this function and is therefore in "suggests"
  }


  # Create the data.frame for timeseries ############
  ts <- DBI::dbGetQuery(con, "SELECT * FROM timeseries")
  names <- names(ts[!(names(ts) %in% c("timeseries_id", "end_datetime", "last_new_data", "last_daily_calculation", "location_id"))])

  if (format == "short") {
    ts_df <- data.frame(matrix(ncol = length(names)))
    names(ts_df) <- names
    for (i in names) {
      ts_df[1,i] <- paste(unique(ts[[i]]), collapse = ", ")
    }
    ts_df[1, "start_datetime"] <- as.POSIXct("2020-01-01 01:01:01")
  } else if (format == "long") {
    max_length <- numeric()
    for (i in names) {
      max_length <- c(max_length, length(unique(ts[[i]])))
    }
    max_length <- max(max_length)
    ts_df <- data.frame(matrix(ncol = length(names), nrow = max_length))
    names(ts_df) <- names
    for (i in names) {
      data <- unique(ts[[i]])
      if (length(data) < max_length) {
        data <- c(data, rep(NA, max_length - length(data)))
      }
      ts_df[, i] <- data
      ts_df[1,"start_datetime"] <- as.POSIXct("2020-01-01 01:01:01")
    }
  }

  #Create the data.frame for locations ###########
  locs <- data.frame(location = c("08AA007", "08AA007"),
                     name = c("Sekulmun Lake Near Whitehorse", "Sekulmun Lake Near Whitehorse"),
                     latitude = c(61.53599, 61.53599),
                     longitude = c(-137.590499, -137.590499),
                     datum_id_from = c(10, 10),
                     datum_id_to = c(35, 605),
                     conversion_m = c(913.86603, 913.51099),
                     current = c(FALSE, TRUE),
                     network = "Network name",
                     project = "Project name",
                     contact = "email@email.com",
                     note = "Optional location note here.")

  # Create the datum list table
  datums <- DBI::dbReadTable(con, "datum_list")
  #Bring datums 10, 35, 605, and 110 to the front
  rownums <- which(datums$datum_id %in% c(10,35,110,605))
  others <- seq(1:nrow(datums))[!(seq(1:nrow(datums)) %in% rownums)]
  datums <- datums[c(rownums, others),]

  # Create the settings table
  settings <- DBI::dbGetQuery(con, "SELECT * FROM settings")

  list <- list("timeseries_table" = ts_df, "locations_table" = locs, "datums" = datums, "settings" = settings)

  if (!is.null(save_path)) {
    openxlsx::write.xlsx(list, file = paste0(save_path, "/addACTemplate output ", Sys.Date(), ".xlsx"))
  }
  return(list)
}
