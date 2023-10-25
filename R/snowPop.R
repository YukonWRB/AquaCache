#' Initial population of PostgreSQL snow database.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Populates the snowDB PostgreSQL database. Pulls existing data from Access database. Currently only populates a couple of tables and is a work in progess.
#'
#' @param old_snow_db_path the path to where the old Access snow database exists
#'
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [snowConnect_pg()].
#'
#' @param overwrite If TRUE, content of tables will be deleted before re-populating. All data in db will be lost!
#'
#' @return A populated snowDB database.
#' @export
#'

#TODO: Add polygon shapes into basins

#snowPop(con = snowConnect_pg(), overwrite = TRUE)

snowPop <- function(old_snow_db_path = "//carver/infosys/Snow/DB/SnowDB.mdb", con = snowConnect_pg(), overwrite = TRUE, basins_shp_path = "G:/water/Hydrology/11_SnowMet_Network/02_Manual_Surveys/04_Miscellaneous/Basins_shapefiles/swe_basins.shp")
                      #"H:/estewart/SnowBulletin/Maps/swe_basins.shp")
  {

  #### Pull data from Access and add to db
  # Create connection
  snowCon <- WRBtools::snowConnect(path = old_snow_db_path, silent = TRUE)
  on.exit(DBI::dbDisconnect(snowCon), add=TRUE)
  #Get tables
  locations <- DBI::dbReadTable(snowCon, "SNOW_COURSE")
  meas <- DBI::dbGetQuery(snowCon, paste0("SELECT * FROM SNOW_SAMPLE WHERE SNOW_COURSE_ID IN ('", paste(locations$SNOW_COURSE_ID, collapse = "', '"), "')"))
  basins <- DBI::dbReadTable(snowCon, "SNOW_BASIN")
  agency <- DBI::dbReadTable(snowCon, "AGENCY")
  DBI::dbDisconnect(snowCon)

  if (overwrite == TRUE) {
    DBI::dbExecute(con, "DELETE FROM measurements")
    DBI::dbExecute(con, "DELETE FROM survey")
    DBI::dbExecute(con, "DELETE FROM locations")
    DBI::dbExecute(con, "DELETE FROM sub_basins")
    DBI::dbExecute(con, "DELETE FROM basins")
  }

#### ---------------------- Basins and sub-basins ------------------------- ####
  ### Basins
  # Create tables
  basins$basins <- c("Alsek", "Yukon", "Porcupine", "Liard", "Peel", "Alaska")
  # Add data to database
  for (i in 1:nrow(basins)) {
    DBI::dbExecute(con, paste0("INSERT INTO basins (basin) VALUES ('", basins$basin[i], "')"))
  }

  ### Sub_basins
      #sub_basins <- sf::st_read(data("swe_basins"))
      sub_basins <- sf::st_read(basins_shp_path)
      # Subset to cols of interest
      sub_basins <- sub_basins[,c(1,4)]
      # Add Alaska and NA
      sub_basins[nrow(sub_basins) + 1,] <- list("Alaska", NA)
      sub_basins[nrow(sub_basins) + 1,] <- list("Other", NA)
      # Change column names to match snowdb ones
      sub_basins <- sub_basins %>% dplyr::rename(sub_basin = "SWE_Basin", polygon = "geometry")
      # Re-project
      sub_basins <- sf::st_transform(sub_basins, 4269)
      # Add to db
      rpostgis::pgWriteGeom(con, name = c('public', 'sub_basins') , data.obj = sub_basins, geom = "polygon")

  # ### Old
  #     # Create tables
  #     sub_basins <- data.frame(sub_basin = c("Upper Yukon", "Teslin", "Central Yukon", "Pelly", "Stewart", "White", "Lower Yukon", "Porcupine", "Peel", "Liard", "Alsek", "Alaska", NA), polygon = NA)
  #     # Add to db
  #     for (i in 1:nrow(sub_basins)) {
  #       DBI::dbExecute(con, paste0("INSERT INTO sub_basins (sub_basin) VALUES ('", sub_basins$sub_basin[i], "')"))
  #     }


#### --------------------------- Locations -------------------------------- ####
  # Fix lat and long
  locations$LATITUDE_SEC[is.na(locations$LATITUDE_SEC)] <- as.numeric(0)
  latitude <- locations$LATITUDE_DEG + locations$LATITUDE_MIN/60 + locations$LATITUDE_SEC/3600
  locations$LONGITUDE_SEC[is.na(locations$LONGITUDE_SEC)] <- as.numeric(0)
  longitude <- -1 * abs(locations$LONGITUDE_DEG + locations$LONGITUDE_MIN/60 + locations$LONGITUDE_SEC/3600)
  # Subset to fields of interest
  locations <- locations[ , c("SNOW_COURSE_ID", "SNOW_COURSE_NAME", "ACTIVE_FLG", "ELEVATION", "AGENCY_ID", "BASIN_ID")]
  locations <- cbind(locations, latitude, longitude)
  # Change basin_id to basin name
  locations <- merge(locations, basins, by = "BASIN_ID")
  # Change agency_id to agency
  locations <- merge(locations, agency, by = "AGENCY_ID")
  # Subset table
  locations <- locations[, c("SNOW_COURSE_ID", "SNOW_COURSE_NAME", "latitude", "longitude", "ACTIVE_FLG", "ELEVATION", "AGENCY_NAME", "basins")]
  # Add notes
  locations$notes <- NA
  # Add sub_basins
  locations$sub_basin <- NA
  # Update column names
  colnames(locations) <- c("location", "name", "latitude", "longitude", "active", "elevation", "agency", "basin", "notes", 'sub_basin')
  # Deal with any apostrophes in strings
  locations$name <- gsub("'", "''", locations$name)
  # Add sub-basins
  name_to_sub_basin <- c('Aishihik Lake'='Alsek', 'Alder Creek'='Alsek', 'Arrowhead Lake'='Stewart', 'Atlin (B.C)'='Upper_Yukon', 'Beaver Creek'='White', 'Blackstone River'='Peel', 'Bonnet Plume Lake'='Peel', 'Boundary (Alaska)'='Lower_Yukon', 'Burns Lake'='Pelly', 'Burwash Airstrip'='White', 'Burwash Uplands'='White', 'Calumet'='Stewart', 'Canyon Lake'='Alsek', 'Casino Creek'='White', 'Chadburn Lake'='Upper_Yukon', 'Chair Mountain'='White', 'Clay Creek'='Alsek', 'Clearwater Creek'='Pelly', 'Clinton Creek'='Lower_Yukon', 'Duke River'='White', 'Duke River A'='White', 'Eagle Plains'='Porcupine', 'Eagle River'='Porcupine', 'Eaglecrest'='Alaska', 'Edwards Lake'='Stewart', 'Felsite Creek'='Alsek', 'Finlayson Airstrip'='Pelly', 'Ford Lake'='Liard', 'Fort Selkirk'='Pelly', 'Frances River'='Liard', 'Fuller Lake'='Pelly', 'Grizzly Creek'='Lower_Yukon', 'Haines Junction Farm'='Alsek', 'Hoole River'='Pelly', 'Hyland River'='Liard', 'Hyland River B'='Liard', 'Jordan Lake'='Teslin_Big_Salmon', 'Keno Hill'='Stewart', 'King Solomon Dome'='Lower_Yukon', 'Log Cabin (B.C.)'='Upper_Yukon', 'Long Lake'='Upper_Yukon', 'MacIntosh'='White', 'MacMillan Pass'='Pelly', 'Mayo Airport A'='Stewart', 'Mayo Airport B'='Stewart', 'McClintock'='Upper_Yukon', 'Meadow Creek'='Teslin_Big_Salmon', 'Midnight Dome'='Lower_Yukon', 'Montana Mountain'='Upper_Yukon', 'Moore Creek Bridge'='Alaska', 'Morley Lake'='Teslin_Big_Salmon', 'Mount Berdoe'='Central_Yukon', 'Mount Nansen'='White', 'Mt McIntyre A'='Upper_Yukon', 'Mt McIntyre B'='Upper_Yukon', 'Mt McIntyre C'='Upper_Yukon', 'Mt McIntyre D'='Upper_Yukon', 'Mt Peters'='Teslin_Big_Salmon', 'Northern Lake'='Teslin_Big_Salmon', 'Ogilvie River'='Peel', 'Old Crow'='Porcupine', 'Pelly Farm'='Pelly', 'Pine Lake Airstrip'='Liard', 'Plata Airstrip'='Stewart', 'Profile Mountain'='Alsek', 'Rackla Lake'='Stewart', "Riff''s Ridge"='Porcupine', 'Rose Creek'='Pelly', 'Ross River Hill'='Pelly', 'Russell Lake'='Pelly', 'Satasha Lake'='Central_Yukon', 'Stanley Creek'='Alsek', 'Stewart Crossing A'='Stewart', 'Summit'='Alsek', 'Tagish'='Upper_Yukon', 'Takhanne'='Alsek', 'Tintina Airstrip'='Liard', 'Tsichu River'='Liard', 'Tungsten'='Liard', 'Twin Creeks A'='Pelly', 'Twin Creeks B'='Pelly', 'Watson Lake Airport'='Liard', 'White River'='White', 'Whitehorse Airport'='Upper_Yukon', 'Williams Creek'='Central_Yukon', 'Withers Lake'='Stewart')

  locations$sub_basin <- name_to_sub_basin[locations$name]

  ## Add to db
  for (i in 1:nrow(locations)) {
    DBI::dbExecute(con, paste0("INSERT INTO locations (location, name, agency, basin, sub_basin, active, elevation, latitude, longitude, notes) VALUES ('", locations$location[i], "', '", locations$name[i], "', '", locations$agency[i], "', '", locations$basin[i], "', '", locations$sub_basin[i], "', '", locations$active[i], "', '", locations$elevation[i], "', '", locations$latitude[i], "', '", locations$longitude[i], "', '", locations$notes[i], "')"))
  }


#### ----------------------------- Survey --------------------------------- ####
  # Remove rows with EXCLUDE_FLG = TRUE
  survey <- meas[meas$EXCLUDE_FLG==FALSE,]
  # Deal with NA target and survey dates
    # For those with data, keep and make sample date the target date
      # Find rows where survey_date is NA and depth is not NA
      rows_to_update <- is.na(survey$SURVEY_DATE) & !is.na(survey$DEPTH)
      # Update survey_date with target_date for selected rows
      survey$SURVEY_DATE[rows_to_update] <- survey$SAMPLE_DATE[rows_to_update]
    # For those without data, remove
      rows_to_remove <- is.na(survey$SURVEY_DATE) & is.na(survey$DEPTH)
      survey <- survey[!rows_to_remove,]

  # Remove columns and add notes
  survey <- survey[c("SNOW_COURSE_ID", "SAMPLE_DATE", "SURVEY_DATE")]
  survey$notes <- NA
  # Rename columns
  colnames(survey) <- c("location", "target_date", "survey_date", "notes")
  # Check for things
    # Check for duplicate rows
    survey[duplicated(survey) | duplicated(survey, fromLast = TRUE), ] # NONE!
    # Check for non-unique combinations of location and target_date
    test <- paste(survey$location, survey$target_date, sep = "")
    test[duplicated(test)] # NONE!
    # Check that all locations exist in locations table and vice-versa
    setdiff(unique(locations$location), unique(survey$location))
    setdiff(unique(survey$location), unique(locations$location))

  # Import into db
  for (i in 1:nrow(survey)) {
    DBI::dbExecute(con, paste0("INSERT INTO survey (location, target_date, survey_date, notes) VALUES ('", survey$location[i], "', '", survey$target_date[i], "', '", survey$survey_date[i], "', '", survey$notes[i], "')"))
  }

#### -------------------------- Measurements ------------------------------ ####
  # Pull survey table from db
    survey <- DBI::dbReadTable(con, "survey")
  # Remove columns not interested in
    measurements <- meas[,c("SNOW_COURSE_ID", "DEPTH", "SNOW_WATER_EQUIV", "SAMPLE_DATE", "ESTIMATE_FLG", "EXCLUDE_FLG")]
  # Rename columns
    colnames(measurements) <- c("location", "depth", "swe", "target_date", "estimate_flag", "exclude_flag")

  # Merge survey with measurements
    measurements <- merge(survey, measurements, by=c('location', 'target_date'))
  # Remove columns
    measurements <- measurements[, !(names(measurements) %in% c("location", "target_date"))]
  # Create sample_datetime from survey_date
    measurements$sample_datetime <- as.POSIXct(paste(measurements$survey_date, "12:00:00"), format = "%Y-%m-%d %H:%M:%S")
  # Remove sample_date
    measurements <- measurements[, !(names(measurements) %in% c("survey_date"))]
  # Add sample_name column
    measurements$sampler_name <- NA

  # Import measurements into db
    for (i in 1:nrow(measurements)) {
      DBI::dbExecute(con, paste0("INSERT INTO measurements (survey_id, sample_datetime, sampler_name, estimate_flag, exclude_flag, swe, depth, notes) VALUES ('", measurements$survey_id[i], "', '", measurements$sample_datetime[i], "', '", measurements$sampler_name[i], "', '", measurements$estimate_flag[i], "', '", measurements$exclude_flag[i], "', '", measurements$swe[i], "', '", measurements$depth[i], "', '", measurements$notes[i], "')"))
    }

 DBI::dbDisconnect(con)

}
