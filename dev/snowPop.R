#' Initial population of PostgreSQL snow database.
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Populates the snowDB PostgreSQL database. Pulls existing data from Access database.
#'
#' @param old_snow_db_path the path to where the old Access snow database exists
#'
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [snowConnect()].
#'
#' @param overwrite If TRUE, content of tables will be deleted before re-populating. All data in db will be lost!
#' @param basins_shp_path Path and file name of sub basins shapefile.
#'
#' @return A populated snowDB database.
#' @export
#'

#TODO: Add polygon shapes into basins, ask Tyler to create these?
#TODO: Consider eventually adding Mayo Airport C and Whitehorse Airport B. These have not been added as they are new sites that will be moved again soon.


snowPop <- function(old_snow_db_path = "//carver/infosys/Snow/DB/SnowDB.mdb", con = snowConnect(), overwrite = TRUE, basins_shp_path = "//env-fs/env-data/corp/water/Hydrology/11_SnowMet_Network/02_Manual_Surveys/04_Miscellaneous/Basins_shapefiles/swe_basins.shp")
                      #"H:/estewart/SnowBulletin/Maps/swe_basins.shp")
  {

  #Initial setup
  rlang::check_installed("sf", reason = "Package sf is required to use function snowPop")
  #### Pull data from Access and add to db
  # Create connection
  snowCon <- snowConnect_access(path = old_snow_db_path, silent = TRUE)
  on.exit(DBI::dbDisconnect(snowCon), add=TRUE)
  #Get tables
  locations <- DBI::dbReadTable(snowCon, "SNOW_COURSE")
  meas <- DBI::dbGetQuery(snowCon, paste0("SELECT * FROM SNOW_SAMPLE WHERE SNOW_COURSE_ID IN ('", paste(locations$SNOW_COURSE_ID, collapse = "', '"), "')"))
  basins <- DBI::dbReadTable(snowCon, "SNOW_BASIN")
  basins$basin <- c("Alsek", "Yukon", "Porcupine", "Liard", "Peel", "Alaska")
  agency <- DBI::dbReadTable(snowCon, "AGENCY")
  DBI::dbDisconnect(snowCon)

  if (overwrite == TRUE) {
    DBI::dbExecute(con, "DELETE FROM measurements")
    DBI::dbExecute(con, "DELETE FROM surveys")
    DBI::dbExecute(con, "DELETE FROM locations")
    DBI::dbExecute(con, "DELETE FROM sub_basins")
    DBI::dbExecute(con, "DELETE FROM basins")
  }

#### ---------------------- Basins and sub-basins ------------------------- ####
  ### Basins
  # Create tables
  basins_db <- c("Alsek", "Yukon", "Porcupine", "Liard", "Peel", "Alaska", "North_Slope")
  # Add data to database
  for (i in 1:length(basins)) {
    DBI::dbExecute(con, paste0("INSERT INTO basins (basin) VALUES ('", basins_db[i], "')"))
  }

  ### Sub_basins
      #sub_basins <- sf::st_read(data("swe_basins"))
      sub_basins <- sf::st_read(basins_shp_path)
      # Subset to cols of interest
      sub_basins <- sub_basins[,c(1,4)]
      # Add Alaska and NA
      sub_basins[nrow(sub_basins) + 1,] <- list("North_Slope", NA)
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
  locations <- locations[, c("SNOW_COURSE_ID", "SNOW_COURSE_NAME", "latitude", "longitude", "ACTIVE_FLG", "ELEVATION", "AGENCY_NAME", "basin")]
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

  # Change agencies
  name_to_agency <- c('Aishihik Lake'='Yukon Energy Corporation', 'Alder Creek'='Parks Canada', 'Arrowhead Lake'='WRB', 'Atlin (B.C)'='WRB', 'Beaver Creek'='EMR', 'Blackstone River'='EMR', 'Bonnet Plume Lake'='EMR', 'Boundary (Alaska)'='Natural Resources Conservation Service, USDA', 'Burns Lake'='EMR', 'Burwash Airstrip'='EMR', 'Burwash Uplands'='EMR', 'Calumet'='EMR', 'Canyon Lake'='Yukon Energy Corporation', 'Casino Creek'='EMR', 'Chadburn Lake'='WRB', 'Chair Mountain'='EMR', 'Clay Creek'='Parks Canada', 'Clearwater Creek'='WRB', 'Clinton Creek'='WRB', 'Duke River'='Parcs Canada', 'Duke River A'='WRB', 'Eagle Plains'='EMR', 'Eagle River'='EMR', 'Eaglecrest'='Natural Resources Conservation Service, USDA', 'Edwards Lake'='EMR', 'Felsite Creek'='WRB', 'Finlayson Airstrip'='EMR', 'Ford Lake'='EMR', 'Fort Selkirk'='WRB', 'Frances River'='EMR', 'Fuller Lake'='EMR', 'Grizzly Creek'='EMR', 'Haines Junction Farm'='EMR', 'Hoole River'='EMR', 'Hyland River'='EMR', 'Hyland River B'='EMR', 'Jordan Lake'='EMR', 'Keno Hill'='WRB', 'King Solomon Dome'='EMR', 'Log Cabin (B.C.)'='EMR', 'Long Lake'='WRB', 'MacIntosh'='EMR', 'MacMillan Pass'='WRB', 'Mayo Airport A'='EMR', 'Mayo Airport B'='EMR', 'McClintock'='WRB', 'Meadow Creek'='EMR', 'Midnight Dome'='EMR', 'Montana Mountain'='EMR', 'Moore Creek Bridge'='Natural Resources Conservation Service, USDA', 'Morley Lake'='EMR', 'Mount Berdoe'='EMR', 'Mount Nansen'='EMR', 'Mt McIntyre A'='WRB', 'Mt McIntyre B'='WRB', 'Mt McIntyre C'='WRB', 'Mt McIntyre D'='WRB', 'Mt Peters'='WRB', 'Northern Lake'='WRB', 'Ogilvie River'='EMR', 'Old Crow'='Vuntut Gwitchin First Nation', 'Pelly Farm'='Private Contract', 'Pine Lake Airstrip'='EMR', 'Plata Airstrip'='EMR', 'Profile Mountain'='Parcs Canada', 'Rackla Lake'='EMR', "Riff''s Ridge"='EMR', 'Rose Creek'='EMR', 'Ross River Hill'='WRB', 'Russell Lake'='EMR', 'Satasha Lake'='EMR', 'Stanley Creek'='WRB', 'Stewart Crossing A'='WRB', 'Summit'='EMR', 'Tagish'='EMR', 'Takhanne'='WRB', 'Tintina Airstrip'='EMR', 'Tsichu River'='WRB', 'Tungsten'='WRB', 'Twin Creeks A'='EMR', 'Twin Creeks B'='EMR', 'Watson Lake Airport'='EMR', 'White River'='WRB', 'Whitehorse Airport'='WRB', 'Williams Creek'='EMR', 'Withers Lake'='EMR')

  locations$agency <- name_to_agency[locations$name]

  locations$agency[locations$agency == 'EMR'] <- 'Yukon Energy Mines and Resources, Compliance Monitoring and Inspections Branch'
  locations$agency[locations$agency == 'WRB'] <- 'Yukon Environment, Water Resources Branch'

  # Add missing locations
  # Mayo Airport C, Whitehorse Airport A, Whitehorse Airport B (only Whitehorse Airport)
  # North Slope locations
  new_locs <- data.frame("location" = c("09MD-SC01", "09MD-SC02", "09MD-SC03", "09MD-SC04", "09MD-SC05", "09MD-SC06"),
                     "name" = c("AK Border", "Komakuk Beach", "Herschel Island", "Stokes Point", "Shingle Point", "NWT/YK Border"),
                     "latitude" = c(69.6458, 69.5908, 69.5748, 69.3177, 68.9212, 68.7312),
                     "longitude" = c(-141.0000, -140.1813, -138.8630, -138.7460, -137.2695, -136.4750),
                     "active" = c(rep(TRUE, times = 6)),
                     "elevation" = c(2, 8, 61, 17, 38, 100),
                     "agency" = rep("Parks Canada", times = 6),
                     "basin" = rep("North_Slope", times = 6),
                     "notes" = rep(NA, times = 6),
                     "sub_basin" = rep("North_Slope", times = 6))
  locations <- rbind(locations, new_locs)
  # Snow scale/pillows
    # Twin Creeks B Snow Scale, Withers Pillow, Withers Scale, Tagish Snow Scale, Tagish Snow Pillow, Hyland Snow Scale, Buckbrush Snow Scales. King Solomon Dome?
  #TODO: Ask Ghislain what would make the most sense. Maybe add something to timeseries that would give method, so that you could specify snow pillow or snow scale? Other option is to just create new sites, but this could get messy.
  # new_locs <- data.frame("location" = c("09BA-M7", "09DB-M1_pillow", "09DB-M1_scale",
  #                                       "09AA-M1_pillow", "09AA-M1_scale", "10AD-M2",
  #                                       "?"),
  #                        "name" = c("Twin Creeks North", "Withers Lake", "Withers Lake",
  #                                   "Tagish", "Tagish", "Hyland North",
  #                                   "Buckbrush"),
  #                        "latitude" = c(62.61939, 63.980283, "?",
  #                                       "?", "?", 61.58012),
  #                        "longitude" = c(-131.26337, -132.297827, "?",
  #                                        "?", "?", -128.30507),
  #                        "active" = c(rep(TRUE, times = 6)),
  #                        "elevation" = c(888, 965),
  #                        "agency" = rep("Yukon Environment", times = 6),
  #                        "basin" = c("Yukon", "Yukon"),
  #                        "notes" = rep(NA, times = 6),
  #                        "sub_basin" = c("Pelly", "Stewart"))
  # locations <- rbind(locations, new_locs)



  ## Add to db
  for (i in 1:nrow(locations)) {
    DBI::dbExecute(con, paste0("INSERT INTO locations (location, name, agency, basin, sub_basin, active, elevation, latitude, longitude, notes) VALUES ('", locations$location[i], "', '", locations$name[i], "', '", locations$agency[i], "', '", locations$basin[i], "', '", locations$sub_basin[i], "', '", locations$active[i], "', '", locations$elevation[i], "', '", locations$latitude[i], "', '", locations$longitude[i], "', '", locations$notes[i], "')"))
  }


#### ----------------------------- Surveys --------------------------------- ####
  # Remove rows with EXCLUDE_FLG = TRUE
  surveys <- meas[meas$EXCLUDE_FLG==FALSE,]
  # Deal with NA target and survey dates
    # For those with data, keep and make sample date the target date
      # Find rows where survey_date is NA and depth is not NA
      rows_to_update <- is.na(surveys$SURVEY_DATE) & !is.na(surveys$DEPTH)
      # Update survey_date with target_date for selected rows
      surveys$SURVEY_DATE[rows_to_update] <- surveys$SAMPLE_DATE[rows_to_update]
    # For those without data, remove
      rows_to_remove <- is.na(surveys$SURVEY_DATE) & is.na(surveys$DEPTH)
      surveys <- surveys[!rows_to_remove,]

  # Remove columns and add notes
  surveys <- surveys[c("SNOW_COURSE_ID", "SAMPLE_DATE", "SURVEY_DATE")]
  surveys$notes <- NA
  # Rename columns
  colnames(surveys) <- c("location", "target_date", "survey_date", "notes")
  # Check for things
    # Check for duplicate rows
    surveys[duplicated(surveys) | duplicated(surveys, fromLast = TRUE), ] # NONE!
    # Check for non-unique combinations of location and target_date
    test <- paste(surveys$location, surveys$target_date, sep = "")
    test[duplicated(test)] # NONE!
    # Check that all survey locations exist in locations table and vice-versa
    setdiff(unique(surveys$location), unique(locations$location))
    # Add sampler_name column
    surveys$sampler_name <- NA

  # Import into db
  for (i in 1:nrow(surveys)) {
    DBI::dbExecute(con, paste0("INSERT INTO surveys (location, target_date, survey_date, notes, sampler_name) VALUES ('", surveys$location[i], "', '", surveys$target_date[i], "', '", surveys$survey_date[i], "', '", surveys$notes[i], "', '", measurements$sampler_name[i], "')"))
  }

#### -------------------------- Measurements ------------------------------ ####
  # Pull survey table from db
    survey <- DBI::dbReadTable(con, "surveys")
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
  # Add average column
    measurements$average <- rep(TRUE, times=nrow(measurements))

  # Import measurements into db
    for (i in 1:nrow(measurements)) {
      DBI::dbExecute(con, paste0("INSERT INTO measurements (survey_id, sample_datetime, estimate_flag, exclude_flag, swe, depth, average, notes) VALUES ('", measurements$survey_id[i], "', '", measurements$sample_datetime[i], "', '", measurements$estimate_flag[i], "', '", measurements$exclude_flag[i], "', '", measurements$swe[i], "', '", measurements$depth[i], "', '", measurements$average[i], "', '", measurements$notes[i], "')"))
    }

 DBI::dbDisconnect(con)

}
