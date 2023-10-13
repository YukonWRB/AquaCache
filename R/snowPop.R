#' Initial population of PostgreSQL snow database.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Populates the snowDB PostgreSQL database. Pulls existing data from Access database. Currently only populates a couple of tables and is a work in progess.
#'
#' @param old_snow_db_path the path to where the old Access snow database exists
#'
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [snowConnect()].
#'
#' @return A populated snowDB database.
#' @export
#'
#'
snowPop <- function(old_snow_db_path = "//carver/infosys/Snow/DB/SnowDB.mdb", con = snowConnect_pg()) {

  #### Pull locations data from Access and add to db
  # Create connection
  snowCon <- WRBtools::snowConnect(path = old_snow_db_path, silent = TRUE)
  on.exit(DBI::dbDisconnect(snowCon), add=TRUE)
  #Get tables
  locations <- DBI::dbReadTable(snowCon, "SNOW_COURSE")
  meas <- DBI::dbGetQuery(snowCon, paste0("SELECT * FROM SNOW_SAMPLE WHERE SNOW_COURSE_ID IN ('", paste(locations$SNOW_COURSE_ID, collapse = "', '"), "')"))
  basins <- DBI::dbReadTable(snowCon, "SNOW_BASIN")
  agency <- DBI::dbReadTable(snowCon, "AGENCY")
  DBI::dbDisconnect(snowCon)

#### ---------------------- Basins and sub-basins ------------------------- ####
  # Create tables
  sub_basins <- data.frame(sub_basin = c("Upper Yukon", "Teslin", "Central Yukon", "Pelly", "Stewart", "White", "Lower Yukon", "Porcupine", "Peel", "Liard", "Alsek", "Alaska", NA), polygon = NA)
  basins$basins <- c("Alsek", "Yukon", "Porcupine", "Liard", "Peel", "Alaska")

  DBI::dbExecute(con, "DELETE FROM sub_basins")
  DBI::dbExecute(con, "DELETE FROM basins")

  # Add data to database
  for (i in 1:nrow(basins)) {
    DBI::dbExecute(con, paste0("INSERT INTO basins (basin) VALUES ('", basins$basin[i], "')"))
  }

  for (i in 1:nrow(sub_basins)) {
    DBI::dbExecute(con, paste0("INSERT INTO sub_basins (sub_basin) VALUES ('", sub_basins$sub_basin[i], "')"))
  }
  DBI::dbDisconnect(con)



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
  locations <- locations[, c("SNOW_COURSE_ID", "SNOW_COURSE_NAME", "latitude", "longitude", "ACTIVE_FLG", "ELEVATION", "AGENCY_NAME", "basins")]
  # Add notes
  locations$notes <- NA
  # Add sub_basins
  locations$sub_basin <- NA

  # Update column names
  colnames(locations) <- c("location", "name", "latitude", "longitude", "active", "elevation", "agency", "basin", "notes", 'sub_basin')

  ## Add to db
  DBI::dbExecute(con, "INSERT INTO locations "
  )

  for (i in 1:nrow(locations)) {
    DBI::dbExecute(con, paste0("INSERT INTO locations (location, name, agency, basin, sub_basin, active, elevation, latitude, longitude, notes) VALUES ('", locations$location[i], "', '", locations$name[i], "', '", locations$agency[i], "', '", locations$basin[i], "', '", locations$sub_basin[i], "', '", locations$active[i], "', '", locations$elevation[i], "', '", locations$latitude[i], "', '", locations$longitude[i], "', '", locations$notes[i], "')"))
  }
  DBI::dbDisconnect(con)

}
#### ------------------------ Other to be added --------------------------- ####
#   ##### Measurements pre-processing ####
#   #Manipulate/preprocess things a bit
#   meas <- meas[which(meas$EXCLUDE_FLG==0),] # OMIT VALUES OF EXCLUDEFLG=1, aka TRUE
#   meas$SAMPLE_DATE <- as.Date(meas$SAMPLE_DATE)
#   meas$year <- lubridate::year(meas$SAMPLE_DATE)
#   meas$month <- lubridate::month(meas$SAMPLE_DATE)
#
#   ##### Dealing with special cases #####
#   #Deal with special cases - correction factors
#   # Special case (i) Twin Creeks: 09BA-SC02B will take precedence over A from 2016 onwards.
#   subset <- meas[meas$SNOW_COURSE_ID %in% c("09BA-SC02A", "09BA-SC02B"),]
#   duplicated <- data.frame(table(subset$SAMPLE_DATE))
#   duplicated <- duplicated[duplicated$Freq > 1 , ]
#   duplicated <- as.Date(as.vector(duplicated$Var1))
#   swe_factor <- NULL
#   for (i in 1:length(duplicated)){ # Calculate correction factors:
#     a <- subset[subset$SNOW_COURSE_ID == "09BA-SC02A" & subset$SAMPLE_DATE == duplicated[i], "SNOW_WATER_EQUIV"]
#     b <- subset[subset$SNOW_COURSE_ID == "09BA-SC02B" & subset$SAMPLE_DATE == duplicated[i], "SNOW_WATER_EQUIV"]
#     swe_factor[i] <- 1 + (b-a)/a
#   }
#   depth_factor <- NULL
#   for (i in 1:length(duplicated)){
#     a <- subset[subset$SNOW_COURSE_ID == "09BA-SC02A" & subset$SAMPLE_DATE == duplicated[i], "DEPTH"]
#     b <- subset[subset$SNOW_COURSE_ID == "09BA-SC02B" & subset$SAMPLE_DATE == duplicated[i], "DEPTH"]
#     depth_factor[i] <- 1 + (b-a)/a
#   }
#   swe_correction <- mean(swe_factor)
#   depth_correction <- mean(depth_factor)
#   meas <- meas[!(meas$SNOW_COURSE_ID=="09BA-SC02A" & meas$year >= 2016),] # Remove 09BA-SC02A values in 2016 and later.
#   meas$SNOW_WATER_EQUIV[meas$SNOW_COURSE_ID=="09BA-SC02A"] <- swe_correction*(meas$SNOW_WATER_EQUIV[meas$SNOW_COURSE_ID=="09BA-SC02A"]) # Multiply all 09BA-SC02A values by correction factors
#   meas$DEPTH[meas$SNOW_COURSE_ID=="09BA-SC02A"] <- depth_correction*(meas$DEPTH[meas$SNOW_COURSE_ID=="09BA-SC02A"])
#   meas$SNOW_COURSE_ID[meas$SNOW_COURSE_ID=="09BA-SC02A"] <- "09BA-SC02B" #Rename as 09BA-SC02B (A will no longer exist here)
#   locations <- locations[!(locations$SNOW_COURSE_ID == "09BA-SC02A") , ]
#
#   # Special case (ii) Hyland 10AD-SC01 and 10AD-SC01B. B will take precedence over (no letter) from 2018 onwards.
#   subset <- meas[meas$SNOW_COURSE_ID %in% c("10AD-SC01", "10AD-SC01B"),]
#   duplicated <- data.frame(table(subset$SAMPLE_DATE))
#   duplicated <- duplicated[duplicated$Freq > 1 , ]
#   duplicated <- as.Date(as.vector(duplicated$Var1))
#   swe_factor <- NULL
#   for (i in 1:length(duplicated)){ # Calculate correction factors
#     a <- subset[subset$SNOW_COURSE_ID == "10AD-SC01" & subset$SAMPLE_DATE == duplicated[i], "SNOW_WATER_EQUIV"]
#     b <- subset[subset$SNOW_COURSE_ID == "10AD-SC01B" & subset$SAMPLE_DATE == duplicated[i], "SNOW_WATER_EQUIV"]
#     swe_factor[i] <- 1 + (b-a)/a
#   }
#   depth_factor <- NULL
#   for (i in 1:length(duplicated)){
#     a <- subset[subset$SNOW_COURSE_ID == "10AD-SC01" & subset$SAMPLE_DATE == duplicated[i], "DEPTH"]
#     b <- subset[subset$SNOW_COURSE_ID == "10AD-SC01B" & subset$SAMPLE_DATE == duplicated[i], "DEPTH"]
#     depth_factor[i] <- 1 + (b-a)/a
#   }
#   swe_correction <- mean(swe_factor)
#   depth_correction <- mean(depth_factor)
#   meas <- meas[!(meas$SNOW_COURSE_ID=="10AD-SC01" & meas$year>=2018),] #Remove SC01 blank values in 2018 and later.
#   meas$SNOW_WATER_EQUIV[meas$SNOW_COURSE_ID=="10AD-SC01"] <- swe_correction*(meas$SNOW_WATER_EQUIV[meas$SNOW_COURSE_ID=="10AD-SC01"]) #Multiply all remaining SC01 values by correction factors
#   meas$DEPTH[meas$SNOW_COURSE_ID=="10AD-SC01"] <- depth_correction*(meas$DEPTH[meas$SNOW_COURSE_ID=="10AD-SC01"])
#   meas$SNOW_COURSE_ID[meas$SNOW_COURSE_ID=="10AD-SC01"] <- "10AD-SC01B" #Step 3: Rename as 010AD-SC01B (blank will no longer exist)
#   locations <- locations[!(locations$SNOW_COURSE_ID == "10AD-SC01") , ]
#
#   if (!inactive){ #Filter out the inactive stations if inactive is FALSE
#     remove <- locations[locations$ACTIVE_FLG==TRUE,]$SNOW_COURSE_ID
#     meas <- meas[meas$SNOW_COURSE_ID %in% remove , ]
#     locations <- locations[locations$ACTIVE_FLG == TRUE ,]
#   }
#
# }
