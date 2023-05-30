#' Add watershed polygons to database
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' **This function is only capable of fetching pre-made polygons for WSC locations**. Improvements are planned to allow for creation of new polygons using function [WRBtools::drainageBasins()].
#'
#' Add watershed polygons to the database, stored in a separate folder alongside the database itself. Currently works only with WSC stations, as uses existing watershed polygons created by the WSC. Each watershed gets a folder containing the polygon, the associated pour point, and the monitoring location itself. In addition, a single shapefile is created containing only the watershed polygons.
#'
#' @param locations The locations for which you want to add or update polygons. Default "WSC" will get polygons for all WSC stations in the 'locations' table, or specify a vector of location IDs (must all be present in table 'locations'. Currently only supports addition of WSC polygons.
#' @param path The path to he database. A new folder will be created at the same level as the database to hold the drainage polygon shapefiles.
#'
#' @return The database table 'polygons' is populated or re-populated with links to shapefiles of polygons, pour points, and station points.
#' @export
#'

getWatersheds <- function(locations = "WSC", path){

  if (file.exists(path)){ #Then it could be a file or a directory
    if (!dir.exists(path)){ #Then it is a file, go up one level to get the directory.
      db_path <- path
      path <- dirname(path)
    } else {
      stop("You're pointing to a folder, not to the database itself. Please specify the exact path to the database.")
    }
  } else {
    stop("The path you specified does not point to an existing file. Please specify the exact path to the database.")
  }

  #Connect to the DB
  hydro <- WRBtools::hydroConnect(path = db_path, silent = TRUE)
  on.exit(DBI::dbDisconnect(hydro))

  tables <- DBI::dbListTables(hydro)
  if (!("polygons" %in% tables)){
    warning("The table 'polygons' is being created in the database using function initial_create.")
    initial_create(path = db_path, extras = "polygons", overwrite = FALSE)
  }

  if (!dir.exists(paste0(path, "/polygons/watersheds"))){ # create folder if does not exist yet
    dir.create(paste0(path, "/polygons"))
    dir.create(paste0(path, "/polygons/watersheds"))
  }
  watersheds_folder <- paste0(path, "/polygons/watersheds")

  old_files <- list.files(tempdir(),  full.names = TRUE) #clean out the tempdir
  suppressWarnings(file.remove(old_files))

  old_timeout <- getOption('timeout') #The default file download timeout is 60 seconds, and this isn't quite enough sometimes. Increase it for success, reset after.
  on.exit(options(timeout = old_timeout))
  options(timeout = 600) #10 minutes

  if (locations[1] == "WSC"){ #Get all the WSC locations in the database
    from_table <- DBI::dbGetQuery(hydro, "SELECT DISTINCT location FROM timeseries WHERE operator = 'WSC'")
    locations <- from_table$location
  }
  drainages <- unique(substr(locations, 1, 2)) #Gets the first two digits so as to DL WSC polygon zipped files

  for (i in drainages){ #Download the zipped files and extract them to a temp directory.
    if (sub("0", "", i) %in% c(1:11)){
      suppressWarnings(dir.create(paste0(tempdir(), "/drainages_zipped")))
      utils::download.file(paste0("https://collaboration.cmc.ec.gc.ca/cmc/hydrometrics/www/HydrometricNetworkBasinPolygons/", i, ".zip"), destfile = paste0(tempdir(), "/drainages_zipped/", i, ".zip"))
      utils::unzip(paste0(tempdir(), "/drainages_zipped/", i, ".zip"), exdir = paste0(tempdir(), "/drainages"))
    }
  }

  count <- 0
  if (file.exists(paste0(watersheds_folder, "/all_basins.shp"))){
    poly <- sf::st_read(dsn = watersheds_folder, layer = "all_basins") #if there is an existing shapefile, add to it.
  } else {
    poly <- NULL
  }
  for (i in locations){ #Get the shapefile-containing folder from the temp directory, put it in the path/polygons folder. Make corresponding entry to the "polygons" table.
    tryCatch({
      fs::dir_copy(paste0(tempdir(), "/drainages/", i), paste0(watersheds_folder, "/", i), overwrite = TRUE)
      basin <- list.files(paste0(watersheds_folder, "/", i), pattern = "*.+Drainage.+.shp$", full.names = TRUE)
      #Find existing entries for that location, check if file path valid. If not, delete entry.
      existing <- DBI::dbGetQuery(hydro, paste0("SELECT * FROM polygons WHERE location = '", i, "'"))
      if (nrow(existing > 0)){ #check if the file paths are still valid
        for (j in nrow(existing)){
          if (!(file.exists(existing$file_path[j]))){
            DBI::dbExecute(hydro, paste0("DELETE FROM polygons WHERE file_path = '", existing$file_path[j], "'"))
          }
        }
      }
        DBI::dbExecute(hydro, paste0("INSERT OR IGNORE INTO polygons (description, location, file_path) VALUES ('drainage_basin', '", i, "', '", basin, "')"))
        DBI::dbExecute(hydro, paste0("UPDATE polygons SET file_path = '", basin, "' WHERE location = '", i, "'"))

      if (is.null(poly)){
        if (count == 0){
          poly <- sf::st_zm(sf::read_sf(dsn=paste0(watersheds_folder, "/", i), layer=paste0(i, "_DrainageBasin_BassinDeDrainage"))) #st_zm is there because doing rbind on many polygons sometimes causes an error where it is looking for a z-dimension. No idea why.
        }
      } else {
        poly <- poly[!(poly$StationNum == i) , ]
        poly <- rbind(poly, sf::st_zm(sf::read_sf(dsn=paste0(watersheds_folder, "/", i), layer=paste0(i, "_DrainageBasin_BassinDeDrainage"))))
      }
      count <- count + 1
    }, error = function(e) {
      print(paste0("Could not find a watershed polygon for location ", i))
    })
  }

  #check if file path valid. If not, delete entry.
  existing <- DBI::dbGetQuery(hydro, "SELECT * FROM polygons WHERE location = 'all_locations'")
  if (nrow(existing > 0)){ #check if the file paths are still valid
    for (j in nrow(existing)){
      if (!(file.exists(existing$file_path[j]))){
        DBI::dbExecute(hydro, paste0("DELETE FROM polygons WHERE file_path = '", existing$file_path[j], "'"))
      }
    }
  }
  suppressMessages(sf::write_sf(poly, dsn = watersheds_folder, layer = "all_basins", driver = "ESRI Shapefile"))
  DBI::dbExecute(hydro, paste0("INSERT OR IGNORE INTO polygons (description, location, file_path) VALUES ('all_drainage_basins', 'all_locations', '", paste0(watersheds_folder, "/all_basins.shp"), "')"))
  DBI::dbExecute(hydro, paste0("UPDATE polygons SET file_path = '", paste0(watersheds_folder, "/all_basins.shp"), "' WHERE location = 'all_locations'"))

  old_files <- list.files(tempdir(),  full.names = TRUE) #clean out the tempdir
  fs::dir_delete(old_files)
  DBI::dbExecute(hydro, paste0("UPDATE internal_status SET value = '", .POSIXct(Sys.time(), "UTC"), "' WHERE event = 'last_update_watersheds'"))

}
