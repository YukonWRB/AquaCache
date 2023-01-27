#' Add watershed polygons to database
#'
#' Add watershed polygons to the database, stored in a separate folder alongside the database itself. Currently works only with WSC stations, as uses existing watershed polygons created by the WSC. Each watershed gets a folder containing the polygon, the associated pour point, and the monitoring location itself. In addition, a single shapefile is created containing only the watershed polygons.
#'
#' @param locations The locations for which you want to add or update polygons. Default "WSC" will get polygons for all WSC stations in the 'locations' table, or specify a vector of location IDs (must all be present in table 'locations'. Currently only supports addition of WSC polygons.
#' @param path The path to he database. A new folder will be created at the same level as the database to hold the drainage polygon shapefiles.
#'
#' @return The database table 'watersheds' is populated or re-populated with links to shapefiles of polygons, pour points, and station points.
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
  hydro <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(hydro))

  tables <- DBI::dbListTables(hydro)
  if (!("watersheds" %in% tables)){
    warning("The table 'watersheds' is being created in the database using function initial_create.")
    initial_create(path = db_path, extras = "watersheds", overwrite = FALSE)
  }

  if (locations == "WSC"){ #Get all the WSC locations in the database
    from_table <- DBI::dbGetQuery(hydro, "SELECT location FROM locations WHERE operator = 'WSC'")
    locations <- unlist(as.vector(unique(from_table)))
  }
  drainages <- unique(substr(locations, 1, 2)) #Gets the first two digits so as to DL WSC polygon zipped files

  if (!dir.exists(paste0(path, "/watersheds"))){ # create folder if does not exist yet
    dir.create(paste0(path, "/watersheds"))
  }
  watersheds_folder <- paste0(path, "/watersheds")

  old_files <- list.files(tempdir(),  full.names = TRUE) #clean out the tempdir
  suppressWarnings(file.remove(old_files))

  old_timeout <- getOption('timeout') #The default file download timeout is 60 seconds, and this isn't quite enough sometimes. Increase it for success, reset after.
  on.exit(options(timeout = old_timeout))
  options(timeout = 600) #10 minutes

  for (i in drainages){ #Download the zipped files and extract them to a temp directory.
    if (sub("0", "", i) %in% c(1:11)){
      suppressWarnings(dir.create(paste0(tempdir(), "/drainages_zipped")))
      utils::download.file(paste0("https://collaboration.cmc.ec.gc.ca/cmc/hydrometrics/www/HydrometricNetworkBasinPolygons/", i, ".zip"), destfile = paste0(tempdir(), "/drainages_zipped/", i, ".zip"))
      utils::unzip(paste0(tempdir(), "/drainages_zipped/", i, ".zip"), exdir = paste0(tempdir(), "/drainages"))
    }
  }

  count <- 0
  if (file.exists(paste0(watersheds_folder, "/watershed_polygons.shp"))){
    poly <- sf::st_read(dsn = watersheds_folder, layer = "watershed_polygons") #if there is an existing shapefile, add to it.
  } else {
    poly <- NULL
  }
  for (i in locations){ #Get the shapefile-containing folder from the temp directory, put it in the path/watersheds folder. Make corresponding entry to the "watersheds" table.
    tryCatch({
      fs::dir_copy(paste0(tempdir(), "/drainages/", i), paste0(watersheds_folder, "/", i), overwrite = TRUE)
      DBI::dbExecute(hydro, paste0("INSERT OR IGNORE INTO watersheds (location, folder) VALUES('", i, "', '", paste0(watersheds_folder, "/", i), "')"))
      DBI::dbExecute(hydro, paste0("UPDATE watersheds SET folder = '", paste0(watersheds_folder, "/", i), "' WHERE location = '", i, "'"))
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
  suppressMessages(sf::write_sf(poly, dsn = watersheds_folder, layer = "watershed_polygons", driver = "ESRI Shapefile"))

  old_files <- list.files(tempdir(),  full.names = TRUE) #clean out the tempdir
  fs::dir_delete(old_files)
}
