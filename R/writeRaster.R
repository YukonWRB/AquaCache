# pgWriteRast - modified version

#' Write raster to PostGIS database table.
#'
#' @description
#' This function is not meant to be used by itself: in most cases use [add_model_raster()] or [add_raster()] which will populate reference tables so that your raster can be easily found later.
#'
#' Sends R raster to a PostGIS database table, allowing it to be fetched later into an R environment. This function is an adaptation of [rpostgis::pgWriteRast()].
#'
#' @details
#' SpatRaster band names will be stored in an array in the column
#' "band_names", which will be restored in R when imported with the function
#' \code{\link[rpostgis]{pgGetRast}}.
#'
#' If \code{blocks = NULL}, the number of block will vary by raster size, with
#' a default value of 100 copies of the data in the memory at any point in time.
#' If a specified number of blocks is desired, set blocks to a one or two-length
#' integer vector. Note that fewer, larger blocks generally results in faster
#' write times.
#'
#' @param con A connection object to a PostgreSQL database.
#' @param raster A terra \code{SpatRaster}; objects from the raster
#' package.
#' @param rast_table A character string specifying a PostgreSQL schema in the
#' database (if necessary) and table name to hold the raster (e.g.,
#' \code{name = c("schema","table")}).
#' @param bit.depth The bit depth of the raster. Will be set to 32-bit
#'     (unsigned int, signed int, or float, depending on the data)
#'     if left null, but can be specified (as character) as one of the
#'     PostGIS pixel types (see \url{http://postgis.net/docs/RT_ST_BandPixelType.html}).
#' @param blocks Optional desired number of blocks (tiles) to split the raster
#'     into in the resulting PostGIS table. This should be specified as a
#'     one or two-length (columns, rows) integer vector. See also 'Details'.
#' @param constraints Whether to create constraints from raster data. Recommended
#'     to leave \code{TRUE} unless applying constraints manually (see
#'     \url{http://postgis.net/docs/RT_AddRasterConstraints.html}).
#'     Note that constraint notices may print to the console,
#'     depending on the PostgreSQL server settings.
#'
#' @export
#' @keywords internal
#' @return A list with TRUE for successful import and the rid(s) of the appended entries.


# raster <- terra::rast("https://dd.weather.gc.ca/model_hrdpa/2.5km/06/20231217T06Z_MSC_HRDPA_APCP-Accum6h_Sfc_RLatLon0.0225_PT0H.grib2")
# raster <- raster[[1]]

writeRaster <- function(con, raster, rast_table = "rasters", bit.depth = NULL, blocks = NULL,
                        constraints = TRUE) {

  if (!suppressMessages(rpostgis::pgPostGIS(con))) {
    stop("PostGIS is not enabled on this database.")
  }

  if (!inherits(raster, "SpatRaster")){
    stop("Raster must be a terra SpatRaster object.")
  }

  # class and crs
  r_class <- DBI::dbQuoteString(con, class(raster)[1])
  r_crs <- DBI::dbQuoteString(con, terra::crs(raster))

  if (!(rast_table %in% DBI::dbListTables(con))) {
    message("Raster table does not already exist. Creating it.")
    version <- DBI::dbGetQuery(con, "SELECT version()")
    if (grepl("PostgreSQL", version$version)){
      rast.tmp.query <- paste0("CREATE TABLE ", rast_table,
                             " (rid SERIAL PRIMARY KEY,
                             reference_id INTEGER,
                             r_class TEXT,
                             r_proj4 TEXT,
                             rast RASTER NOT NULL);")
    } else if (grepl("Microsoft", version$version)) {
      rast.tmp.query <- paste0("CREATE TABLE ", rast_table,
                               " (rid INT IDENTITY(1,1) PRIMARY KEY,
                             reference_id INTEGER,
                             r_class VARCHAR(MAX),
                             r_proj4 VARCHAR(MAX),
                             rast RASTER NOT NULL);")
    } else {
      stop("This script is designed to work with either postgreSQL or SQL server databases.")
    }

    ## If the execute fails, postgis.raster extension is not installed?
    tryCatch(
      {
        DBI::dbExecute(con, rast.tmp.query)
      },
      error = function(e) {
        stop('Check if postgis.raster extension is created in the database.')
        print(e)
      }
    )
    n.base <- 0
    new <- T
  } else {
    message("Appending to existing table. Dropping any existing raster constraints...")
    try(DBI::dbExecute(con, paste0("SELECT DropRasterConstraints('", rast_table, "','rast',",
                                    paste(rep("TRUE", 12), collapse = ","),");")))
    n.base <- DBI::dbGetQuery(con, paste0("SELECT max(rid) r from ", rast_table, ";"))$r
    new = F
  }

  r1 <- raster
  res <- round(terra::res(r1), 10)

  # figure out block size
  if (!is.null(blocks)) {
    bs <- bs(r1, blocks)
    tr <- bs$tr
    cr <- bs$cr
  } else {
    tr <- terra::blocks(r1[[1]], 100)
    cr <- terra::blocks(terra::t(r1[[1]]), 100)
  }

  message("Splitting ",length(names(r1))," band(s) into ", cr$n, " x ", tr$n, " blocks...")

  # figure out bit depth
  if (is.null(bit.depth)) {
    if (is.integer(terra::values(r1))) {
      if (min(terra::values(r1), na.rm = TRUE) >= 0) {
        bit.depth <- "32BUI"
      } else {
        bit.depth <- "32BSI"
      }
    } else {
      bit.depth <- "32BF"
    }
  }
  bit.depth <- DBI::dbQuoteString(con, bit.depth)
  ndval <- -99999

  srid <- 0
  try(srid <- suppressMessages(rpostgis::pgSRID(con, sf::st_crs(terra::crs(r1)), create.srid = TRUE)),
      silent = TRUE)

  # Warning about no CRS
  if (length(srid) == 1) {
    if (srid == 0) warning("The raster has no CRS specified.")
  }

  # Grid with all band/block combinations
  crossed_df <- expand.grid(trn = 1:tr$n,
                            crn = 1:cr$n,
                            band = 1:terra::nlyr(r1))

  n <- unlist(tapply(crossed_df$band,
                     crossed_df$band,
                     function(x) seq(from = n.base + 1, by = 1, length.out = length(x))))

  rgrid <- cbind(crossed_df, n = n)


  # Function to export a block
  export_block <- function(band, trn, crn, n) {

    # Get band b
    rb <- r1[[band]]

    # Handle empty data rasters by setting ndval (-99999) to all values
    if (all(is.na(terra::values(rb)))) terra::values(rb) <- ndval

    # Get raster tile
    suppressWarnings(r <- rb[tr$row[trn]:(tr$row[trn] + tr$nrows[trn] - 1),
                             cr$row[crn]:(cr$row[crn] + cr$nrows[crn] - 1),
                             drop = FALSE])

    # Get extent and dimensions of tile
    ex <- terra::ext(r)
    d <- dim(r)

    # Only ST_MakeEmptyRaster/ST_AddBand during first band loop
    if (band == 1) {

      # Create empty raster
      tmp.query <- paste0("INSERT INTO ", rast_table, " (rid, r_class, r_proj4, rast) VALUES (",n,
                          ",",r_class,",",r_crs,", ST_MakeEmptyRaster(",
                          d[2], ",", d[1], ",", ex[1], ",", ex[4], ",",
                          res[1], ",", -res[2], ", 0, 0,", srid[1], ") );")
      DBI::dbExecute(con, tmp.query)

      # Upper left x/y for alignment snapping
      # if (trn == 1 & crn == 1) {
      tmp.query <- paste0("SELECT ST_UpperLeftX(rast) x FROM ", rast_table ," where rid = 1;")
      upx <- DBI::dbGetQuery(con, tmp.query)$x
      tmp.query <- paste0("SELECT ST_UpperLeftY(rast) y FROM ", rast_table ," where rid = 1;")
      upy <- DBI::dbGetQuery(con, tmp.query)$y
      # }

      # New band
      if (res[1] != res[2]) s2g <- paste0(", ", res[1], ", ", -res[2]) else s2g <- NULL
      bndargs <- paste0("ROW(",1:length(names(r1)),",",bit.depth,"::text,0,", ndval,")")
      tmp.query <- paste0("UPDATE ", rast_table,
                          " SET rast = ST_SnapToGrid(ST_AddBand(rast,ARRAY[",
                          paste(bndargs,collapse = ","),"]::addbandarg[]), ", upx, "," , upy , s2g, ") ",
                          "where rid = ",
                          n, ";")
      DBI::dbExecute(con, tmp.query)

    }

    mr <- terra::as.matrix(r, wide = TRUE)
    mr[is.na(mr)] <- ndval
    r2 <- paste(apply(mr, 1, FUN = function(x) {
      paste0("[", paste(x, collapse = ","), "]")
    }), collapse = ",")

    tmp.query <- paste0("UPDATE ", rast_table,
                        " SET rast = ST_SetValues(rast,",band,", 1, 1, ARRAY[",
                        r2, "]::double precision[][])
                               where rid = ",
                        n, ";")
    DBI::dbExecute(con, tmp.query)

  }

  purrr::pmap(list(rgrid$band, rgrid$trn, rgrid$crn, rgrid$n), export_block)

  # Get the rids of the rows just appended. Done as a query here in case any rows in object rgrid failed to append
  new_rids <- DBI::dbGetQuery(con, paste0("SELECT rid FROM ", rast_table, " WHERE rid > ", n.base, ";"))[, 1]

  # Create index
  if (!new) {
    tmp.query <- paste0("DROP INDEX ", gsub("\"", "", rast_table), "_rast_st_conhull_idx")
    DBI::dbExecute(con, tmp.query)
  }
  tmp.query <- paste0("CREATE INDEX ", gsub("\"", "", rast_table),
                      "_rast_st_conhull_idx ON ", rast_table,
                      " USING gist( ST_ConvexHull(rast) );")
  DBI::dbExecute(con, tmp.query)

  # 5. add raster constraints
  tmp.query <- paste0("SELECT AddRasterConstraints('public'::name,", DBI::dbQuoteString(con, rast_table),
                      "::name, 'rast'::name);")
  DBI::dbExecute(con, tmp.query)

  return(list(status = TRUE, appended_rids = new_rids))
}
