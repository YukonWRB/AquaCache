#' Write raster to PostGIS database table.
#'
#' @description
#' This function is not meant to be used by itself: in most cases use [insertACModelRaster()] or [insertACRaster()] which will populate reference tables so that your raster can be easily found later.
#'
#' Sends R raster to a PostGIS database table, allowing it to be fetched later into an R environment. This function is an adaptation of [rpostgis::pgWriteRast()]. Will create the raster table if necessary.
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
#' If the raster has no CRS specified, it will be assigned
#' \code{EPSG:4326} before being written. This avoids importing
#' tiles with an SRID of 0, which can lead to "Multiple SRIDs"
#' errors when reading the raster back.
#'
#'
#' @param con A connection object to a PostgreSQL database.
#' @param raster A terra \code{SpatRaster}.
#' @param rast_table A character string specifying a PostgreSQL schema in the database (if necessary) and table name to hold the raster (e.g., \code{c("schema","table")}).
#' @param bit.depth The bit depth of the raster. Will be set to 32-bit (unsigned int, signed int, or float, depending on the data) if left null, but can be specified (as character) as one of the PostGIS pixel types (see \url{http://postgis.net/docs/RT_ST_BandPixelType.html}).
#' @param blocks Optional desired number of blocks (tiles) to split the raster into in the resulting PostGIS table. This should be specified as a one or two-length (columns, rows) integer vector. See also 'Details'.
#' @param constraints Whether to create constraints from raster data. Recommended to leave \code{TRUE} unless applying constraints manually (see \url{http://postgis.net/docs/RT_AddRasterConstraints.html}). Note that constraint notices may print to the console, depending on the PostgreSQL server settings.
#'
#' @export
#' @keywords internal
#' @return A list with TRUE for successful import and the rid(s) of the appended entries.

writeRaster_old <- function(
    con,
    raster,
    rast_table = "rasters",
    bit.depth = NULL,
    blocks = NULL,
    constraints = TRUE
) {
  if (!suppressMessages(rpostgis::pgPostGIS(con))) {
    stop("PostGIS is not enabled on this database.")
  }
  
  if (!inherits(raster, "SpatRaster")) {
    stop("Raster must be a terra SpatRaster object.")
  }
  
  # Make sure raster is in WGS84 (EPSG:4326) before writing to the database
  # Ensure a CRS is present to avoid SRID 0 in the database
  if (is.na(terra::crs(raster)) || terra::crs(raster) == "") {
    warning("Raster has no CRS. Defaulting to EPSG:4326.")
    terra::crs(raster) <- "EPSG:4326"
  } else {
    raster <- terra::project(raster, "epsg:4326")
  }
  
  # class and crs
  r_class <- DBI::dbQuoteString(con, class(raster)[1])
  r_crs <- DBI::dbQuoteString(con, terra::crs(raster))
  
  if (!any(rast_table %in% DBI::dbListTables(con))) {
    rast_table <- if (length(rast_table) == 2) {
      paste0(rast_table[1], ".", rast_table[2])
    } else {
      rast_table
    }
    message("Raster table does not already exist. Creating it.")
    version <- DBI::dbGetQuery(con, "SELECT version()")
    if (grepl("PostgreSQL", version$version)) {
      rast.tmp.query <- paste0(
        "CREATE TABLE ",
        rast_table,
        " (rid SERIAL PRIMARY KEY,
                             reference_id INTEGER,
                             r_class TEXT,
                             r_proj4 TEXT,
                             rast RASTER NOT NULL);"
      )
    } else if (grepl("Microsoft", version$version)) {
      rast.tmp.query <- paste0(
        "CREATE TABLE ",
        rast_table,
        " (rid INT IDENTITY(1,1) PRIMARY KEY,
                             reference_id INTEGER,
                             r_class VARCHAR(MAX),
                             r_proj4 VARCHAR(MAX),
                             rast RASTER NOT NULL);"
      )
    } else {
      stop(
        "This script is designed to work with either postgreSQL or SQL server databases."
      )
    }
    
    ## If the execute fails, postgis.raster extension is not installed?
    tryCatch(
      {
        DBI::dbExecute(con, rast.tmp.query)
        DBI::dbExecute(
          con,
          paste0("COMMENT ON TABLE ", rast_table, " IS 'Holds raster tiles. Rasters may be broken up in multiple tiles, so refer to table rasters_reference to find the reference_ID for each raster. Otherwise this table is designed for extracting rasters using R, hence the r_class and r_proj4 columns.'"
          )
        )
        DBI::dbExecute(
          con,
          paste0("COMMENT ON COLUMN ", rast_table, ".reference_id IS 'Matches a unique entry in table rasters_reference. If a raster is broken up into tiles, each tile will have the same reference_id; this number is what identifies them as being tiles of the same raster.'"
          )
        )
      },
      error = function(e) {
        stop('Check if postgis.raster extension is created in the database.')
        print(e)
      }
    )
    n.base <- 0
    new <- TRUE
  } else {
    rast_table <- if (length(rast_table) == 2) {
      paste0(rast_table[1], ".", rast_table[2])
    } else {
      rast_table
    }
    message(
      "Appending to existing table. Dropping any existing raster constraints..."
    )
    if (constraints) {
      try(DBI::dbExecute(
        con,
        paste0(
          "SELECT DropRasterConstraints('",
          sub(".*\\.", "", rast_table),
          "','rast',",
          paste(rep("TRUE", 12), collapse = ","),
          ");"
        )
      ))
    }
    n.base <- DBI::dbGetQuery(
      con,
      paste0("SELECT max(rid) r from ", rast_table, ";")
    )$r
    if (is.na(n.base)) {
      n.base <- 0
      new <- TRUE
      tmp.query <- paste0(
        "DROP INDEX IF EXISTS ",
        gsub("\"", "", rast_table),
        "_rast_st_conhull_idx"
      )
      DBI::dbExecute(con, tmp.query)
    } else {
      new <- FALSE
    }
  }
  
  r1 <- raster
  res <- round(terra::res(r1), 10)
  
  # figure out block size
  if (!is.null(blocks)) {
    # Define a function basically straight from the 'bs' unexported function from rpostgis
    bs <- function(r1, blocks) {
      blocks <- as.integer(blocks)
      if (any(is.na(blocks)) || length(blocks) > 2) {
        cli::cli_abort("blocks must be a 1- or 2-length integer vector.")
      }
      if (any(blocks == 0)) {
        cli::cli_abort("Invalid number of blocks (0).")
      }
      if (length(blocks) == 1) {
        blocks <- c(blocks, blocks)
      }
      r1 <- r1[[1]]
      
      cr <- list()
      tr <- list()
      
      # Manage RasterLayer
      r1 <- terra::rast(r1)
      
      n.c <- terra::ncol(r1)
      n.r1 <- terra::nrow(r1)
      
      # cr
      b <- blocks[1]
      if (b == 1) {
        cr$row <- 1
        cr$nrows <- n.c
        cr$n <- 1
      } else {
        if (b >= n.c) {
          b <- n.c
        }
        if (n.c %% b == 0) {
          by <- n.c / b
          cr$row <- seq(1, to = n.c, by = by)
          cr$nrows <- rep(by, b)
          cr$n <- length(cr$row)
        } else {
          by <- floor(n.c / b)
          cr$row <- c(1, seq(1 + by + (n.c %% b), to = n.c, by = by))
          cr$nrows <- c(cr$row[2:length(cr$row)], n.c + 1) - cr$row
          cr$n <- length(cr$row)
        }
      }
      
      # tr
      b <- blocks[2]
      if (b == 1) {
        tr$row <- 1
        tr$nrows <- n.r1
        tr$n <- 1
      } else {
        if (b >= n.r1) {
          b <- n.r1
        }
        if (n.r1 %% b == 0) {
          by <- n.r1 / b
          tr$row <- seq(1, to = n.r1, by = by)
          tr$nrows <- rep(by, b)
          tr$n <- length(tr$row)
        } else {
          by <- floor(n.r1 / b)
          tr$row <- c(1, seq(1 + by + (n.r1 %% b), to = n.r1, by = by))
          tr$nrows <- c(tr$row[2:length(tr$row)], n.r1 + 1) - tr$row
          tr$n <- length(tr$row)
        }
      }
    }
    bs <- bs(r1, blocks)
    
    tr <- bs$tr
    cr <- bs$cr
  } else {
    tr <- terra::blocks(r1[[1]], 100)
    cr <- terra::blocks(terra::t(r1[[1]]), 100)
  }
  
  message(
    "Splitting ",
    length(names(r1)),
    " band(s) into ",
    cr$n,
    " x ",
    tr$n,
    " blocks..."
  )
  
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
  
  srid <- tryCatch(
    suppressMessages(rpostgis::pgSRID(
      con,
      sf::st_crs(terra::crs(r1)),
      create.srid = TRUE
    )[[1]]),
    error = function(e) 0
  )
  
  # Default to EPSG:4326 when CRS is missing or unrecognized
  if (length(srid) != 1 || srid == 0) {
    warning(
      "The raster CRS was missing or unrecognized. Defaulting to EPSG:4326."
    )
    srid <- 4326
  }
  
  # Grid with all band/block combinations
  crossed_df <- expand.grid(
    trn = 1:tr$n,
    crn = 1:cr$n,
    band = 1:terra::nlyr(r1)
  )
  
  n <- unlist(tapply(crossed_df$band, crossed_df$band, function(x) {
    seq(from = n.base + 1, by = 1, length.out = length(x))
  }))
  
  rgrid <- cbind(crossed_df, n = n)
  
  # Function to export a block
  export_block <- function(band, trn, crn, n) {
    # Get band b
    rb <- r1[[band]]
    
    # Handle empty data rasters by setting ndval (-99999) to all values
    if (all(is.na(terra::values(rb)))) {
      terra::values(rb) <- ndval
    }
    
    # Get raster tile
    suppressWarnings(
      r <- rb[
        tr$row[trn]:(tr$row[trn] + tr$nrows[trn] - 1),
        cr$row[crn]:(cr$row[crn] + cr$nrows[crn] - 1),
        drop = FALSE
      ]
    )
    
    # Get extent and dimensions of tile
    ex <- terra::ext(r)
    d <- dim(r)
    
    # Only ST_MakeEmptyRaster/ST_AddBand during first band loop
    if (band == 1) {
      # Create empty raster
      tmp.query <- paste0(
        "INSERT INTO ",
        rast_table,
        " (rid, r_class, r_proj4, rast) VALUES (",
        n,
        ",",
        r_class,
        ",",
        r_crs,
        ", ST_MakeEmptyRaster(",
        d[2],
        ",",
        d[1],
        ",",
        ex[1],
        ",",
        ex[4],
        ",",
        res[1],
        ",",
        -res[2],
        ", 0, 0,",
        srid[1],
        ") );"
      )
      DBI::dbExecute(con, tmp.query)
      
      # Upper left x/y for alignment snapping
      # if (trn == 1 & crn == 1) {
      tmp.query <- paste0(
        "SELECT ST_UpperLeftX(rast) x FROM ",
        rast_table,
        " where rid = ",
        n,
        ";"
      )
      upx <- DBI::dbGetQuery(con, tmp.query)$x
      tmp.query <- paste0(
        "SELECT ST_UpperLeftY(rast) y FROM ",
        rast_table,
        " where rid = ",
        n,
        ";"
      )
      upy <- DBI::dbGetQuery(con, tmp.query)$y
      # }
      
      # New band
      if (res[1] != res[2]) {
        s2g <- paste0(", ", res[1], ", ", -res[2])
      } else {
        s2g <- NULL
      }
      bndargs <- paste0(
        "ROW(",
        1:length(names(r1)),
        ",",
        bit.depth,
        "::text,0,",
        ndval,
        ")"
      )
      tmp.query <- paste0(
        "UPDATE ",
        rast_table,
        " SET rast = ST_SnapToGrid(ST_AddBand(rast,ARRAY[",
        paste(bndargs, collapse = ","),
        "]::addbandarg[]), ",
        upx,
        ",",
        upy,
        s2g,
        ") ",
        "where rid = ",
        n,
        ";"
      )
      DBI::dbExecute(con, tmp.query)
    }
    
    mr <- terra::as.matrix(r, wide = TRUE)
    mr[is.na(mr)] <- ndval
    r2 <- paste(
      apply(mr, 1, FUN = function(x) {
        paste0("[", paste(x, collapse = ","), "]")
      }),
      collapse = ","
    )
    
    tmp.query <- paste0(
      "UPDATE ",
      rast_table,
      " SET rast = ST_SetValues(rast,",
      band,
      ", 1, 1, ARRAY[",
      r2,
      "]::double precision[][])
                               where rid = ",
      n,
      ";"
    )
    DBI::dbExecute(con, tmp.query)
  }
  
  purrr::pmap(list(rgrid$band, rgrid$trn, rgrid$crn, rgrid$n), export_block)
  
  # Get rid of the rows just appended. Done as a query here in case any rows in object rgrid failed to append
  new_rids <- DBI::dbGetQuery(
    con,
    paste0("SELECT rid FROM ", rast_table, " WHERE rid > ", n.base, ";")
  )[, 1]
  
  # Create index if new table, else reindex
  if (new) {
    tmp.query <- paste0(
      "CREATE INDEX ",
      gsub("\"", "", sub(".*\\.", "", rast_table)),
      "_rast_st_conhull_idx ON ",
      rast_table,
      " USING gist( ST_ConvexHull(rast) );"
    )
    DBI::dbExecute(con, tmp.query)
  } else { 
    try({
      # re-index existing table for performance
      tmp.query <- paste0("REINDEX INDEX CONCURRENTLY ",  gsub("\"", "", rast_table),
                          "_rast_st_conhull_idx;")
      DBI::dbExecute(con, tmp.query)
    })
  }
  
  # 5. add raster constraints
  if (constraints) {
    tmp.query <- paste0(
      "SELECT AddRasterConstraints('spatial'::name,",
      DBI::dbQuoteString(con, sub(".*\\.", "", rast_table)),
      "::name, 'rast'::name);"
    )
    DBI::dbExecute(con, tmp.query)
  }
  
  return(list(status = TRUE, appended_rids = new_rids))
}

