#' Write raster to PostGIS database table.
#'
#' @description
#' This function is not meant to be used by itself: in most cases use [insertACModelRaster()] or [insertACRaster()] which will populate reference tables so that your raster can be easily found later.
#'
#' Sends R raster data to a PostGIS database table via the `raster2pgsql`/`psql`
#' toolchain so it can be fetched later into an R environment. This helper keeps
#' the existing `spatial.rasters`/`spatial.rasters_reference` relationship intact
#' by appending tiles to the raster table and returning the new `rid` values. The
#' target raster table must already exist before calling this helper and both
#' `raster2pgsql` and `psql` must be available on the system PATH.
#'
#' @details
#' SpatRaster band names will be stored in an array in the column
#' "band_names", which will be restored in R when imported with the function
#' \code{\link[rpostgis]{pgGetRast}}.
#'
#' If \code{blocks = NULL}, the raster is uploaded as a single tile. When a
#' 1- or 2-length integer vector is supplied, it represents the desired number
#' of tiles along the X and Y axes; the raster will be uploaded in tiles sized
#' \code{ceiling(ncol(raster) / blocks[1])} by \code{ceiling(nrow(raster) / blocks[2])}
#' pixels. Fewer, larger tiles generally result in faster uploads and downloads.
#' Each tile is inserted as a brand-new row, so previously stored rasters remain
#' untouched regardless of how much data is already present in the table. PostGIS
#' rasters do not need to share a common tiling scheme, so older uploads and new
#' inserts retrieved through the same reference_id workflow continue to behave in
#' exactly the same way even when their tile dimensions differ.
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
#' @param constraints Whether to reset constraints. Will drop all constraints except for SRID.
#'
#' @export
#' @keywords internal
#' @return A list with TRUE for successful import and the rid(s) of the appended entries.

writeRaster <- function(
    con,
    raster,
    rast_table = c("spatial", "rasters"),
    bit.depth = NULL,
    blocks = NULL,
    constraints = FALSE
) {
  if (!suppressMessages(rpostgis::pgPostGIS(con))) {
    stop("PostGIS is not enabled on this database.")
  }
  
  if (!inherits(raster, "SpatRaster")) {
    stop("Raster must be a terra SpatRaster object.")
  }
  
  raster2pgsql_path <- Sys.which("raster2pgsql")
  psql_path <- Sys.which("psql")
  if (!nzchar(raster2pgsql_path) || !nzchar(psql_path)) {
    warning("Either raster2pgsql or psql utilities were not found on the system PATH. Defaulting to the (slow) R only method.")
    res <- writeRaster_old(con = con,
                           raster = raster,
                           rast_table = rast_table,
                           blocks = blocks,
                           bit.depth = bit.depth,
                           constraints = FALSE)
    return(res)
  }
  
  # Make sure raster is in WGS84 (EPSG:4326) before writing to the database
  # Ensure a CRS is present to avoid SRID 0 in the database
  if (is.na(terra::crs(raster)) || terra::crs(raster) == "") {
    warning("Raster has no CRS. Defaulting to EPSG:4326.")
    terra::crs(raster) <- "EPSG:4326"
  } else {
    crs_str <- terra::crs(raster)
    if (is.na(crs_str) || crs_str == "") {
      warning("Raster has no CRS. Defaulting to EPSG:4326.")
      terra::crs(raster) <- "EPSG:4326"
    } else if (!grepl("EPSG\\s*:?\\s*4326", crs_str, ignore.case = TRUE)) {
      raster <- terra::project(raster, "EPSG:4326")
    }
    
  }
  
  # Prepare table identifiers -------------------------------------------------
  r_class <- class(raster)[1]
  r_crs <- terra::crs(raster)
  
  if (length(rast_table) == 1 && grepl(".", rast_table[1], fixed = TRUE)) {
    table_parts <- strsplit(rast_table[1], ".", fixed = TRUE)[[1]]
    if (length(table_parts) != 2) {
      cli::cli_abort("Unable to parse rast_table specification '{rast_table}'.")
    }
    schema_name <- table_parts[1]
    table_name <- table_parts[2]
    rast_table_id <- DBI::Id(schema = schema_name, table = table_name)
  } else if (length(rast_table) == 2) {
    schema_name <- rast_table[1]
    table_name <- rast_table[2]
    rast_table_id <- DBI::Id(schema = schema_name, table = table_name)
  } else {
    schema_name <- NULL
    table_name <- rast_table
    rast_table_id <- DBI::Id(table = table_name)
  }
  
  rast_table_sql <- DBI::dbQuoteIdentifier(con, rast_table_id)
  table_plain <- table_name
  if (is.null(schema_name)) {
    current_schema <- tryCatch(
      DBI::dbGetQuery(con, "SELECT current_schema() AS schema")[['schema']],
      error = function(e) NA_character_
    )
    if (is.na(current_schema) || current_schema == "") {
      schema_for_constraints <- "public"
    } else {
      schema_for_constraints <- current_schema
    }
  } else {
    schema_for_constraints <- schema_name
  }
  
  qualified_table <- if (is.null(schema_name)) {
    table_plain
  } else {
    paste0(schema_name, ".", table_plain)
  }
  
  if (!DBI::dbExistsTable(con, rast_table_id)) {
    cli::cli_abort(
      "Raster table '{if (is.null(schema_name)) table_plain else paste0(schema_name, '.', table_plain)}' must exist before calling writeRaster()."
    )
  }
  
  if (constraints) {
    message("Resetting raster constraints (keeping SRID constraint)...")
    
    # Drop all raster constraints except SRID in case they already exist
    DBI::dbExecute(
      con,
      paste0(
        "SELECT DropRasterConstraints(",
        DBI::dbQuoteString(con, schema_for_constraints),
        ", ",
        DBI::dbQuoteString(con, table_plain),
        ",'rast',",
        "FALSE, ", # srid
       "TRUE, ", # scale_x
       "TRUE, ", # scale_y
       "TRUE, ", # blocksize_x
       "TRUE, ", # blocksize_y
       "TRUE, ", # same_alignment
       "TRUE, ", # nodata
       "TRUE, ", # out_db
       "TRUE, ", # extent
       "TRUE, ", # num_bands
       "TRUE, ", # pixel_types
       "TRUE", # spatial extent
      ");"
      )
    )
    
    # fix rows that might violate SRID=4326
    # Adjust based on r_proj4 column if available
    DBI::dbExecute(
      con,
      paste0(
        "UPDATE ", rast_table_sql, " ",
        "SET rast = ST_SetSRID(rast, 4326) ",
        "WHERE ST_SRID(rast) = 0 ",
        "  AND (COALESCE(r_proj4,'') ILIKE '%4326%' ",
        "       OR COALESCE(r_proj4,'') ILIKE '%WGS 84%');"
      )
    )
    # Transform others to 4326 if possible
    DBI::dbExecute(
      con,
      paste0(
        "UPDATE ", rast_table_sql, " ",
        "SET rast = ST_Transform(rast, 4326) ",
        "WHERE ST_SRID(rast) NOT IN (0, 4326);"
      )
    )
    
    # Adjust the r_proj4 column accordingly
    DBI::dbExecute(
      con,
      paste0(
        "UPDATE ", rast_table_sql, " ",
        "SET r_proj4 = 'EPSG:4326' ",
        "WHERE ST_SRID(rast) = 4326;"
      )
    )
    
    # Re-add SRID constraint only in case it wasn't there
    DBI::dbExecute(
      con,
      paste0(
        "SELECT AddRasterConstraints(",
        DBI::dbQuoteString(con, schema_for_constraints),
        ", ",
        DBI::dbQuoteString(con, table_plain),
        ",'rast',",
        "TRUE, ", # srid
       "FALSE, ", # scale_x
       "FALSE, ", # scale_y
       "FALSE, ", # blocksize_x
       "FALSE, ", # blocksize_y
       "FALSE, ", # same_alignment
       "FALSE, ", # regular blocking
       "FALSE, ", # num bands
       "FALSE, ", # pixel types
       "FALSE, ", # nodata values
       "FALSE, ", # out_db
       "FALSE", # spatial extent
      ");"
      )
    )
  }
  
  # Determine srid ------------------------------------------------------------
  srid <- tryCatch(
    suppressMessages(rpostgis::pgSRID(
      con,
      sf::st_crs(terra::crs(raster)),
      create.srid = TRUE
    )[[1]]),
    error = function(e) 0
  )
  
  if (length(srid) != 1 || srid == 0) {
    warning(
      "The raster CRS was missing or unrecognized. Defaulting to EPSG:4326."
    )
    srid <- 4326
  }
  
  r1 <- raster
  
  # Determine tiling strategy -------------------------------------------------
  tile_cols <- terra::ncol(r1)
  tile_rows <- terra::nrow(r1)
  tile_option <- NULL
  if (!is.null(blocks)) {
    blocks <- as.integer(blocks)
    if (any(is.na(blocks)) || length(blocks) > 2) {
      cli::cli_abort("blocks must be a 1- or 2-length integer vector.")
    }
    if (any(blocks <= 0)) {
      cli::cli_abort("Invalid number of blocks (must be > 0).")
    }
    if (length(blocks) == 1) {
      blocks <- rep(blocks, 2)
    }
    tile_cols <- max(1L, ceiling(terra::ncol(r1) / blocks[1]))
    tile_rows <- max(1L, ceiling(terra::nrow(r1) / blocks[2]))
    tile_option <- paste0(tile_cols, "x", tile_rows)
  }
  
  if (is.null(tile_option)) {
    message(
      "Uploading ",
      terra::nlyr(r1),
      " band(s) as a single PostGIS raster tile"
    )
  } else {
    message(
      "Uploading ",
      terra::nlyr(r1),
      " band(s) as PostGIS raster tiles sized ",
      tile_option,
      " pixels"
    )
  }
  
  # Determine terra datatype if a bit depth was provided ----------------------
  terra_datatype <- NULL
  if (!is.null(bit.depth)) {
    terra_datatype <- switch(
      toupper(bit.depth),
      "32BF" = "FLT4S",
      "32BUI" = "INT4U",
      "32BSI" = "INT4S",
      cli::cli_abort("Unsupported bit.depth '{bit.depth}'.")
    )
  }
  
  tmp_file <- tempfile(fileext = ".tif")
  on.exit(unlink(tmp_file), add = TRUE)
  
  write_args <- list(r1, filename = tmp_file, overwrite = TRUE)
  if (!is.null(terra_datatype)) {
    write_args$datatype <- terra_datatype
  }
  do.call(terra::writeRaster, write_args)
  
  file_size <- file.info(tmp_file)$size
  if (is.na(file_size) || file_size == 0) {
    stop("Failed to serialise raster to temporary GeoTIFF for upload.")
  }
  
  max_before <- DBI::dbGetQuery(
    con,
    paste0("SELECT COALESCE(max(rid), 0) AS max_rid FROM ", rast_table_sql, ";")
  )$max_rid
  if (length(max_before) != 1 || is.na(max_before)) {
    max_before <- 0L
  }
  
  tmp_sql <- tempfile(fileext = ".sql")
  tmp_r2p_err <- tempfile(fileext = ".log")
  on.exit(unlink(tmp_sql), add = TRUE)
  on.exit(unlink(tmp_r2p_err), add = TRUE)
  
  # Call raster2pgsql to generate SQL ----------------------------------------
  r2p_args <- c("-a", "-s", as.character(srid), "-f", "rast")
  if (!is.null(tile_option)) {
    r2p_args <- c(r2p_args, "-t", tile_option)
  }
  r2p_args <- c(r2p_args, tmp_file, qualified_table)
  
  r2p_status <- system2(
    command = raster2pgsql_path,
    args = r2p_args,
    stdout = tmp_sql,
    stderr = tmp_r2p_err
  )
  
  if (!identical(r2p_status, 0L)) {
    r2p_err <- readLines(tmp_r2p_err, warn = FALSE)
    if (length(r2p_err) == 0) {
      r2p_err <- "raster2pgsql exited with a non-zero status but produced no error output."
    }
    stop(paste0("raster2pgsql failed: ", paste(r2p_err, collapse = "\n")))
  }
  
  qt <- as.character(rast_table_sql)          # e.g. "\"spatial\".\"rasters\"" or "\"rasters\""
  qt_rx <- gsub("([][\\^$.|?*+(){}\\\\])", "\\\\\\1", qt, perl = TRUE)  
  sql_lines <- readLines(tmp_sql, warn = FALSE)
  idx <- grepl(sprintf("^\\s*INSERT\\s+INTO\\s+%s\\s*\\(\"rast\"\\)\\s*VALUES\\s*\\(", qt_rx),
               sql_lines, perl = TRUE)
  sql_lines[idx] <- sub(";\\s*$", " RETURNING rid;", sql_lines[idx], perl = TRUE)
  writeLines(sql_lines, tmp_sql)
  
  
  # Prepare and run psql command to execute the SQL ------------------------------
  psql_args <- c(
    "-X",                 # no .psqlrc variables
    "--no-password",      # never prompt (fail fast)
    "--no-psqlrc",        # ignore ~/.psqlrc entirely
    "--echo-errors",
    "-v","ON_ERROR_STOP=1",
    "-v","VERBOSITY=verbose",
    "-tA",                # tuples only, unaligned (cleaner to parse)
    "-f", tmp_sql         # then run your raster2pgsql SQL
  )
  
  conn_info <- tryCatch(DBI::dbGetInfo(con), error = function(e) list())
  
  # Save/restore env
  old_env <- Sys.getenv(c("PGHOST","PGPORT","PGDATABASE","PGUSER","PGPASSWORD","PGCONNECT_TIMEOUT","PSQLRC"), unset = NA)
  on.exit(invisible(mapply(function(k,v) if (is.na(v)) Sys.unsetenv(k) else Sys.setenv(k=v),
                           names(old_env), old_env)), add = TRUE)
  
  # Export connection vars for psql
  if (!is.null(conn_info$host) && nzchar(conn_info$host)) Sys.setenv(PGHOST = conn_info$host)
  if (!is.null(conn_info$port) && nzchar(as.character(conn_info$port))) Sys.setenv(PGPORT = as.character(conn_info$port))
  if (!is.null(conn_info$dbname) && nzchar(conn_info$dbname)) Sys.setenv(PGDATABASE = conn_info$dbname)
  if (!is.null(conn_info$user) && nzchar(conn_info$user)) Sys.setenv(PGUSER = conn_info$user)
  
  # Password from .Renviron (correct key)
  pg_pass <- Sys.getenv("aquacacheAdminPass", unset = NA_character_)
  # Only set password if found
  if (!is.na(pg_pass) && nzchar(pg_pass)) {
    Sys.setenv(PGPASSWORD = pg_pass)
    on.exit(Sys.unsetenv("PGPASSWORD"), add = TRUE)
  } else {
    warning("No password found in .Renviron under 'aquacacheAdminPass'. Can't add a raster without this variable.")
  }
  
  # Make failures obvious & fast
  Sys.setenv(PSQLRC = if (.Platform$OS.type == "windows") "NUL" else "/dev/null")
  
  tmp_psql_err <- tempfile(fileext = ".log")
  tmp_psql_out <- tempfile(fileext = ".log")
  on.exit(unlink(tmp_psql_err), add = TRUE)
  on.exit(unlink(tmp_psql_out), add = TRUE)
  
  psql_status <- system2(
    command = psql_path,
    args = psql_args,
    stdout = tmp_psql_out,
    stderr = tmp_psql_err
  )
  
  out <- readLines(tmp_psql_out, warn = FALSE)
  new_rids <- suppressWarnings(as.integer(out[grepl("^[0-9]+$", out)]))
  new_rids <- new_rids[!is.na(new_rids)]
  
  if (!identical(psql_status, 0L)) {
    err_msg <- readLines(tmp_psql_err, warn = FALSE)
    if (length(err_msg) == 0) {
      err_msg <- "psql exited with a non-zero status but produced no error output."
    }
    stop(paste0("psql failed while loading raster tiles: ", paste(err_msg, collapse = "\n")))
  }
  
  if (!length(new_rids)) {
    msg_out <- paste(readLines(tmp_psql_out, warn=FALSE), collapse="\n")
    msg_err <- paste(readLines(tmp_psql_err, warn=FALSE), collapse="\n")
    message("psql stdout:\n", msg_out, "\npsql stderr:\n", msg_err)
  }
  
  
  if (length(new_rids) == 0) {
    stop("No raster tiles were appended by raster2pgsql. Check the raster input and table definition.")
  }
  
  
  rid_list <- paste(new_rids, collapse = ",")
  DBI::dbExecute(
    con,
    paste0(
      "UPDATE ",
      rast_table_sql,
      " SET r_class = ",
      DBI::dbQuoteString(con, r_class),
      ", r_proj4 = ",
      DBI::dbQuoteString(con, r_crs),
      " WHERE rid IN (",
      rid_list,
      ");"
    )
  )
  
  # Ensure a spatial index exists ---------------------------------------------
  index_name <- paste0(table_plain, "_rast_st_conhull_idx")
  index_regclass <- if (is.null(schema_name)) {
    index_name
  } else {
    paste0(schema_name, ".", index_name)
  }
  index_exists <- tryCatch(
    {
      DBI::dbGetQuery(
        con,
        "SELECT to_regclass($1::text) IS NOT NULL AS exists",
        params = list(index_regclass)
      )$exists
    },
    error = function(e) NA
  )
  if (!isTRUE(index_exists)) {
    index_sql <- paste0(
      "CREATE INDEX ",
      if (!is.null(schema_name)) paste0(DBI::dbQuoteIdentifier(con, schema_name), ".") else "",
      DBI::dbQuoteIdentifier(con, index_name),
      " ON ",
      rast_table_sql,
      " USING gist( ST_ConvexHull(rast) );"
    )
    DBI::dbExecute(con, index_sql)
  }
  
  return(list(status = TRUE, appended_rids = new_rids))
}

