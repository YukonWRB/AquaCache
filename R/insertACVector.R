#' Add a vector file to the database
#'
#'@description
#'
#' Use this function to add a vector file to the database. Ensures that database constraints are met and permits spatial queries. If you need to delete (not overwrite) a vector for any reason you'll have to use SQL (perhaps via R using the DBI package).
#'
#' ## Extracting from the database
#' Use function [YGwater::getVector()] to retrieve a point, line, or polygon from the database.
#'
#' ## Attribute tables:
#' The attribute table of the object will be retained in the `attributes` JSON column for all fields that are not already represented by dedicated columns in the `vectors` table. This preserves additional metadata for each feature while still allowing spatial relationships to be maintained within the database. Fields supplied through `feature_name`/`feature_name_col` and `description`/`description_col` are still written to their respective columns, with all remaining fields stored as JSON.
#'
#' @param geom The geometry object to add to the database, as a [terra::vect()] object or as a file path to a shapefile, geopackage or something else that terra::vect() can use. Conversion will automatically be made to epsg:4269, NAD83 lat/long decimal degrees. Can be points, lines, or polygons with one or more features. Multi-feature geoms will be split up into individual database entries.
#' @param layer_name The name to give to the vector layer, which defines which layer_name it gets assigned to in the database. This should always be an existing layer_name unless you have a good reason to create a new one.
#' @param feature_name A short but descriptive name to give to the geom feature. Leave NULL if specified with parameter `feature_name_col` instead. This parameter only works for geoms with a single feature (row).
#' @param description Optional but highly recommended long-form description of the geometry feature. Leave NULL is specifying a `description_col` instead.
#' @param feature_name_col The name of the column with names to give to the geom features. Each feature (row in the attribute table) will be entered to the database using the string in this column. Leave NULL if specified with parameter `feature_name`.
#' @param description_col The name of the column containing descriptions associated with each feature. Each feature (row in the attribute table) will be entered to the database using the string in this column. Leave NULL if specified with parameter `description` instead.
#' @param table The target table in the database (as character string). See 'schema' if not under the 'spatial' schema.
#' @param schema The schema in which the target 'table' is located. Default is 'spatial'. Note that this is NOT the default for [rpostgis::pgWriteGeom()].
#' @param geom_col The name of the database table column in which to insert the geometry object.
#' @param overwrite If a row already exists for the combination of layer_name, name,  and geometry type (point, line, or polygon), should it be overwritten?
#' @param ask Whether to ask for user confirmation when creating a new layer_name or overwriting existing entries. Default TRUE.
#' @param use_ogr2ogr Whether to use the `ogr2ogr` command line utility when it
#'   is available. This is much faster for multi-feature vector loads. If
#'   `ogr2ogr` is unavailable or the fast path fails, the existing R-only
#'   workflow is used.
#' @param install_ogr2ogr Whether to offer an interactive attempt to install a
#'   GDAL distribution that includes `ogr2ogr` when the utility cannot be found.
#'   This is only attempted when `use_ogr2ogr = TRUE`.
#' @param con A connection to the database. Default NULL will use the utility function [AquaConnect()] and disconnect afterwards.
#'
#' @return A boolean vector, one element per feature. Messages will also be printed to the console.
#' @export

insertACVector <- function(
  geom,
  layer_name,
  feature_name = NULL,
  description = NULL,
  feature_name_col = NULL,
  description_col = NULL,
  table = "vectors",
  schema = "spatial",
  geom_col = "geom",
  overwrite = FALSE,
  ask = TRUE,
  use_ogr2ogr = TRUE,
  install_ogr2ogr = interactive(),
  con = NULL
) {
  if (!isTRUE(use_ogr2ogr)) {
    return(insertACVector_old(
      geom = geom,
      layer_name = layer_name,
      feature_name = feature_name,
      description = description,
      feature_name_col = feature_name_col,
      description_col = description_col,
      table = table,
      schema = schema,
      geom_col = geom_col,
      overwrite = overwrite,
      ask = ask,
      con = con
    ))
  }

  ogr2ogr_path <- find_gdal_utility("ogr2ogr")

  if (!nzchar(ogr2ogr_path) && isTRUE(install_ogr2ogr)) {
    ogr2ogr_path <- aquacache_try_install_ogr2ogr(ask = ask)
  }

  if (!nzchar(ogr2ogr_path)) {
    message(
      "ogr2ogr was not found. Defaulting to the existing slower R-only vector insert method."
    )
    return(insertACVector_old(
      geom = geom,
      layer_name = layer_name,
      feature_name = feature_name,
      description = description,
      feature_name_col = feature_name_col,
      description_col = description_col,
      table = table,
      schema = schema,
      geom_col = geom_col,
      overwrite = overwrite,
      ask = ask,
      con = con
    ))
  }

  tryCatch(
    insertACVector_ogr2ogr(
      geom = geom,
      layer_name = layer_name,
      feature_name = feature_name,
      description = description,
      feature_name_col = feature_name_col,
      description_col = description_col,
      table = table,
      schema = schema,
      geom_col = geom_col,
      overwrite = overwrite,
      ask = ask,
      con = con,
      ogr2ogr_path = ogr2ogr_path
    ),
    error = function(e) {
      warning(
        "ogr2ogr vector insert failed: ",
        conditionMessage(e),
        "\nDefaulting to the existing slower R-only vector insert method.",
        call. = FALSE
      )
      insertACVector_old(
        geom = geom,
        layer_name = layer_name,
        feature_name = feature_name,
        description = description,
        feature_name_col = feature_name_col,
        description_col = description_col,
        table = table,
        schema = schema,
        geom_col = geom_col,
        overwrite = overwrite,
        ask = ask,
        con = con
      )
    }
  )
}


#' @keywords internal
#' @noRd
insertACVector_old <- function(
  geom,
  layer_name,
  feature_name = NULL,
  description = NULL,
  feature_name_col = NULL,
  description_col = NULL,
  table = "vectors",
  schema = "spatial",
  geom_col = "geom",
  overwrite = FALSE,
  ask = TRUE,
  con = NULL
) {
  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }

  DBI::dbExecute(con, "SET timezone = 'UTC'")

  exist_layer_names <- DBI::dbGetQuery(
    con,
    paste0("SELECT DISTINCT layer_name FROM ", schema, ".", table, ";")
  )

  if (!layer_name %in% exist_layer_names$layer_name) {
    if (ask) {
      message(
        "The layer_name you specified does not exist yet. Are you sure you want to create it? The current entries are:\n",
        paste(exist_layer_names$layer_name, collapse = "\n")
      )
      commit <- readline(
        prompt = writeLines(paste(
          "\n1: Definitely not",
          "\n2: Maybe?",
          "\n3: Yes, I want to create it."
        ))
      )
      commit <- as.numeric(commit)
    } else {
      commit <- 3
    }
    if (commit != 3) {
      stop("Allright, come back when you're ready.")
    }
  }

  if (inherits(geom, "character")) {
    geom <- terra::vect(geom)
  } else if (!inherits(geom, "SpatVector")) {
    stop(
      "The 'geom' parameter must be a file path to a vector file or a terra::vect() object."
    )
  }

  if (is.null(feature_name) & is.null(feature_name_col)) {
    stop("You need to specify either a 'feature_name' or 'feature_name_col.'")
  }

  if (!is.null(feature_name_col) & !is.null(feature_name)) {
    stop(
      "You specified a 'feature_name' as well as a 'feature_name_col.' Choose only one. 'feature_name' only works for objects with one feature, 'feature_name_col' works for multiple features."
    )
  }

  if (!is.null(feature_name_col)) {
    if (!(feature_name_col %in% names(geom))) {
      stop("You specified a non-existent column for 'feature_name_col.'")
    }
    if (any(duplicated(geom[[feature_name_col]]))) {
      stop(
        "Multiple rows share the same value in the 'feature_name_col' column. You can either modify the column values or aggregate geometries on this column, i.e. using terra::aggregation(geom, by = feature_name_col, dissolve = TRUE)."
      )
    }
  }

  if (!is.null(feature_name) & nrow(geom) > 1) {
    stop(
      "You specified the parameter 'feature_name' but this only works for vector files with one feature. Please review the help file."
    )
  }

  # re-project if necessary
  if (!(terra::same.crs(geom, "epsg:4269"))) {
    geom <- terra::project(geom, "epsg:4269")
  }
  tbl <- as.data.frame(geom)
  attr_json <- insertACVector_attribute_json(
    tbl = tbl,
    feature_name_col = feature_name_col,
    description_col = description_col
  )

  success <- vector()
  for (i in 1:nrow(geom)) {
    tryCatch(
      {
        sub.geom <- geom[i, 0] # Drop attribute table for the feature
        sub.geom$layer_name <- layer_name
        if (nrow(geom) == 1 & !is.null(feature_name)) {
          feat_name <- feature_name
          sub.geom$feature_name <- feat_name
        } else {
          feat_name <- tbl[i, feature_name_col]
          sub.geom$feature_name <- feat_name
        }
        if (!is.null(description)) {
          desc <- description
          sub.geom$description <- desc
        } else if (!is.null(description_col)) {
          desc <- tbl[i, description_col]
          sub.geom$description <- desc
        }

        sub.geom$attributes <- attr_json[[i]]

        if (!terra::is.valid(sub.geom)) {
          sub.geom <- terra::makeValid(sub.geom)
          message(
            "geom object had invalid geometry, fix attempted using terra::makeValid()."
          )
        }

        if (overwrite) {
          exist <- DBI::dbGetQuery(
            con,
            paste0(
              "SELECT layer_name, feature_name, geom_type, geom_id FROM ",
              schema,
              ".",
              table,
              " WHERE layer_name = '",
              layer_name,
              "' AND feature_name = '",
              feat_name,
              "';"
            )
          )
          if (nrow(exist) == 1) {
            message(
              "Updating entry for layer_name = ",
              layer_name,
              ", feature_name = ",
              feat_name,
              "."
            )
            sub.geom$geom_id <- exist[1, "geom_id"]
            success[[i]] <- suppressMessages(rpostgis::pgWriteGeom(
              con,
              name = c(schema, table),
              data.obj = sub.geom,
              geom = geom_col,
              partial.match = TRUE,
              upsert.using = "geom_id"
            ))
          } else if (nrow(exist) == 0) {
            message(
              "There is no existing database entry for this mix of layer_name = ",
              layer_name,
              ", feature_name = ",
              feat_name,
              ". Writing it without overwrite."
            )
            success[[i]] <- suppressMessages(rpostgis::pgWriteGeom(
              con,
              name = c(schema, table),
              data.obj = sub.geom,
              geom = geom_col,
              partial.match = TRUE
            ))
          }
        } else {
          # overwrite if FALSE
          exist <- DBI::dbGetQuery(
            con,
            paste0(
              "SELECT layer_name, feature_name, geom_type, geom_id FROM ",
              schema,
              ".",
              table,
              " WHERE layer_name = '",
              layer_name,
              "' AND feature_name = '",
              feat_name,
              "';"
            )
          )
          if (nrow(exist) != 0) {
            if (ask) {
              message(
                "There is already an entry for layer_name = ",
                layer_name,
                " and feature_name = ",
                feat_name,
                " but you didn't ask to overwrite it. Would you like to delete the old feature and replace it with the new one?"
              )
              agg <- readline(
                prompt = writeLines(paste("\n1: Yes", "\n2: No way!"))
              )
            } else {
              agg <- 2
            }
            agg <- as.numeric(agg)
            if (agg != 1) {
              if (ask) {
                warning(
                  "Not writing layer_name = ",
                  layer_name,
                  ", feature_name = ",
                  feat_name,
                  ". There is already an entry matching this but parameter overwrite is FALSE."
                )
              }
              success[[i]] <- FALSE
            } else {
              DBI::dbExecute(
                con,
                paste0(
                  "DELETE FROM ",
                  schema,
                  ".",
                  table,
                  " WHERE geom_id = ",
                  exist$geom_id,
                  ";"
                )
              )
              success[[i]] <- suppressMessages(rpostgis::pgWriteGeom(
                con,
                name = c(schema, table),
                data.obj = sub.geom,
                geom = geom_col,
                partial.match = TRUE
              ))
              message("Succeeded in adding to the existing vector!")
            }
          } else {
            success[[i]] <- suppressMessages(rpostgis::pgWriteGeom(
              con,
              name = c(schema, table),
              data.obj = sub.geom,
              geom = geom_col,
              partial.match = TRUE
            ))
          }
        }
      },
      error = function(e) {
        if (!success[[i]]) {
          warning(
            "Failed to write feature ",
            i,
            ": error occured when calling rpostgis::pgWriteGeom to write to the DB."
          )
        } else {
          warning("Unknown error occured while writing feature ", i, ".")
        }
      }
    )
  }

  return(success)
}


#' Build vector attribute JSON by column
#'
#' @keywords internal
#' @noRd
insertACVector_attribute_json <- function(
  tbl,
  feature_name_col = NULL,
  description_col = NULL
) {
  n_features <- nrow(tbl)
  drop_cols <- c("layer_name", "feature_name", "description")
  if (!is.null(feature_name_col)) {
    drop_cols <- c(drop_cols, feature_name_col)
  }
  if (!is.null(description_col)) {
    drop_cols <- c(drop_cols, description_col)
  }
  keep_cols <- setdiff(names(tbl), unique(drop_cols))
  if (!length(keep_cols)) {
    return(rep(NA_character_, n_features))
  }

  attribute_data <- tbl[, keep_cols, drop = FALSE]
  json_names <- as.character(jsonlite::toJSON(
    names(attribute_data),
    auto_unbox = FALSE,
    na = "null",
    collapse = FALSE
  ))

  json_cols <- vector("list", length(attribute_data))
  for (j in seq_along(attribute_data)) {
    values <- attribute_data[[j]]
    if (inherits(values, "POSIXct")) {
      values <- format(values, tz = "UTC", usetz = TRUE)
    } else if (inherits(values, "Date")) {
      values <- format(values, "%Y-%m-%d")
    } else if (is.factor(values)) {
      values <- as.character(values)
    }

    json_values <- as.character(jsonlite::toJSON(
      values,
      auto_unbox = FALSE,
      null = "null",
      na = "null",
      POSIXt = "ISO8601",
      digits = NA,
      collapse = FALSE
    ))
    if (length(json_values) != n_features) {
      stop(
        "Could not serialize vector attribute column '",
        names(attribute_data)[[j]],
        "' as one JSON value per feature."
      )
    }
    json_cols[[j]] <- paste0(json_names[[j]], ":", json_values)
  }

  paste0("{", do.call(paste, c(json_cols, sep = ",")), "}")
}


#' @keywords internal
#' @noRd
insertACVector_ogr2ogr <- function(
  geom,
  layer_name,
  feature_name = NULL,
  description = NULL,
  feature_name_col = NULL,
  description_col = NULL,
  table = "vectors",
  schema = "spatial",
  geom_col = "geom",
  overwrite = FALSE,
  ask = TRUE,
  con = NULL,
  ogr2ogr_path
) {
  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
  }

  DBI::dbExecute(con, "SET timezone = 'UTC'")

  target_id <- if (is.null(schema) || !nzchar(schema)) {
    DBI::Id(table = table)
  } else {
    DBI::Id(schema = schema, table = table)
  }
  target_sql <- as.character(DBI::dbQuoteIdentifier(con, target_id))
  geom_col_sql <- as.character(DBI::dbQuoteIdentifier(con, geom_col))

  exist_layer_names <- DBI::dbGetQuery(
    con,
    paste0("SELECT DISTINCT layer_name FROM ", target_sql, ";")
  )

  if (!layer_name %in% exist_layer_names$layer_name) {
    if (ask) {
      message(
        "The layer_name you specified does not exist yet. Are you sure you want to create it? The current entries are:\n",
        paste(exist_layer_names$layer_name, collapse = "\n")
      )
      commit <- readline(
        prompt = writeLines(paste(
          "\n1: Definitely not",
          "\n2: Maybe?",
          "\n3: Yes, I want to create it."
        ))
      )
      commit <- as.numeric(commit)
    } else {
      commit <- 3
    }
    if (commit != 3) {
      stop("Allright, come back when you're ready.")
    }
  }

  if (inherits(geom, "character")) {
    geom <- terra::vect(geom)
  } else if (!inherits(geom, "SpatVector")) {
    stop(
      "The 'geom' parameter must be a file path to a vector file or a terra::vect() object."
    )
  }

  if (is.null(feature_name) & is.null(feature_name_col)) {
    stop("You need to specify either a 'feature_name' or 'feature_name_col.'")
  }

  if (!is.null(feature_name_col) & !is.null(feature_name)) {
    stop(
      "You specified a 'feature_name' as well as a 'feature_name_col.' Choose only one. 'feature_name' only works for objects with one feature, 'feature_name_col' works for multiple features."
    )
  }

  if (!is.null(feature_name_col)) {
    if (!(feature_name_col %in% names(geom))) {
      stop("You specified a non-existent column for 'feature_name_col.'")
    }
    if (any(duplicated(geom[[feature_name_col]]))) {
      stop(
        "Multiple rows share the same value in the 'feature_name_col' column. You can either modify the column values or aggregate geometries on this column, i.e. using terra::aggregation(geom, by = feature_name_col, dissolve = TRUE)."
      )
    }
  }

  if (!is.null(feature_name) & nrow(geom) > 1) {
    stop(
      "You specified the parameter 'feature_name' but this only works for vector files with one feature. Please review the help file."
    )
  }

  if (!is.null(description_col) && !(description_col %in% names(geom))) {
    stop("You specified a non-existent column for 'description_col.'")
  }

  if (!(terra::same.crs(geom, "epsg:4269"))) {
    geom <- terra::project(geom, "epsg:4269")
  }

  valid <- terra::is.valid(geom)
  valid <- as.logical(valid)
  valid[is.na(valid)] <- FALSE
  if (any(!valid)) {
    geom <- terra::makeValid(geom)
    message(
      "geom object had invalid geometry, fix attempted using terra::makeValid()."
    )
  }

  tbl <- as.data.frame(geom)
  n_features <- nrow(geom)

  feature_names <- if (!is.null(feature_name)) {
    rep(as.character(feature_name), n_features)
  } else {
    as.character(tbl[[feature_name_col]])
  }

  descriptions <- if (!is.null(description)) {
    rep(as.character(description), n_features)
  } else if (!is.null(description_col)) {
    as.character(tbl[[description_col]])
  } else {
    rep(NA_character_, n_features)
  }

  attr_json <- insertACVector_attribute_json(
    tbl = tbl,
    feature_name_col = feature_name_col,
    description_col = description_col
  )

  existing <- insertACVector_fetch_existing(
    con = con,
    target_sql = target_sql,
    layer_name = layer_name,
    feature_names = feature_names
  )

  resolved <- insertACVector_resolve_actions(
    existing = existing,
    feature_names = feature_names,
    layer_name = layer_name,
    overwrite = overwrite,
    ask = ask
  )
  actions <- resolved$actions
  update_geom_ids <- resolved$update_geom_ids

  success <- actions != "skip"
  if (!any(success)) {
    message(
      "No vector features were written. All ",
      length(success),
      " feature(s) already exist for layer_name = ",
      layer_name,
      " and overwrite is FALSE or replacement was declined."
    )
    return(success)
  }

  load_geom <- geom[success, 0]
  load_geom$ac_row <- which(success)
  load_geom$ac_action <- actions[success]
  load_geom$ac_geom_id <- update_geom_ids[success]
  load_geom$layer_name <- layer_name
  load_geom$feature_name <- feature_names[success]
  load_geom$description <- descriptions[success]
  load_geom$attributes <- attr_json[success]

  tmp_gpkg <- tempfile(fileext = ".gpkg")
  on.exit(unlink(tmp_gpkg), add = TRUE)
  terra::writeVector(
    load_geom,
    filename = tmp_gpkg,
    layer = "vectors",
    filetype = "GPKG",
    overwrite = TRUE
  )

  stage_table <- paste0(
    "aquacache_vector_stage_",
    Sys.getpid(),
    "_",
    sample.int(1000000L, 1L)
  )
  stage_id <- if (is.null(schema) || !nzchar(schema)) {
    DBI::Id(table = stage_table)
  } else {
    DBI::Id(schema = schema, table = stage_table)
  }
  stage_sql <- as.character(DBI::dbQuoteIdentifier(con, stage_id))
  on.exit(
    try(
      DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", stage_sql)),
      silent = TRUE
    ),
    add = TRUE
  )

  insertACVector_run_ogr2ogr(
    con = con,
    ogr2ogr_path = ogr2ogr_path,
    source_file = tmp_gpkg,
    source_layer = "vectors",
    stage_table = stage_table,
    stage_schema = schema
  )

  target_cols <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT column_name",
      "FROM information_schema.columns",
      "WHERE table_schema = $1",
      "  AND table_name = $2"
    ),
    params = list(schema, table)
  )$column_name
  has_attributes <- "attributes" %in% target_cols

  seq_name <- DBI::dbGetQuery(
    con,
    "SELECT pg_get_serial_sequence($1, 'geom_id') AS seqname;",
    params = list(
      if (is.null(schema) || !nzchar(schema)) {
        table
      } else {
        paste0(schema, ".", table)
      }
    )
  )$seqname[[1]]
  if (is.na(seq_name) || !nzchar(seq_name)) {
    stop(
      "Could not find the geom_id sequence for ",
      target_sql,
      ". The ogr2ogr fast path needs this sequence to bulk insert rows."
    )
  }

  attr_update_sql <- if (has_attributes) {
    ", attributes = NULLIF(s.attributes, '')::jsonb"
  } else {
    ""
  }
  attr_insert_cols_sql <- if (has_attributes) {
    ", attributes"
  } else {
    ""
  }
  attr_insert_vals_sql <- if (has_attributes) {
    ", NULLIF(s.attributes, '')::jsonb"
  } else {
    ""
  }

  geom_expr <- paste0("s.", geom_col_sql)

  delete_sql <- paste0(
    "DELETE FROM ",
    target_sql,
    " t ",
    "USING ",
    stage_sql,
    " s ",
    "WHERE s.ac_action = 'deleteinsert' ",
    "  AND t.layer_name = s.layer_name ",
    "  AND t.feature_name = s.feature_name;"
  )
  update_sql <- paste0(
    "UPDATE ",
    target_sql,
    " t SET ",
    "layer_name = s.layer_name, ",
    "feature_name = s.feature_name, ",
    "description = NULLIF(s.description, '')",
    attr_update_sql,
    ", ",
    geom_col_sql,
    " = ",
    geom_expr,
    " ",
    "FROM ",
    stage_sql,
    " s ",
    "WHERE s.ac_action = 'update' ",
    "  AND t.geom_id = s.ac_geom_id;"
  )
  insert_sql <- paste0(
    "INSERT INTO ",
    target_sql,
    " (geom_id, layer_name, feature_name, description",
    attr_insert_cols_sql,
    ", ",
    geom_col_sql,
    ") ",
    "SELECT nextval(",
    DBI::dbQuoteString(con, seq_name),
    "::regclass), ",
    "s.layer_name, s.feature_name, NULLIF(s.description, '')",
    attr_insert_vals_sql,
    ", ",
    geom_expr,
    " ",
    "FROM ",
    stage_sql,
    " s ",
    "WHERE s.ac_action IN ('insert', 'deleteinsert');"
  )

  in_transaction <- dbTransCheck(con)
  if (!isTRUE(in_transaction)) {
    DBI::dbExecute(con, "BEGIN;")
  }

  tryCatch(
    {
      DBI::dbExecute(con, "SET LOCAL synchronous_commit = off;")
      DBI::dbExecute(con, delete_sql)
      DBI::dbExecute(con, update_sql)
      DBI::dbExecute(con, insert_sql)
      if (!isTRUE(in_transaction)) {
        DBI::dbExecute(con, "COMMIT;")
      }
    },
    error = function(e) {
      if (!isTRUE(in_transaction)) {
        try(DBI::dbExecute(con, "ROLLBACK;"), silent = TRUE)
      }
      stop(e)
    }
  )

  message(
    "Succeeded in adding ",
    sum(actions %in% c("insert", "deleteinsert")),
    " and updating ",
    sum(actions == "update"),
    " vector feature(s) using ogr2ogr."
  )

  success
}


#' Resolve vector insert/update/skip actions
#'
#' @keywords internal
#' @noRd
insertACVector_resolve_actions <- function(
  existing,
  feature_names,
  layer_name,
  overwrite = FALSE,
  ask = TRUE
) {
  n_features <- length(feature_names)
  actions <- rep("insert", n_features)
  update_geom_ids <- rep(NA_integer_, n_features)

  if (!nrow(existing)) {
    return(list(actions = actions, update_geom_ids = update_geom_ids))
  }

  existing_feature_names <- as.character(existing$feature_name)
  existing_match <- match(existing_feature_names, feature_names, nomatch = 0L)
  existing_counts <- tabulate(existing_match, nbins = n_features)
  existing_idx <- which(existing_counts > 0L)
  if (!length(existing_idx)) {
    return(list(actions = actions, update_geom_ids = update_geom_ids))
  }

  if (overwrite) {
    duplicate_idx <- which(existing_counts > 1L)
    if (length(duplicate_idx)) {
      stop(
        "Multiple existing database entries match layer_name = '",
        layer_name,
        "' and feature_name = '",
        feature_names[[duplicate_idx[[1]]]],
        "'. The ogr2ogr fast path cannot choose which row to update."
      )
    }

    existing_row <- match(feature_names[existing_idx], existing_feature_names)
    actions[existing_idx] <- "update"
    update_geom_ids[existing_idx] <- existing$geom_id[existing_row]
    message(
      "Updating ",
      length(existing_idx),
      " existing vector feature(s) for layer_name = ",
      layer_name,
      "."
    )
    return(list(actions = actions, update_geom_ids = update_geom_ids))
  }

  if (!ask) {
    actions[existing_idx] <- "skip"
    return(list(actions = actions, update_geom_ids = update_geom_ids))
  }

  for (i in existing_idx) {
    message(
      "There is already an entry for layer_name = ",
      layer_name,
      " and feature_name = ",
      feature_names[[i]],
      " but you didn't ask to overwrite it. Would you like to delete the old feature and replace it with the new one?"
    )
    agg <- readline(prompt = writeLines(paste("\n1: Yes", "\n2: No way!")))
    agg <- as.numeric(agg)
    if (agg != 1) {
      warning(
        "Not writing layer_name = ",
        layer_name,
        ", feature_name = ",
        feature_names[[i]],
        ". There is already an entry matching this but parameter overwrite is FALSE."
      )
      actions[[i]] <- "skip"
    } else {
      actions[[i]] <- "deleteinsert"
    }
  }

  list(actions = actions, update_geom_ids = update_geom_ids)
}


#' @keywords internal
#' @noRd
insertACVector_fetch_existing <- function(
  con,
  target_sql,
  layer_name,
  feature_names
) {
  feature_names <- unique(feature_names)
  feature_names <- feature_names[!is.na(feature_names) & nzchar(feature_names)]
  if (!length(feature_names)) {
    return(data.frame(
      feature_name = character(),
      geom_id = integer(),
      geom_type = character()
    ))
  }

  layer_exists <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT 1 FROM ",
      target_sql,
      " WHERE layer_name = ",
      DBI::dbQuoteString(con, layer_name),
      " LIMIT 1;"
    )
  )
  if (!nrow(layer_exists)) {
    return(data.frame(
      feature_name = character(),
      geom_id = integer(),
      geom_type = character()
    ))
  }

  if (length(feature_names) > 1000L) {
    temp_table <- paste0(
      "aquacache_vector_features_",
      Sys.getpid(),
      "_",
      sample.int(1000000L, 1L)
    )
    temp_id <- DBI::Id(table = temp_table)
    temp_sql <- as.character(DBI::dbQuoteIdentifier(con, temp_id))
    temp_result <- tryCatch(
      {
        DBI::dbWriteTable(
          con,
          name = temp_id,
          value = data.frame(feature_name = feature_names),
          temporary = TRUE,
          overwrite = TRUE
        )
        on.exit(
          try(
            DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", temp_sql)),
            silent = TRUE
          ),
          add = TRUE
        )
        DBI::dbGetQuery(
          con,
          paste0(
            "SELECT v.feature_name, v.geom_id, v.geom_type FROM ",
            target_sql,
            " v INNER JOIN ",
            temp_sql,
            " f ON v.feature_name = f.feature_name",
            " WHERE v.layer_name = ",
            DBI::dbQuoteString(con, layer_name),
            ";"
          )
        )
      },
      error = function(e) NULL
    )
    if (!is.null(temp_result)) {
      return(temp_result)
    }
  }

  chunks <- split(feature_names, ceiling(seq_along(feature_names) / 1000L))
  out <- lapply(chunks, function(chunk) {
    feature_sql <- paste(DBI::dbQuoteString(con, chunk), collapse = ", ")
    DBI::dbGetQuery(
      con,
      paste0(
        "SELECT feature_name, geom_id, geom_type FROM ",
        target_sql,
        " WHERE layer_name = ",
        DBI::dbQuoteString(con, layer_name),
        " AND feature_name IN (",
        feature_sql,
        ");"
      )
    )
  })

  do.call(rbind, out)
}


#' @keywords internal
#' @noRd
insertACVector_run_ogr2ogr <- function(
  con,
  ogr2ogr_path,
  source_file,
  source_layer,
  stage_table,
  stage_schema = NULL
) {
  psql_path <- find_postgres_utility("psql")
  if (!nzchar(psql_path)) {
    stop("psql was not found, so ogr2ogr PGDump output cannot be loaded.")
  }

  conn_info <- tryCatch(DBI::dbGetInfo(con), error = function(e) list())

  old_env <- Sys.getenv(
    c(
      "PGHOST",
      "PGPORT",
      "PGDATABASE",
      "PGUSER",
      "PGPASSWORD",
      "PSQLRC",
      "PGOPTIONS"
    ),
    unset = NA_character_
  )
  on.exit(
    invisible(mapply(
      function(k, v) if (is.na(v)) Sys.unsetenv(k) else Sys.setenv(k = v),
      names(old_env),
      old_env
    )),
    add = TRUE
  )

  if (!is.null(conn_info$host) && nzchar(conn_info$host)) {
    Sys.setenv(PGHOST = conn_info$host)
  }
  if (!is.null(conn_info$port) && nzchar(as.character(conn_info$port))) {
    Sys.setenv(PGPORT = as.character(conn_info$port))
  }
  if (!is.null(conn_info$dbname) && nzchar(conn_info$dbname)) {
    Sys.setenv(PGDATABASE = conn_info$dbname)
  }
  if (!is.null(conn_info$user) && nzchar(conn_info$user)) {
    Sys.setenv(PGUSER = conn_info$user)
  }

  pg_pass <- Sys.getenv("aquacacheAdminPass", unset = NA_character_)
  if (!is.na(pg_pass) && nzchar(pg_pass)) {
    Sys.setenv(PGPASSWORD = pg_pass)
  }
  Sys.setenv(
    PSQLRC = if (.Platform$OS.type == "windows") "NUL" else "/dev/null"
  )
  Sys.setenv(PGOPTIONS = "-c synchronous_commit=off")

  tmp_sql <- tempfile(fileext = ".sql")
  tmp_ogr_out <- tempfile(fileext = ".log")
  tmp_ogr_err <- tempfile(fileext = ".log")
  tmp_psql_out <- tempfile(fileext = ".log")
  tmp_psql_err <- tempfile(fileext = ".log")
  on.exit(
    unlink(c(tmp_sql, tmp_ogr_out, tmp_ogr_err, tmp_psql_out, tmp_psql_err)),
    add = TRUE
  )

  args <- c(
    "-f",
    "PGDUMP",
    shQuote(tmp_sql),
    shQuote(source_file),
    source_layer,
    "-nln",
    stage_table,
    "-overwrite",
    "-lco",
    "GEOMETRY_NAME=geom",
    "-lco",
    "FID=ogr_fid",
    "-lco",
    "DROP_TABLE=IF_EXISTS",
    "-lco",
    "CREATE_SCHEMA=NO",
    "-lco",
    "SPATIAL_INDEX=NONE",
    "-lco",
    "UNLOGGED=ON",
    "-lco",
    "SRID=4269",
    "-dim",
    "XY",
    "-t_srs",
    "EPSG:4269"
  )

  if (!is.null(stage_schema) && nzchar(stage_schema)) {
    args <- c(args, "-lco", paste0("SCHEMA=", stage_schema))
  }

  status <- system2(
    command = ogr2ogr_path,
    args = args,
    stdout = tmp_ogr_out,
    stderr = tmp_ogr_err
  )

  if (!identical(status, 0L)) {
    err <- readLines(tmp_ogr_err, warn = FALSE)
    out <- readLines(tmp_ogr_out, warn = FALSE)
    msg <- c(err, out)
    if (!length(msg)) {
      msg <- "ogr2ogr exited with a non-zero status but produced no output."
    }
    stop(paste(msg, collapse = "\n"))
  }

  psql_args <- c(
    "-X",
    "--no-password",
    "--no-psqlrc",
    "--echo-errors",
    "-v",
    "ON_ERROR_STOP=1",
    "-f",
    shQuote(tmp_sql)
  )

  psql_status <- system2(
    command = psql_path,
    args = psql_args,
    stdout = tmp_psql_out,
    stderr = tmp_psql_err
  )

  if (!identical(psql_status, 0L)) {
    err <- readLines(tmp_psql_err, warn = FALSE)
    out <- readLines(tmp_psql_out, warn = FALSE)
    msg <- c(err, out)
    if (!length(msg)) {
      msg <- "psql exited with a non-zero status while loading ogr2ogr PGDump output."
    }
    stop(paste(msg, collapse = "\n"))
  }

  invisible(TRUE)
}
