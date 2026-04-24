#' Maintenance function for aquacache database
#'
#' @description
#'
#' Performs maintenance operations on the database according to the parameters selected.
#'
#' @param con A connection to the database. If left NULL will use function AquaConnect and automatically disconnect when finished.
#' @param vacuum Performs a vacuum with analyze. Default TRUE.
#' @param vacuum_full Performs a full vacuum. This takes longer and requires an exclusive lock, but can reclaim more space as tables are re-written without dead space. Default FALSE.
#' @param timeseries_check Runs checks on data present in the timeseries table: ensures that each timeseries_id is used somewhere in the database (warns if not) and checks start and end datetimes against the data in measurement tables and adjusts if needed.
#' @param locations_check Runs checks on the data present in the locations table: Ensures that each location is used somewhere in the database (warns if not) and makes sure that each location has an associated point in the 'vectors' table.
#' @param visibility_check Checks all tables that have colums for 'share_with' AND 'private_expiry'. For rows where share_with != 'public_reader' AND private_expiry is NOT NULL, a check is made if the expiry datetime has been reached. If after the private_expiry date, the record visibility is modified to 'public_reader' and private_expiry set to NULL.
#'
#' @return TRUE if completed successfully, possibly with messages and warnings printed to the console.
#' @export
#'

maintain <- function(
  con = NULL,
  vacuum = TRUE,
  vacuum_full = FALSE,
  timeseries_check = FALSE,
  locations_check = FALSE,
  visibility_check = TRUE
) {
  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }
  if (vacuum) {
    if (vacuum_full) {
      DBI::dbExecute(con, "VACUUM (FULL, ANALYZE)")
    } else {
      DBI::dbExecute(con, "VACUUM (ANALYZE)")
    }

    try(
      # In a try in case the user doesn't have update permissions on internal_status
      {
        DBI::dbExecute(
          con,
          "UPDATE internal_status SET value = NOW() WHERE event = 'last_vacuum';"
        )
      },
      silent = TRUE
    )

    message("Database vacuum completed")
  }

  if (timeseries_check) {
    metadata_refresh <- DBI::dbGetQuery(
      con,
      "SELECT
         to_regprocedure(
           'continuous.refresh_basic_timeseries_datetime_bounds(integer[])'
         ) IS NOT NULL AS has_basic_refresh,
         to_regprocedure(
           'continuous.refresh_direct_compound_timeseries_datetime_bounds(integer[])'
         ) IS NOT NULL AS has_compound_refresh"
    )

    if (
      !isTRUE(metadata_refresh$has_basic_refresh[[1]]) ||
        !isTRUE(metadata_refresh$has_compound_refresh[[1]])
    ) {
      warning(
        "Timeseries metadata refresh functions are not installed in the database; start_datetime/end_datetime checks were skipped."
      )
    } else {
      DBI::dbExecute(
        con,
        "SELECT continuous.refresh_basic_timeseries_datetime_bounds(
           ARRAY(
             SELECT timeseries_id
             FROM continuous.timeseries
             WHERE timeseries_type = 'basic'
           )
         );"
      )
      DBI::dbExecute(
        con,
        "SELECT continuous.refresh_direct_compound_timeseries_datetime_bounds(
           ARRAY(
             SELECT timeseries_id
             FROM continuous.timeseries
           )
         );"
      )
      message("Timeseries datetime metadata refresh completed")
    }
  }

  if (locations_check) {
    loc_tbl <- DBI::dbGetQuery(con, "SELECT * FROM locations;")

    # Get the foreign key references for the locations table
    refs <- DBI::dbGetQuery(
      con,
      "WITH fk_deps AS (
          SELECT
            con.oid AS constraint_oid,
            ns.nspname AS fk_schema,
            cl.relname AS fk_table,
            a.attname AS fk_column
          FROM pg_constraint con
          JOIN pg_class cl ON cl.oid = con.conrelid
          JOIN pg_namespace ns ON ns.oid = cl.relnamespace
          JOIN unnest(con.confkey) WITH ORDINALITY AS fk_cols(colnum, ord) 
            ON TRUE
          JOIN pg_attribute a 
            ON a.attrelid = con.confrelid 
           AND a.attnum   = con.confkey[fk_cols.ord]
          WHERE con.contype = 'f'
            AND con.confrelid = 'public.locations'::regclass
            AND a.attname = 'location_id'
        )
        SELECT fk_schema, fk_table, fk_column
        FROM fk_deps;
        "
    )

    for (i in 1:nrow(loc_tbl)) {
      locid <- loc_tbl$location_id[i]
      loc_name <- loc_tbl$name[i]

      # Check if location is used in any other table
      # Loop over all tables listed in 'refs' until a reference to 'locid' is found
      for (j in 1:nrow(refs)) {
        schema <- refs$fk_schema[j]
        tbl <- refs$fk_table[j]
        col <- refs$fk_column[j]

        # Check if the location_id is used in the current table
        check <- DBI::dbGetQuery(
          con,
          paste0(
            "SELECT COUNT(*) FROM ",
            schema,
            ".",
            tbl,
            " WHERE ",
            col,
            " = ",
            locid,
            ";"
          )
        )[1, 1]
        if (check > 0) {
          break
        }
        if (j == nrow(refs)) {
          message(
            "Location ",
            loc_name,
            " (",
            locid,
            ") is not used in any other table. You should consider deleting this location."
          )
        }
      }

      # Check if location has a corresponding entry in the vectors table
      vec_check <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT COUNT(*) FROM spatial.vectors WHERE layer_name = 'Locations' AND LOWER(feature_name) = '",
          tolower(loc_tbl$location_code[i]),
          "';"
        )
      )
      if (vec_check == 0) {
        message(
          "Location ",
          loc_name,
          " (",
          locid,
          ") does not have a corresponding entry in the vectors table, creating it."
        )
        # Create a new entry in the vectors table
        point <- data.frame(
          "feature_name" = loc_tbl$location_code[i],
          "description" = loc_tbl$name[i],
          "latitude" = loc_tbl$latitude[i],
          "longitude" = loc_tbl$longitude[i]
        )
        point <- terra::vect(
          point,
          geom = c("longitude", "latitude"),
          crs = "epsg:4269"
        )
        insertACVector(
          geom = point,
          layer_name = "Locations",
          feature_name_col = "feature_name",
          description_col = "description",
          con = con
        )
      }
    }
    message("Location checks completed")
  }

  if (visibility_check) {
    # get tables that have BOTH share_with (text[]) and private_expiry (date)
    sql <- "
      SELECT table_schema, table_name
      FROM information_schema.columns
      WHERE (column_name IN ('share_with','private_expiry'))
        AND table_schema NOT IN ('pg_catalog','information_schema')
      GROUP BY table_schema, table_name
      HAVING COUNT(DISTINCT column_name) = 2;
      "
    tbls <- DBI::dbGetQuery(con, sql)

    for (k in seq_len(nrow(tbls))) {
      sch <- tbls$table_schema[k]
      tab <- tbls$table_name[k]
      # Update share_with to 'public_reader' and set private_expiry to NULL if private_expiry is before today
      DBI::dbExecute(
        con,
        paste0(
          "UPDATE ",
          sch,
          ".",
          tab,
          " SET share_with = ARRAY['public_reader']::text[] WHERE share_with IS DISTINCT FROM ARRAY['public_reader']::text[] AND private_expiry < CURRENT_DATE;"
        )
      )
      # Set private_expiry to NULL if share_with is 'public_reader'
      DBI::dbExecute(
        con,
        paste0(
          "UPDATE ",
          sch,
          ".",
          tab,
          " SET private_expiry = NULL WHERE share_with = ARRAY['public_reader']::text[];"
        )
      )
    }
  }

  return(TRUE)
}
