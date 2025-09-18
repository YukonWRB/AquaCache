#' Maintenance (vacuum) function for aquacache database.
#'
#' @description
#'
#' Performs maintenance operations operation on the database, according to the parameters selected.
#'
#' @param con A connection to the database. If left NULL will use function AquaConnect and automatically disconnect when finished.
#' @param vacuum Performs a vacuum with analyze. Default TRUE.
#' @param vacuum_full Performs a full vacuum. This takes longer and requires an exclusive lock, but can reclaim more space as tables are re-written without dead space. Default FALSE.
#' @param timeseries_check Runs check on that data present in the timeseries table: ensures that each timeseries_id is used somewhere in the database (warns if not) and checks start and end datetimes against the data in measurement tables.
#' @param locations_check Runs checks on the data present in the locations table: Ensures that each location is used somewhere in the database (warns if not) and makes sure that each location has an associated point in the 'vectors' table.
#' @param visibility_check Checks all table entries where share_with != 'public_reader' AND an entry to column private_expiry. If after the private_expiry date, the record visibility is modified to 'public_reader' and private_expiry set to NULL
#'
#' @return TRUE if completed successfully, possibly with messages printed to the console.
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
    DBI::dbExecute(
      con,
      paste0(
        "UPDATE internal_status SET value = '",
        .POSIXct(Sys.time(), "UTC"),
        "' WHERE event = 'last_vacuum';"
      )
    )
    message("Database vacuum completed")
  }

  if (timeseries_check) {
    ts_tbl <- DBI::dbGetQuery(con, "SELECT * FROM timeseries;")
    for (i in 1:nrow(ts_tbl)) {
      # Confirm start/end datetimes are correct
      tsid <- ts_tbl$timeseries_id[i]
      start_rt <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT MIN(datetime) FROM measurements_continuous WHERE timeseries_id = ",
          tsid,
          ";"
        )
      )[1, 1]
      start_dly <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT MIN(date) FROM measurements_calculated_daily WHERE timeseries_id = ",
          tsid,
          ";"
        )
      )[1, 1]
      if (is.na(start_rt) && is.na(start_dly)) {
        message(
          "No data found associated with timeseries_id ",
          tsid,
          ". You should consider deleting this timeseries."
        )
        next
      }
      start <- if (!is.na(start_rt) & !is.na(start_dly)) {
        min(start_rt, as.POSIXct(start_dly, tz = "UTC"))
      } else if (is.na(start_rt)) {
        start_dly
      } else {
        start_rt
      }

      end_rt <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT MAX(datetime) FROM measurements_continuous WHERE timeseries_id = ",
          tsid,
          ";"
        )
      )[1, 1]
      end_dly <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT MAX(date) FROM measurements_calculated_daily WHERE timeseries_id = ",
          tsid,
          ";"
        )
      )[1, 1]
      end <- if (!is.na(end_rt) & !is.na(end_dly)) {
        max(end_rt, as.POSIXct(end_dly, tz = "UTC"))
      } else if (is.na(end_rt)) {
        end_dly
      } else {
        end_rt
      }

      if (start != ts_tbl$start_datetime[i]) {
        DBI::dbExecute(
          con,
          paste0(
            "UPDATE timeseries SET start_datetime = '",
            start,
            "' WHERE timeseries_id = ",
            tsid,
            ";"
          )
        )
      }
      if (end != ts_tbl$end_datetime[i]) {
        DBI::dbExecute(
          con,
          paste0(
            "UPDATE timeseries SET end_datetime = '",
            end,
            "' WHERE timeseries_id = ",
            tsid,
            ";"
          )
        )
      }
    }
    message("Timeseries checks completed")
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
      loc_name <- loc_tbl$location[i]

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
          tolower(loc_tbl$location[i]),
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
          "feature_name" = loc_tbl$location[i],
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
