# Patch 22

# Initial checks #################
# Ensure the user is postgres as this patch requires it
check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (check$session_user != "postgres") {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres user to make this work."
  )
}

message(
  "Working on Patch 22. Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state (but you have a backup, right?)."
)

message(
  "This patch updates several back-end functions to add checks and improve processes."
)

# Begin a transaction
message("Starting transaction...")

check <- dbTransCheck(con) # Check if a transaction is already in progress
if (check) {
  stop(
    "A transaction is already in progress. Please commit or rollback the current transaction before applying this patch."
  )
}
active <- dbTransBegin(con)

tryCatch(
  {
    DBI::dbExecute(
      con,
      "
                 CREATE OR REPLACE FUNCTION continuous.apply_corrections(p_timeseries_id integer, p_datetime timestamp with time zone, p_value numeric)
 RETURNS numeric
 LANGUAGE plpgsql
AS $function$
DECLARE
  corrected_value NUMERIC := p_value;
  correction_row RECORD;
  time_since_start NUMERIC;
  time_window NUMERIC;
  rate NUMERIC;
  correction NUMERIC;
BEGIN
  IF p_value IS NULL THEN
    RETURN NULL;
    END IF;
    
  FOR correction_row IN
    SELECT
      c.value1 AS c_value1,
      c.value2 AS c_value2,
      c.timestep_window AS c_timestep_window,
      c.equation AS c_equation,
      c.start_dt,
      c.end_dt,
      ct.correction_type,
      ct.priority
    FROM corrections c
    JOIN correction_types ct ON c.correction_type = ct.correction_type_id
    WHERE c.timeseries_id = p_timeseries_id
      AND c.start_dt <= p_datetime
      AND c.end_dt >= p_datetime
    ORDER BY ct.priority ASC
  LOOP
    -- Apply correction based on correction_row.correction_type
    IF correction_row.correction_type = 'delete' THEN
      -- Remove the data point
      RETURN NULL;
      
    ELSIF correction_row.correction_type = 'trim' THEN
      -- Remove data points outside of a specified value range
      IF correction_row.c_value1 IS NOT NULL AND corrected_value < correction_row.c_value1 THEN
        RETURN NULL;
      ELSIF correction_row.c_value2 IS NOT NULL AND corrected_value > correction_row.c_value2 THEN
        RETURN NULL;
      END IF;
      
    ELSIF correction_row.correction_type = 'offset linear' THEN
      -- Apply linear offset
      corrected_value := corrected_value + correction_row.c_value1;
      
    ELSIF correction_row.correction_type = 'offset two-point' THEN
      -- Apply two-point offset correction between start_dt and end_dt
      time_since_start := EXTRACT(EPOCH FROM (p_datetime - correction_row.start_dt));
      time_window := EXTRACT(EPOCH FROM (correction_row.end_dt - correction_row.start_dt));
      IF time_window <= 0 THEN
        RAISE EXCEPTION 'Invalid time window for offset two-point correction';
      END IF;
      rate := (correction_row.c_value2 - correction_row.c_value1) / time_window;
      correction := correction_row.c_value1 + rate * time_since_start;
      corrected_value := corrected_value + correction;
      
    ELSIF correction_row.correction_type = 'scale' THEN
      -- Apply percent scaling
      corrected_value := corrected_value * (correction_row.c_value1 / 100.0);
      
    ELSIF correction_row.correction_type = 'drift linear' THEN
      -- Apply linear drift correction that continues until end_dt
      IF p_datetime < correction_row.start_dt OR p_datetime > correction_row.end_dt THEN
        -- No correction applied outside the correction period
        correction := 0;
      ELSE
        -- During the correction period; apply proportional correction
        time_since_start := EXTRACT(EPOCH FROM (p_datetime - correction_row.start_dt));
        time_window := EXTRACT(EPOCH FROM correction_row.c_timestep_window);
        IF time_window <= 0 THEN
          RAISE EXCEPTION 'Invalid timestep_window for drift linear correction';
        END IF;
        rate := correction_row.c_value1 / time_window;  -- Correction per second
        correction := rate * time_since_start;
      END IF;
      corrected_value := corrected_value + correction;
      
    ELSIF correction_row.correction_type = 'drift equation' THEN
      -- Apply drift correction based on a user-provided equation
      time_since_start := EXTRACT(EPOCH FROM (p_datetime - correction_row.start_dt));
      EXECUTE format('SELECT %s', correction_row.c_equation) INTO correction USING corrected_value, time_since_start;
      corrected_value := correction;
      
    ELSE
      RAISE NOTICE 'Correction type % not handled', correction_row.correction_type;
    END IF;
  END LOOP;

  RETURN corrected_value;
END;
$function$
;
                 "
    )

    DBI::dbExecute(
      con,
      "
                 CREATE OR REPLACE VIEW continuous.measurements_continuous_corrected
AS SELECT mc.timeseries_id,
      mc.datetime,
  mc.value         AS value_raw,
  continuous.apply_corrections(
    mc.timeseries_id,
    mc.datetime,
    mc.value
  )                AS value_corrected,
  mc.period,
  mc.imputed
   FROM measurements_continuous mc
     JOIN timeseries ts ON mc.timeseries_id = ts.timeseries_id
     LEFT JOIN LATERAL ( SELECT apply_corrections(mc.timeseries_id, mc.datetime, mc.value) AS value_corrected
           FROM corrections c
          WHERE c.timeseries_id = mc.timeseries_id AND mc.datetime <@ tstzrange(c.start_dt, c.end_dt)) ac ON true
  WHERE ('public_reader'::text = ANY (ts.share_with)) OR (EXISTS ( SELECT 1
           FROM unnest(ts.share_with) role(role)
          WHERE pg_has_role(CURRENT_USER, role.role::name, 'MEMBER'::text) AND (role.role::name IN ( SELECT pg_roles.rolname
                   FROM pg_roles))));
                 "
    )

    # Modify validate_corrections()
    DBI::dbExecute(
      con,
      "
                 CREATE OR REPLACE FUNCTION continuous.validate_corrections()
 RETURNS trigger
 LANGUAGE plpgsql
AS $function$
DECLARE
  v1_required BOOLEAN;
  v2_required BOOLEAN;
  timestep_required BOOLEAN;
  equation_required BOOLEAN;
BEGIN
  -- Fetch the correction type definition from correction_types
  SELECT value1, value2, timestep_window, equation
  INTO v1_required, v2_required, timestep_required, equation_required
  FROM correction_types
  WHERE correction_type_id = NEW.correction_type;

  -- Check value1
  IF v1_required AND NEW.value1 IS NULL THEN
    RAISE EXCEPTION 'value1 cannot be NULL for correction_type_id %', NEW.correction_type;
  ELSIF NOT v1_required AND NEW.value1 IS NOT NULL THEN
    RAISE EXCEPTION 'value1 must be NULL for correction_type_id %', NEW.correction_type;
  END IF;

  -- Check value2
  IF v2_required AND NEW.value2 IS NULL THEN
    RAISE EXCEPTION 'value2 cannot be NULL for correction_type_id %', NEW.correction_type;
  ELSIF NOT v2_required AND NEW.value2 IS NOT NULL THEN
    RAISE EXCEPTION 'value2 must be NULL for correction_type_id %', NEW.correction_type;
  END IF;

  -- Check timestep_window
  IF timestep_required AND (NEW.timestep_window IS NULL OR NEW.timestep_window <= interval '0') THEN
    RAISE EXCEPTION 'timestep_window cannot be NULL or non-positive for correction_type_id %', NEW.correction_type;
  ELSIF NOT timestep_required AND NEW.timestep_window IS NOT NULL THEN
    RAISE EXCEPTION 'timestep_window must be NULL for correction_type_id %', NEW.correction_type;
  END IF;
  
  -- Check equation
  IF equation_required AND NEW.equation IS NULL THEN
    RAISE EXCEPTION 'equation cannot be NULL for correction_type_id %', NEW.correction_type;
  ELSIF NOT equation_required AND NEW.equation IS NOT NULL THEN
    RAISE EXCEPTION 'equation must be NULL for correction_type_id %', NEW.correction_type;
  END IF;

  RETURN NEW;
END;
$function$
;"
    )

    DBI::dbExecute(
      con,
      "
                 ALTER TABLE continuous.corrections
    ADD CONSTRAINT corrections_date_range_check CHECK (start_dt < end_dt);
                 "
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE ONLY public.correction_types
    ADD CONSTRAINT correction_types_type_key UNIQUE (correction_type);"
    )

    # Update foreign keys in locations_networks and locations_projects tables
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations_networks DROP CONSTRAINT networks_locations_network_id_fkey;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations_networks ADD CONSTRAINT networks_locations_network_id_fkey FOREIGN KEY (network_id) REFERENCES public.networks(network_id) ON DELETE CASCADE ON UPDATE CASCADE;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations_networks DROP CONSTRAINT networks_locations_location_id_fkey;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations_networks ADD CONSTRAINT networks_locations_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.locations(location_id) ON DELETE CASCADE ON UPDATE CASCADE;"
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations_projects DROP CONSTRAINT projects_locations_location_id_fkey;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations_projects ADD CONSTRAINT projects_locations_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.locations(location_id) ON DELETE CASCADE ON UPDATE CASCADE;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations_projects DROP CONSTRAINT projects_locations_project_id_fkey;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations_projects ADD CONSTRAINT projects_locations_project_id_fkey FOREIGN KEY (project_id) REFERENCES public.projects(project_id) ON DELETE CASCADE ON UPDATE CASCADE;"
    )

    # Update the version_info table
    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '22' WHERE item = 'Last patch number';"
    )
    DBI::dbExecute(
      con,
      paste0(
        "UPDATE information.version_info SET version = '",
        as.character(packageVersion("AquaCache")),
        "' WHERE item = 'AquaCache R package used for last patch';"
      )
    )

    # Commit the transaction
    DBI::dbExecute(con, "COMMIT;")

    message("Patch 22 applied successfully.")
  },
  error = function(e) {
    # Rollback the transaction
    DBI::dbExecute(con, "ROLLBACK;")
    stop(
      "Patch 22 failed and the DB has been rolled back to its earlier state. ",
      e$message
    )
  }
)
