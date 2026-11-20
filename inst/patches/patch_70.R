# Patch 70:
# 1. prevents UNS/UNK qualifiers from overlapping any other qualifier.
# Different non-placeholder qualifier types remain free to overlap, and the
# existing qualifiers_no_overlap exclusion constraint continues to prevent
# overlaps within one qualifier type.

patch_number <- 70L

check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")
if (check$session_user != "postgres") {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres user to make this work."
  )
}

if (dbTransCheck(con)) {
  stop(
    "A transaction is already in progress. Please commit or rollback it before applying this patch."
  )
}

required <- DBI::dbGetQuery(
  con,
  "SELECT
     to_regclass('continuous.qualifiers') IS NOT NULL AS has_qualifiers,
     to_regclass('public.qualifier_types') IS NOT NULL AS has_qualifier_types,
     to_regclass('information.version_info') IS NOT NULL AS has_version_info,
     EXISTS (
       SELECT 1 FROM pg_constraint
        WHERE conrelid = to_regclass('continuous.qualifiers')
          AND conname = 'qualifiers_no_overlap'
     ) AS has_overlap_constraint"
)
if (!all(unlist(required[1, ], use.names = FALSE))) {
  stop(
    "Patch 70 requires continuous.qualifiers, public.qualifier_types, the existing qualifiers_no_overlap constraint, and version metadata."
  )
}

last_patch <- DBI::dbGetQuery(
  con,
  "SELECT version FROM information.version_info
   WHERE item = 'Last patch number'"
)$version
if (length(last_patch) != 1L || last_patch != "69") {
  stop("Patch 70 must be applied to a database at Patch 69.")
}

active <- dbTransBegin(con)
tryCatch(
  {
    # Keep writes out between the legacy-state check and trigger installation.
    DBI::dbExecute(
      con,
      "LOCK TABLE continuous.qualifiers IN ACCESS EXCLUSIVE MODE"
    )

    special_types <- DBI::dbGetQuery(
      con,
      "SELECT qualifier_type_code
         FROM public.qualifier_types
        WHERE qualifier_type_code IN ('UNK', 'UNS')"
    )$qualifier_type_code
    if (!setequal(special_types, c("UNK", "UNS"))) {
      stop("Patch 70 requires qualifier type codes UNK and UNS.")
    }

    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS qualifiers_timeseries_period_gist
         ON continuous.qualifiers USING gist (
           timeseries_id,
           tstzrange(start_dt, end_dt, '[)')
         )"
    )

    special_overlap_sql <- "SELECT count(*) AS n
       FROM continuous.qualifiers a
       JOIN public.qualifier_types at
         ON at.qualifier_type_id = a.qualifier_type_id
       JOIN continuous.qualifiers b
         ON b.timeseries_id = a.timeseries_id
        AND b.qualifier_id > a.qualifier_id
        AND tstzrange(b.start_dt, b.end_dt, '[)')
            && tstzrange(a.start_dt, a.end_dt, '[)')
       JOIN public.qualifier_types bt
         ON bt.qualifier_type_id = b.qualifier_type_id
      WHERE at.qualifier_type_code IN ('UNK', 'UNS')
         OR bt.qualifier_type_code IN ('UNK', 'UNS')"
    special_overlap_count <- DBI::dbGetQuery(
      con,
      special_overlap_sql
    )$n[[1L]]
    if (special_overlap_count > 0L) {
      DBI::dbExecute(
        con,
        "CREATE TEMP TABLE patch_70_special_overlap_ids
         ON COMMIT DROP AS
         SELECT DISTINCT s.qualifier_id
           FROM continuous.qualifiers s
           JOIN public.qualifier_types st
             ON st.qualifier_type_id = s.qualifier_type_id
           JOIN continuous.qualifiers other
             ON other.timeseries_id = s.timeseries_id
            AND other.qualifier_id <> s.qualifier_id
            AND s.start_dt < s.end_dt
            AND other.start_dt < other.end_dt
            AND tstzrange(other.start_dt, other.end_dt, '[)')
                && tstzrange(s.start_dt, s.end_dt, '[)')
          WHERE st.qualifier_type_code IN ('UNK', 'UNS')"
      )

      # Subtract the union of all overlapping qualifier ranges from each
      # placeholder range. Keep non-overlapping pieces, reusing the existing
      # row id for the first piece and copying its source-protection flag.
      DBI::dbExecute(
        con,
        "CREATE TEMP TABLE patch_70_qualifier_residuals
         ON COMMIT DROP AS
         WITH special AS (
           SELECT q.qualifier_id, q.timeseries_id, q.qualifier_type_id,
                  q.start_dt, q.end_dt, q.no_source_update
             FROM continuous.qualifiers q
             JOIN public.qualifier_types t
               ON t.qualifier_type_id = q.qualifier_type_id
             JOIN pg_temp.patch_70_special_overlap_ids affected
               USING (qualifier_id)
            WHERE t.qualifier_type_code IN ('UNK', 'UNS')
         ), blockers AS (
           SELECT s.qualifier_id,
                  GREATEST(s.start_dt, other.start_dt) AS cut_start,
                  LEAST(s.end_dt, other.end_dt) AS cut_end
             FROM special s
             JOIN continuous.qualifiers other
               ON other.timeseries_id = s.timeseries_id
              AND other.qualifier_id <> s.qualifier_id
              AND other.start_dt < other.end_dt
              AND tstzrange(other.start_dt, other.end_dt, '[)')
                  && tstzrange(s.start_dt, s.end_dt, '[)')
         ), boundaries AS (
           SELECT qualifier_id, start_dt AS boundary
             FROM special
            WHERE start_dt < end_dt
           UNION
           SELECT qualifier_id, end_dt
             FROM special
            WHERE start_dt < end_dt
           UNION
           SELECT qualifier_id, cut_start FROM blockers
           UNION
           SELECT qualifier_id, cut_end FROM blockers
         ), ordered_boundaries AS (
           SELECT qualifier_id, boundary,
                  lead(boundary) OVER (
                    PARTITION BY qualifier_id ORDER BY boundary
                  ) AS next_boundary
             FROM boundaries
         ), pieces AS (
           SELECT s.qualifier_id, s.timeseries_id, s.qualifier_type_id,
                  s.no_source_update, b.boundary AS piece_start,
                  b.next_boundary AS piece_end
             FROM ordered_boundaries b
             JOIN special s USING (qualifier_id)
            WHERE b.next_boundary IS NOT NULL
              AND b.boundary < b.next_boundary
         ), uncovered AS (
           SELECT p.*
             FROM pieces p
            WHERE NOT EXISTS (
              SELECT 1 FROM blockers b
               WHERE b.qualifier_id = p.qualifier_id
                 AND b.cut_start < p.piece_end
                 AND b.cut_end > p.piece_start
            )
         )
         SELECT u.*,
                row_number() OVER (
                  PARTITION BY qualifier_id ORDER BY piece_start
                ) AS piece_number
           FROM uncovered u"
      )

      DBI::dbExecute(
        con,
        "UPDATE continuous.qualifiers q
            SET start_dt = r.piece_start,
                end_dt = r.piece_end
           FROM pg_temp.patch_70_qualifier_residuals r
          WHERE r.qualifier_id = q.qualifier_id
            AND r.piece_number = 1
            AND (q.start_dt IS DISTINCT FROM r.piece_start
              OR q.end_dt IS DISTINCT FROM r.piece_end)"
      )
      DBI::dbExecute(
        con,
        "DELETE FROM continuous.qualifiers q
         USING public.qualifier_types t
         WHERE t.qualifier_type_id = q.qualifier_type_id
           AND t.qualifier_type_code IN ('UNK', 'UNS')
           AND EXISTS (
             SELECT 1 FROM pg_temp.patch_70_special_overlap_ids affected
              WHERE affected.qualifier_id = q.qualifier_id
           )
           AND NOT EXISTS (
             SELECT 1 FROM pg_temp.patch_70_qualifier_residuals r
              WHERE r.qualifier_id = q.qualifier_id
           )"
      )
      DBI::dbExecute(
        con,
        "INSERT INTO continuous.qualifiers
           (timeseries_id, qualifier_type_id, start_dt, end_dt,
            no_source_update)
         SELECT timeseries_id, qualifier_type_id, piece_start, piece_end,
                no_source_update
           FROM pg_temp.patch_70_qualifier_residuals
          WHERE piece_number > 1"
      )

      special_overlap_count <- DBI::dbGetQuery(
        con,
        special_overlap_sql
      )$n[[1L]]
      if (special_overlap_count > 0L) {
        stop(
          "Patch 70 could not remove all existing overlaps involving UNS or UNK qualifiers. The transaction was rolled back."
        )
      }
      message(
        "Patch 70 removed overlapping portions of existing UNS/UNK qualifiers."
      )
    }

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.lock_qualifier_overlap_check()
       RETURNS trigger
       LANGUAGE plpgsql
       SECURITY DEFINER
       SET search_path = pg_catalog, continuous
       AS $function$
       DECLARE
         first_id INTEGER;
         second_id INTEGER;
       BEGIN
         IF TG_OP = 'UPDATE' AND OLD.timeseries_id <> NEW.timeseries_id THEN
           first_id := LEAST(OLD.timeseries_id, NEW.timeseries_id);
           second_id := GREATEST(OLD.timeseries_id, NEW.timeseries_id);
           PERFORM pg_advisory_xact_lock(
             hashtext('continuous.qualifier-special-overlap'), first_id
           );
           PERFORM pg_advisory_xact_lock(
             hashtext('continuous.qualifier-special-overlap'), second_id
           );
         ELSE
           PERFORM pg_advisory_xact_lock(
             hashtext('continuous.qualifier-special-overlap'), NEW.timeseries_id
           );
         END IF;
         RETURN NEW;
       END
       $function$"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION continuous.lock_qualifier_overlap_check() OWNER TO postgres"
    )
    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.check_special_qualifier_overlap()
       RETURNS trigger
       LANGUAGE plpgsql
       SECURITY DEFINER
       SET search_path = pg_catalog, continuous, public
       AS $function$
       DECLARE
         new_code TEXT;
         conflict_exists BOOLEAN;
       BEGIN
         -- An earlier deferred trigger event may describe a row later changed
         -- or deleted in this same transaction. Validate only its current
         -- state, if it still exists as proposed.
         IF NOT EXISTS (
           SELECT 1
             FROM continuous.qualifiers current_row
            WHERE current_row.qualifier_id = NEW.qualifier_id
              AND current_row.timeseries_id = NEW.timeseries_id
              AND current_row.qualifier_type_id = NEW.qualifier_type_id
              AND current_row.start_dt = NEW.start_dt
              AND current_row.end_dt = NEW.end_dt
         ) THEN
           RETURN NEW;
         END IF;

         SELECT t.qualifier_type_code INTO new_code
           FROM public.qualifier_types t
          WHERE t.qualifier_type_id = NEW.qualifier_type_id;
         IF new_code IS NULL THEN
           RETURN NEW;
         END IF;

         IF new_code IN ('UNK', 'UNS') THEN
           SELECT EXISTS (
             SELECT 1
               FROM continuous.qualifiers other_row
              WHERE other_row.timeseries_id = NEW.timeseries_id
                AND other_row.qualifier_id <> NEW.qualifier_id
                AND tstzrange(other_row.start_dt, other_row.end_dt, '[)')
                    && tstzrange(NEW.start_dt, NEW.end_dt, '[)')
           ) INTO conflict_exists;
         ELSE
           SELECT EXISTS (
             SELECT 1
               FROM continuous.qualifiers other_row
               JOIN public.qualifier_types other_type
                 ON other_type.qualifier_type_id = other_row.qualifier_type_id
              WHERE other_row.timeseries_id = NEW.timeseries_id
                AND other_row.qualifier_id <> NEW.qualifier_id
                AND other_type.qualifier_type_code IN ('UNK', 'UNS')
                AND tstzrange(other_row.start_dt, other_row.end_dt, '[)')
                    && tstzrange(NEW.start_dt, NEW.end_dt, '[)')
           ) INTO conflict_exists;
         END IF;

         IF conflict_exists THEN
           RAISE EXCEPTION USING
             ERRCODE = '23P01',
             MESSAGE = 'Unknown (UNK) and unspecified (UNS) qualifiers cannot overlap any other qualifier.',
             DETAIL = format(
               'timeseries_id=%s, qualifier_type_code=%s, range=[%s, %s)',
               NEW.timeseries_id, new_code, NEW.start_dt, NEW.end_dt
             );
         END IF;
         RETURN NEW;
       END
       $function$"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION continuous.check_special_qualifier_overlap() OWNER TO postgres"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS lock_qualifier_overlap_check
         ON continuous.qualifiers"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER lock_qualifier_overlap_check
       BEFORE INSERT OR UPDATE ON continuous.qualifiers
       FOR EACH ROW
       EXECUTE FUNCTION continuous.lock_qualifier_overlap_check()"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS check_special_qualifier_overlap
         ON continuous.qualifiers"
    )
    DBI::dbExecute(
      con,
      "CREATE CONSTRAINT TRIGGER check_special_qualifier_overlap
       AFTER INSERT OR UPDATE ON continuous.qualifiers
       DEFERRABLE INITIALLY DEFERRED
       FOR EACH ROW
       EXECUTE FUNCTION continuous.check_special_qualifier_overlap()"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.check_special_qualifier_overlap() IS
       'Deferred cross-type invariant: UNS and UNK qualifier ranges cannot overlap any other qualifier range for the same timeseries.'"
    )

    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = $1
       WHERE item = 'Last patch number'",
      params = list(as.character(patch_number))
    )
    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = $1
       WHERE item = 'AquaCache R package used for last patch'",
      params = list(as.character(utils::packageVersion('AquaCache')))
    )
    DBI::dbExecute(con, "COMMIT")
    active <- FALSE
    message(
      "Patch 70 applied: UNS/UNK qualifier ranges may no longer overlap any other qualifier type."
    )
  },
  error = function(e) {
    if (isTRUE(active)) {
      try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE)
    }
    stop(e)
  }
)
