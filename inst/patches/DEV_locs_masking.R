# DEV_locs_masking.R
#
# Combined work-in-progress development patch copied from the public_locs branch
# public-location masking patch_48.R and patch_49.R. This file intentionally
# lives outside the numbered patch sequence so dev.g can keep its current
# patch_48.R and patch_49.R without merge conflicts.
#
# Set DEV_locs_masking_dry_run <- TRUE before sourcing to exercise the existing
# rollback behavior. In normal use, this script applies the public location
# representation layer first, then updates public.get_csw_layer(). It does not
# update information.version_info.

# DEV_locs_masking section 1: public location representations
#
# Adds database-side support for stable public-safe location geometry. Raw
# locations, samples, and results keep their existing semantics, while new views
# expose coordinates only when the querying role is allowed to see exact
# locations or when a non-exact public geometry is available.

# Initial checks ##############################################################
check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (!check$session_user %in% c("postgres", "admin")) {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres or admin user to make this work."
  )
}

message(
  "Working on patch 48: adding public-safe location geometry and views. Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state."
)

if (dbTransCheck(con)) {
  stop(
    "A transaction is already in progress. Please commit or rollback the current transaction before applying this patch."
  )
}

dry_run <- exists("DEV_locs_masking_dry_run", inherits = TRUE) &&
  isTRUE(get("DEV_locs_masking_dry_run", inherits = TRUE))

message("Starting transaction...")
active <- dbTransBegin(con)
DBI::dbExecute(
  con,
  "SET LOCAL search_path = public, discrete, continuous, spatial, pg_catalog"
)

tryCatch(
  {
    missing_relations <- DBI::dbGetQuery(
      con,
      "WITH required(relation_name) AS (
         VALUES
           ('public.locations'),
           ('public.location_types'),
           ('public.datum_conversions'),
           ('public.datum_list'),
           ('public.media_types'),
           ('public.parameters'),
           ('public.matrix_states'),
           ('public.projects'),
           ('public.networks'),
           ('public.locations_projects'),
           ('public.locations_networks'),
           ('public.sub_locations'),
           ('discrete.samples'),
           ('discrete.results'),
           ('discrete.collection_methods'),
           ('discrete.laboratories'),
           ('discrete.protocols_methods'),
           ('discrete.result_conditions'),
           ('discrete.result_speciations'),
           ('discrete.result_types'),
           ('discrete.result_value_types'),
           ('discrete.sample_fractions'),
           ('discrete.sample_types'),
           ('public.approval_types'),
           ('public.grade_types'),
           ('public.organizations'),
           ('public.qualifier_types')
       )
       SELECT relation_name
       FROM required
       WHERE to_regclass(relation_name) IS NULL
       ORDER BY relation_name"
    )

    if (nrow(missing_relations) > 0) {
      stop(
        "This patch requires the following relations to already exist: ",
        paste(missing_relations$relation_name, collapse = ", ")
      )
    }

    function_check <- DBI::dbGetQuery(
      con,
      "SELECT
         to_regprocedure('public.update_modified()') IS NOT NULL AS has_update_modified,
         to_regprocedure('public.validate_share_with()') IS NOT NULL AS has_validate_share_with,
         to_regprocedure('public.get_parameter_unit_name(integer, integer)') IS NOT NULL AS has_get_parameter_unit_name,
         to_regtype('geometry') IS NOT NULL AS has_postgis_geometry"
    )

    if (
      !isTRUE(function_check$has_update_modified[[1]]) ||
        !isTRUE(function_check$has_validate_share_with[[1]]) ||
        !isTRUE(function_check$has_get_parameter_unit_name[[1]]) ||
        !isTRUE(function_check$has_postgis_geometry[[1]])
    ) {
      stop(
        "This patch requires public.update_modified(), public.validate_share_with(), public.get_parameter_unit_name(integer, integer), and the PostGIS geometry type to already exist."
      )
    }

    existing_roles <- DBI::dbGetQuery(
      con,
      "SELECT rolname FROM pg_roles"
    )$rolname

    view_select_roles <- intersect(
      c(
        "admin",
        "public_reader",
        "yg_reader_group",
        "yg_reader",
        "yg_editor_group",
        "yg_editor",
        "cyfn_editor_group",
        "cyfn_editor",
        "tkc_group",
        "tkc_editor"
      ),
      existing_roles
    )

    write_roles <- intersect(
      c("admin"),
      existing_roles
    )

    coordinate_restricted_roles <- intersect(
      c("public_reader"),
      existing_roles
    )

    q_ident <- function(x) {
      as.character(DBI::dbQuoteIdentifier(con, x))
    }

    grant_table_privileges <- function(object_name, privileges, roles) {
      if (length(roles) == 0) {
        return(invisible(NULL))
      }

      for (role_name in roles) {
        DBI::dbExecute(
          con,
          sprintf(
            "GRANT %s ON TABLE %s TO %s",
            privileges,
            object_name,
            q_ident(role_name)
          )
        )
      }
    }

    grant_sequence_privileges <- function(object_name, privileges, roles) {
      if (length(roles) == 0) {
        return(invisible(NULL))
      }

      for (role_name in roles) {
        DBI::dbExecute(
          con,
          sprintf(
            "GRANT %s ON SEQUENCE %s TO %s",
            privileges,
            object_name,
            q_ident(role_name)
          )
        )
      }
    }

    grant_function_privileges <- function(function_name, privileges, roles) {
      if (length(roles) == 0) {
        return(invisible(NULL))
      }

      for (role_name in roles) {
        DBI::dbExecute(
          con,
          sprintf(
            "GRANT %s ON FUNCTION %s TO %s",
            privileges,
            function_name,
            q_ident(role_name)
          )
        )
      }
    }

    grant_location_columns_except <- function(excluded_columns, roles) {
      if (length(roles) == 0) {
        return(invisible(NULL))
      }

      excluded_sql <- paste(
        as.character(DBI::dbQuoteString(con, excluded_columns)),
        collapse = ", "
      )

      column_names <- DBI::dbGetQuery(
        con,
        sprintf(
          "SELECT column_name
         FROM information_schema.columns
         WHERE table_schema = 'public'
           AND table_name = 'locations'
           AND column_name NOT IN (%s)
         ORDER BY ordinal_position",
          excluded_sql
        )
      )$column_name

      if (length(column_names) == 0) {
        stop(
          "No public.locations columns available for column-level SELECT grant."
        )
      }

      column_sql <- paste(q_ident(column_names), collapse = ", ")
      for (role_name in roles) {
        DBI::dbExecute(
          con,
          sprintf(
            "GRANT SELECT (%s) ON TABLE public.locations TO %s",
            column_sql,
            q_ident(role_name)
          )
        )
      }
    }

    message("Adding exact-coordinate sharing control to public.locations...")
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations
       ADD COLUMN IF NOT EXISTS exact_share_with text[]"
    )
    DBI::dbExecute(
      con,
      "UPDATE public.locations
       SET exact_share_with = ARRAY['public_reader']::text[]
       WHERE exact_share_with IS NULL"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations
       ALTER COLUMN exact_share_with SET DEFAULT ARRAY['public_reader']::text[]"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations
       ALTER COLUMN exact_share_with SET NOT NULL"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.locations.exact_share_with IS
       'Database roles allowed to see exact latitude/longitude for this location in public-safe views. Row visibility remains controlled by share_with and RLS.'"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS locations_exact_share_with_gin_idx
       ON public.locations USING GIN (exact_share_with)"
    )

    if (length(coordinate_restricted_roles) > 0) {
      message(
        "Restricting raw latitude/longitude access on public.locations for public reader roles..."
      )
      for (role_name in coordinate_restricted_roles) {
        DBI::dbExecute(
          con,
          sprintf(
            "REVOKE SELECT ON TABLE public.locations FROM %s",
            q_ident(role_name)
          )
        )
      }
      grant_location_columns_except(
        excluded_columns = c("latitude", "longitude"),
        roles = coordinate_restricted_roles
      )
    }

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION public.validate_exact_share_with()
       RETURNS trigger
       LANGUAGE plpgsql
       AS $function$
       DECLARE
         bad text;
       BEGIN
         IF NEW.exact_share_with IS NULL THEN
           NEW.exact_share_with := ARRAY['public_reader']::text[];
         END IF;

         NEW.exact_share_with := (
           SELECT array_agg(DISTINCT role_name ORDER BY role_name)
           FROM unnest(NEW.exact_share_with) AS roles(role_name)
           WHERE role_name IS NOT NULL
         );

         IF NEW.exact_share_with IS NULL OR array_length(NEW.exact_share_with, 1) IS NULL THEN
           RAISE EXCEPTION 'exact_share_with cannot be empty.';
         END IF;

         IF 'public_reader' = ANY(NEW.exact_share_with)
            AND array_length(NEW.exact_share_with, 1) > 1 THEN
           RAISE EXCEPTION 'If public_reader is present, it must be the only exact_share_with value.';
         END IF;

         SELECT role_name
         INTO bad
         FROM unnest(NEW.exact_share_with) AS roles(role_name)
         WHERE NOT EXISTS (
           SELECT 1
           FROM pg_roles r
           WHERE r.rolname = role_name
             AND (
               r.rolname = 'public_reader'
               OR (
                 r.rolcanlogin = false
                 AND r.rolname <> 'public'
                 AND r.rolname !~ '^pg_'
               )
             )
         )
         LIMIT 1;

         IF bad IS NOT NULL THEN
           RAISE EXCEPTION 'Invalid value in exact_share_with: % (allowed: group roles or public_reader)', bad;
         END IF;

         RETURN NEW;
       END;
       $function$"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS validate_exact_share_with_trigger ON public.locations"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER validate_exact_share_with_trigger
       BEFORE INSERT OR UPDATE OF exact_share_with ON public.locations
       FOR EACH ROW
       EXECUTE FUNCTION public.validate_exact_share_with()"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION public.location_exact_visible(
         p_exact_share_with text[]
       )
       RETURNS boolean
       LANGUAGE sql
       STABLE
       SECURITY INVOKER
       SET search_path = pg_catalog, public
       AS $function$
         SELECT COALESCE(
           p_exact_share_with @> ARRAY['public_reader']::text[]
           OR EXISTS (
             SELECT 1
             FROM unnest(p_exact_share_with) AS roles(role_name)
             JOIN pg_roles r
               ON r.rolname = roles.role_name
             WHERE pg_has_role(current_user, r.rolname, 'member')
           ),
           false
         )
       $function$"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION public.location_exact_visible(text[]) IS
       'Returns true when the current database user can see exact location coordinates under a locations.exact_share_with role array.'"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION public.location_masked_point(
         p_latitude numeric,
         p_longitude numeric,
         p_min_offset_m numeric,
         p_max_offset_m numeric,
         p_seed text
       )
       RETURNS geometry(Point, 4326)
       LANGUAGE plpgsql
       IMMUTABLE
       STRICT
       SET search_path = pg_catalog, public, spatial
       AS $function$
       DECLARE
         bearing_bytes bytea;
         distance_bytes bytea;
         bearing_u double precision;
         distance_u double precision;
         distance_m double precision;
         bearing_rad double precision;
       BEGIN
         IF p_latitude < -90 OR p_latitude > 90 THEN
           RAISE EXCEPTION 'Latitude must be between -90 and 90.';
         END IF;
         IF p_longitude < -180 OR p_longitude > 180 THEN
           RAISE EXCEPTION 'Longitude must be between -180 and 180.';
         END IF;
         IF p_min_offset_m < 0 OR p_max_offset_m < 0 THEN
           RAISE EXCEPTION 'Mask offsets must be non-negative.';
         END IF;
         IF p_max_offset_m < p_min_offset_m THEN
           RAISE EXCEPTION 'max_offset_m must be greater than or equal to min_offset_m.';
         END IF;

         bearing_bytes := decode(substr(md5(p_seed || ':bearing'), 1, 8), 'hex');
         distance_bytes := decode(substr(md5(p_seed || ':distance'), 1, 8), 'hex');

         bearing_u := (
           get_byte(bearing_bytes, 0)::bigint * 16777216
           + get_byte(bearing_bytes, 1)::bigint * 65536
           + get_byte(bearing_bytes, 2)::bigint * 256
           + get_byte(bearing_bytes, 3)::bigint
         )::double precision / 4294967295.0;

         distance_u := (
           get_byte(distance_bytes, 0)::bigint * 16777216
           + get_byte(distance_bytes, 1)::bigint * 65536
           + get_byte(distance_bytes, 2)::bigint * 256
           + get_byte(distance_bytes, 3)::bigint
         )::double precision / 4294967295.0;

         distance_m := p_min_offset_m::double precision
           + (p_max_offset_m - p_min_offset_m)::double precision * distance_u;
         bearing_rad := 2.0 * pi() * bearing_u;

         RETURN ST_Project(
           ST_SetSRID(
             ST_MakePoint(p_longitude::double precision, p_latitude::double precision),
             4326
           )::geography,
           distance_m,
           bearing_rad
         )::geometry(Point, 4326);
       END;
       $function$"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION public.location_masked_point(
         numeric,
         numeric,
         numeric,
         numeric,
         text
       ) IS
       'Returns a deterministic masked point in metres from an input latitude/longitude using a caller-provided seed. The function is intended for stable public location masking, not per-query randomization.'"
    )

    message("Creating public location geometry tables...")
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS public.location_public_geometries (
         public_geom_id SERIAL PRIMARY KEY,
         location_id INTEGER NOT NULL REFERENCES public.locations(location_id) ON UPDATE CASCADE ON DELETE CASCADE,
         public_geom_type TEXT NOT NULL,
         public_geom geometry(Geometry, 4326) NOT NULL,
         mask_method TEXT NOT NULL DEFAULT 'exact',
         min_offset_m NUMERIC,
         max_offset_m NUMERIC,
         public_accuracy_m NUMERIC,
         method_version INTEGER NOT NULL DEFAULT 1,
         active BOOLEAN NOT NULL DEFAULT TRUE,
         note TEXT,
         created TIMESTAMPTZ NOT NULL DEFAULT now(),
         created_by TEXT NOT NULL DEFAULT CURRENT_USER,
         modified TIMESTAMPTZ,
         modified_by TEXT,
         CONSTRAINT location_public_geometries_type_check
           CHECK (public_geom_type IN ('exact_point', 'masked_point', 'reporting_polygon')),
         CONSTRAINT location_public_geometries_offset_check
           CHECK (
             (min_offset_m IS NULL OR min_offset_m >= 0)
             AND (max_offset_m IS NULL OR max_offset_m >= 0)
             AND (
               min_offset_m IS NULL
               OR max_offset_m IS NULL
               OR max_offset_m >= min_offset_m
             )
           ),
         CONSTRAINT location_public_geometries_accuracy_check
           CHECK (public_accuracy_m IS NULL OR public_accuracy_m >= 0),
         CONSTRAINT location_public_geometries_geom_shape_check
           CHECK (
             (
               public_geom_type IN ('exact_point', 'masked_point')
               AND GeometryType(public_geom) = 'POINT'
             )
             OR (
               public_geom_type = 'reporting_polygon'
               AND GeometryType(public_geom) IN ('POLYGON', 'MULTIPOLYGON')
             )
           )
       )"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE public.location_public_geometries IS
       'Stable public-safe geometries for locations. exact_point rows expose true coordinates only to roles allowed by locations.exact_share_with; masked_point and reporting_polygon rows provide non-exact public representations.'"
    )
    DBI::dbExecute(
      con,
      "CREATE UNIQUE INDEX IF NOT EXISTS location_public_geometries_one_active_idx
       ON public.location_public_geometries(location_id)
       WHERE active"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS location_public_geometries_location_idx
       ON public.location_public_geometries(location_id)"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS location_public_geometries_geom_gist_idx
       ON public.location_public_geometries USING GIST (public_geom)"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS update_location_public_geometries_modified
       ON public.location_public_geometries"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER update_location_public_geometries_modified
       BEFORE UPDATE ON public.location_public_geometries
       FOR EACH ROW
       EXECUTE FUNCTION public.update_modified()"
    )

    DBI::dbExecute(
      con,
      "INSERT INTO public.location_public_geometries (
         location_id,
         public_geom_type,
         public_geom,
         mask_method,
         public_accuracy_m,
         note
       )
       SELECT
         loc.location_id,
         'exact_point',
         ST_SetSRID(
           ST_MakePoint(loc.longitude::double precision, loc.latitude::double precision),
           4326
         ),
         'exact',
         0,
         'Default exact public geometry created by patch 48.'
       FROM public.locations loc
       WHERE NOT EXISTS (
         SELECT 1
         FROM public.location_public_geometries existing
         WHERE existing.location_id = loc.location_id
           AND existing.active
       )"
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE public.location_public_geometries ENABLE ROW LEVEL SECURITY"
    )
    DBI::dbExecute(
      con,
      "DROP POLICY IF EXISTS rls ON public.location_public_geometries"
    )
    DBI::dbExecute(
      con,
      "CREATE POLICY rls ON public.location_public_geometries
       FOR ALL
       USING (
         EXISTS (
           SELECT 1
           FROM public.locations loc
           WHERE loc.location_id = location_public_geometries.location_id
             AND (
               loc.share_with @> ARRAY['public_reader']::text[]
               OR EXISTS (
                 SELECT 1
                 FROM unnest(loc.share_with) AS roles(role_name)
                 JOIN pg_roles r
                   ON r.rolname = roles.role_name
                 WHERE pg_has_role(current_user, r.rolname, 'member')
               )
             )
             AND (
               location_public_geometries.public_geom_type <> 'exact_point'
               OR public.location_exact_visible(loc.exact_share_with)
             )
         )
       )
       WITH CHECK (
         EXISTS (
           SELECT 1
           FROM public.locations loc
           WHERE loc.location_id = location_public_geometries.location_id
             AND (
               loc.share_with @> ARRAY['public_reader']::text[]
               OR EXISTS (
                 SELECT 1
                 FROM unnest(loc.share_with) AS roles(role_name)
                 JOIN pg_roles r
                   ON r.rolname = roles.role_name
                 WHERE pg_has_role(current_user, r.rolname, 'member')
               )
             )
         )
       )"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS public.location_reporting_areas (
         reporting_area_id SERIAL PRIMARY KEY,
         reporting_location_id INTEGER REFERENCES public.locations(location_id) ON UPDATE CASCADE ON DELETE SET NULL,
         area_code TEXT NOT NULL UNIQUE,
         name TEXT NOT NULL,
         name_fr TEXT,
         description TEXT,
         description_fr TEXT,
         public_geom_id INTEGER UNIQUE REFERENCES public.location_public_geometries(public_geom_id) ON UPDATE CASCADE ON DELETE SET NULL,
         min_locs INTEGER NOT NULL DEFAULT 1,
         max_locs INTEGER,
         n_locs INTEGER,
         assignment_method TEXT,
         method_version INTEGER NOT NULL DEFAULT 1,
         active BOOLEAN NOT NULL DEFAULT TRUE,
         share_with TEXT[] NOT NULL DEFAULT ARRAY['public_reader']::text[],
         created TIMESTAMPTZ NOT NULL DEFAULT now(),
         created_by TEXT NOT NULL DEFAULT CURRENT_USER,
         modified TIMESTAMPTZ,
         modified_by TEXT,
         CONSTRAINT location_reporting_areas_locs_check
           CHECK (
             min_locs >= 1
             AND (max_locs IS NULL OR max_locs >= min_locs)
             AND (n_locs IS NULL OR n_locs >= 0)
           )
       )"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE public.location_reporting_areas IS
       'Metadata for public reporting polygons used to represent one or more exact locations without exposing true coordinates.'"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS location_reporting_areas_share_with_gin_idx
       ON public.location_reporting_areas USING GIN (share_with)"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS validate_share_with_trigger
       ON public.location_reporting_areas"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER validate_share_with_trigger
       BEFORE INSERT OR UPDATE ON public.location_reporting_areas
       FOR EACH ROW
       EXECUTE FUNCTION public.validate_share_with()"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS update_location_reporting_areas_modified
       ON public.location_reporting_areas"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER update_location_reporting_areas_modified
       BEFORE UPDATE ON public.location_reporting_areas
       FOR EACH ROW
       EXECUTE FUNCTION public.update_modified()"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE public.location_reporting_areas ENABLE ROW LEVEL SECURITY"
    )
    DBI::dbExecute(
      con,
      "DROP POLICY IF EXISTS rls ON public.location_reporting_areas"
    )
    DBI::dbExecute(
      con,
      "CREATE POLICY rls ON public.location_reporting_areas
       FOR ALL
       USING (
         share_with @> ARRAY['public_reader']::text[]
         OR EXISTS (
           SELECT 1
           FROM unnest(share_with) AS roles(role_name)
           JOIN pg_roles r
             ON r.rolname = roles.role_name
           WHERE pg_has_role(current_user, r.rolname, 'member')
         )
       )
       WITH CHECK (
         share_with @> ARRAY['public_reader']::text[]
         OR EXISTS (
           SELECT 1
           FROM unnest(share_with) AS roles(role_name)
           JOIN pg_roles r
             ON r.rolname = roles.role_name
           WHERE pg_has_role(current_user, r.rolname, 'member')
         )
       )"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS public.location_reporting_area_members (
         membership_id SERIAL PRIMARY KEY,
         location_id INTEGER NOT NULL REFERENCES public.locations(location_id) ON UPDATE CASCADE ON DELETE CASCADE,
         reporting_area_id INTEGER NOT NULL REFERENCES public.location_reporting_areas(reporting_area_id) ON UPDATE CASCADE ON DELETE CASCADE,
         assignment_method TEXT NOT NULL DEFAULT 'manual',
         active BOOLEAN NOT NULL DEFAULT TRUE,
         created TIMESTAMPTZ NOT NULL DEFAULT now(),
         created_by TEXT NOT NULL DEFAULT CURRENT_USER,
         modified TIMESTAMPTZ,
         modified_by TEXT
       )"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE public.location_reporting_area_members IS
       'Mapping between exact locations and public reporting areas. SELECT is RLS-limited to visible locations and reporting areas so public-safe views can expose reporting area grouping without exposing coordinates.'"
    )
    DBI::dbExecute(
      con,
      "CREATE UNIQUE INDEX IF NOT EXISTS location_reporting_area_members_one_active_idx
       ON public.location_reporting_area_members(location_id)
       WHERE active"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS location_reporting_area_members_area_idx
       ON public.location_reporting_area_members(reporting_area_id)"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS update_location_reporting_area_members_modified
       ON public.location_reporting_area_members"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER update_location_reporting_area_members_modified
       BEFORE UPDATE ON public.location_reporting_area_members
       FOR EACH ROW
       EXECUTE FUNCTION public.update_modified()"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE public.location_reporting_area_members ENABLE ROW LEVEL SECURITY"
    )
    DBI::dbExecute(
      con,
      "DROP POLICY IF EXISTS rls ON public.location_reporting_area_members"
    )
    DBI::dbExecute(
      con,
      "CREATE POLICY rls ON public.location_reporting_area_members
       FOR ALL
       USING (
         EXISTS (
           SELECT 1
           FROM public.locations loc
           WHERE loc.location_id = location_reporting_area_members.location_id
             AND (
               loc.share_with @> ARRAY['public_reader']::text[]
               OR EXISTS (
                 SELECT 1
                 FROM unnest(loc.share_with) AS roles(role_name)
                 JOIN pg_roles r
                   ON r.rolname = roles.role_name
                 WHERE pg_has_role(current_user, r.rolname, 'member')
               )
             )
         )
         AND EXISTS (
           SELECT 1
           FROM public.location_reporting_areas area
           WHERE area.reporting_area_id = location_reporting_area_members.reporting_area_id
             AND area.active
             AND (
               area.share_with @> ARRAY['public_reader']::text[]
               OR EXISTS (
                 SELECT 1
                 FROM unnest(area.share_with) AS roles(role_name)
                 JOIN pg_roles r
                   ON r.rolname = roles.role_name
                 WHERE pg_has_role(current_user, r.rolname, 'member')
               )
             )
         )
       )
       WITH CHECK (
         EXISTS (
           SELECT 1
           FROM public.locations loc
           WHERE loc.location_id = location_reporting_area_members.location_id
             AND (
               loc.share_with @> ARRAY['public_reader']::text[]
               OR EXISTS (
                 SELECT 1
                 FROM unnest(loc.share_with) AS roles(role_name)
                 JOIN pg_roles r
                   ON r.rolname = roles.role_name
                 WHERE pg_has_role(current_user, r.rolname, 'member')
               )
             )
         )
         AND EXISTS (
           SELECT 1
           FROM public.location_reporting_areas area
           WHERE area.reporting_area_id = location_reporting_area_members.reporting_area_id
             AND (
               area.share_with @> ARRAY['public_reader']::text[]
               OR EXISTS (
                 SELECT 1
                 FROM unnest(area.share_with) AS roles(role_name)
                 JOIN pg_roles r
                   ON r.rolname = roles.role_name
                 WHERE pg_has_role(current_user, r.rolname, 'member')
               )
             )
         )
       )"
    )

    message("Creating public-safe location, sample, and result views...")
    DBI::dbExecute(
      con,
      "CREATE OR REPLACE VIEW public.locations_public
       WITH (security_barrier = true)
       AS
       SELECT
         loc.location_id,
         COALESCE(ra.reporting_location_id, loc.location_id) AS public_location_id,
         CASE
           WHEN public.location_exact_visible(loc.exact_share_with) THEN loc.location_id
           ELSE NULL::integer
         END AS exact_location_id,
         ra.reporting_area_id,
         ra.reporting_location_id,
         ra.n_locs AS reporting_area_n_locs,
         loc.location_code,
         loc.name,
         loc.name_fr,
         loc.alias,
         loc.location_type,
         lt.type AS location_type_name,
         lt.type_fr AS location_type_name_fr,
         CASE
           WHEN public.location_exact_visible(loc.exact_share_with) THEN loc.latitude
           WHEN lpg.public_geom_type = 'masked_point' THEN ST_Y(lpg.public_geom)::numeric
           ELSE NULL::numeric
         END AS latitude,
         CASE
           WHEN public.location_exact_visible(loc.exact_share_with) THEN loc.longitude
           WHEN lpg.public_geom_type = 'masked_point' THEN ST_X(lpg.public_geom)::numeric
           ELSE NULL::numeric
         END AS longitude,
         CASE
           WHEN public.location_exact_visible(loc.exact_share_with) THEN
             ST_SetSRID(
               ST_MakePoint(loc.longitude::double precision, loc.latitude::double precision),
               4326
             )
           WHEN lpg.public_geom_type <> 'exact_point' THEN lpg.public_geom
           ELSE NULL::geometry
         END AS public_geom,
         CASE
           WHEN public.location_exact_visible(loc.exact_share_with) THEN 'exact_point'
           WHEN lpg.public_geom_type <> 'exact_point' THEN lpg.public_geom_type
           ELSE NULL
         END AS public_geom_type,
         CASE
           WHEN public.location_exact_visible(loc.exact_share_with) THEN 0::numeric
           WHEN lpg.public_geom_type <> 'exact_point' THEN lpg.public_accuracy_m
           ELSE NULL::numeric
         END AS public_accuracy_m,
         CASE
           WHEN public.location_exact_visible(loc.exact_share_with) THEN 'exact'
           WHEN lpg.public_geom_type <> 'exact_point' THEN lpg.mask_method
           ELSE NULL
         END AS public_geom_method,
         public.location_exact_visible(loc.exact_share_with) AS exact_location_visible,
         loc.note,
         loc.contact,
         loc.install_purpose,
         loc.current_purpose,
         loc.location_images,
         loc.jurisdictional_relevance,
         loc.anthropogenic_influence,
         loc.sentinel_location,
         loc.share_with,
         loc.private_expiry,
         loc.created,
         loc.created_by,
         loc.modified,
         loc.modified_by
       FROM public.locations loc
       LEFT JOIN public.location_types lt
         ON loc.location_type = lt.type_id
       LEFT JOIN public.location_public_geometries lpg
         ON loc.location_id = lpg.location_id
        AND lpg.active
       LEFT JOIN public.location_reporting_area_members lram
         ON loc.location_id = lram.location_id
        AND lram.active
       LEFT JOIN public.location_reporting_areas ra
         ON lram.reporting_area_id = ra.reporting_area_id
        AND ra.active
       WHERE loc.share_with @> ARRAY['public_reader']::text[]
          OR EXISTS (
            SELECT 1
            FROM unnest(loc.share_with) AS roles(role_name)
            JOIN pg_roles r
              ON r.rolname = roles.role_name
            WHERE pg_has_role(current_user, r.rolname, 'member')
          )"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON VIEW public.locations_public IS
       'Public-safe location view. Coordinates are exact only when current_user is allowed by locations.exact_share_with; otherwise a stable masked point or reporting polygon is returned when available. public_location_id can be used by applications to group records under a reporting location. The view enforces share_with visibility directly so public_reader does not need raw latitude/longitude column privileges on public.locations.'"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE VIEW discrete.samples_public
       WITH (security_barrier = true)
       AS
       SELECT
         s.sample_id,
         s.location_id,
         lp.public_location_id,
         lp.exact_location_id,
         lp.reporting_area_id,
         lp.reporting_location_id,
         lp.reporting_area_n_locs,
         lp.location_code,
         lp.name AS location_name,
         lp.alias AS alias_name,
         lp.latitude,
         lp.longitude,
         lp.public_geom,
         lp.public_geom_type,
         lp.public_accuracy_m,
         lp.public_geom_method,
         lp.exact_location_visible,
         dc.conversion_m AS location_elevation,
         loc_projects.projects,
         loc_networks.networks,
         s.sub_location_id,
         sub_loc.sub_location_name,
         CASE
           WHEN lp.exact_location_visible THEN sub_loc.latitude
           ELSE NULL::numeric
         END AS sub_location_latitude,
         CASE
           WHEN lp.exact_location_visible THEN sub_loc.longitude
           ELSE NULL::numeric
         END AS sub_location_longitude,
         s.media_id,
         mt.media_type,
         s.z AS depth_height_m,
         s.datetime,
         s.target_datetime,
         s.collection_method AS collection_method_id,
         cm.collection_method,
         s.sample_type AS sample_type_id,
         st.sample_type,
         s.linked_with AS linked_sample_id,
         s.sample_volume_ml,
         s.purge_volume_l,
         s.purge_time_min,
         s.flow_rate_l_min,
         s.wave_hgt_m,
         s.sample_grade AS sample_grade_id,
         gt.grade_type_code AS sample_grade_code,
         gt.grade_type_description AS sample_grade_description,
         s.sample_approval AS sample_approval_id,
         at.approval_type_code AS sample_approval_code,
         at.approval_type_description AS sample_approval_description,
         s.sample_qualifier AS sample_qualifier_id,
         qt.qualifier_type_code AS sample_qualifier_code,
         qt.qualifier_type_description AS sample_qualifier_description,
         s.owner AS owner_id,
         owner_org.name AS owner_name,
         s.contributor AS contributor_id,
         contributor_org.name AS contributor_name,
         s.comissioning_org AS commissioning_org_id,
         commissioning_org.name AS commissioning_org_name,
         s.sampling_org AS sampling_org_id,
         sampling_org.name AS sampling_org_name,
         s.field_visit_id,
         s.data_sharing_agreement_id,
         s.documents,
         s.import_source,
         s.import_source_id,
         s.no_update,
         s.note,
         s.share_with,
         s.private_expiry,
         s.created,
         s.created_by,
         s.modified,
         s.modified_by
       FROM discrete.samples s
       JOIN public.locations_public lp
         ON s.location_id = lp.location_id
       LEFT JOIN public.sub_locations sub_loc
         ON s.sub_location_id = sub_loc.sub_location_id
       LEFT JOIN public.media_types mt
         ON s.media_id = mt.media_id
       LEFT JOIN discrete.collection_methods cm
         ON s.collection_method = cm.collection_method_id
       LEFT JOIN discrete.sample_types st
         ON s.sample_type = st.sample_type_id
       LEFT JOIN public.grade_types gt
         ON s.sample_grade = gt.grade_type_id
       LEFT JOIN public.approval_types at
         ON s.sample_approval = at.approval_type_id
       LEFT JOIN public.qualifier_types qt
         ON s.sample_qualifier = qt.qualifier_type_id
       LEFT JOIN public.organizations owner_org
         ON s.owner = owner_org.organization_id
       LEFT JOIN public.organizations contributor_org
         ON s.contributor = contributor_org.organization_id
       LEFT JOIN public.organizations commissioning_org
         ON s.comissioning_org = commissioning_org.organization_id
       LEFT JOIN public.organizations sampling_org
         ON s.sampling_org = sampling_org.organization_id
       LEFT JOIN LATERAL (
         SELECT dc2.conversion_m
         FROM public.datum_conversions dc2
         WHERE dc2.location_id = s.location_id
           AND dc2.current IS TRUE
         ORDER BY dc2.conversion_id
         LIMIT 1
       ) dc
         ON TRUE
       LEFT JOIN LATERAL (
         SELECT array_agg(DISTINCT proj.name ORDER BY proj.name) AS projects
         FROM public.locations_projects loc_proj
         JOIN public.projects proj
           ON loc_proj.project_id = proj.project_id
         WHERE loc_proj.location_id = s.location_id
       ) loc_projects
         ON TRUE
       LEFT JOIN LATERAL (
         SELECT array_agg(DISTINCT net.name ORDER BY net.name) AS networks
         FROM public.locations_networks loc_net
         JOIN public.networks net
           ON loc_net.network_id = net.network_id
         WHERE loc_net.location_id = s.location_id
       ) loc_networks
         ON TRUE
       WHERE s.share_with @> ARRAY['public_reader']::text[]
          OR EXISTS (
            SELECT 1
            FROM unnest(s.share_with) AS roles(role_name)
            JOIN pg_roles r
              ON r.rolname = roles.role_name
            WHERE pg_has_role(current_user, r.rolname, 'member')
          )"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON VIEW discrete.samples_public IS
       'Public-safe English-language discrete sample metadata view using public.locations_public for masked or exact location geometry.'"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE VIEW discrete.results_public
       WITH (security_barrier = true)
       AS
       SELECT
         r.result_id,
         r.sample_id,
         sp.location_id,
         sp.public_location_id,
         sp.exact_location_id,
         sp.reporting_area_id,
         sp.reporting_location_id,
         sp.reporting_area_n_locs,
         sp.location_code,
         sp.location_name,
         sp.alias_name,
         sp.latitude,
         sp.longitude,
         sp.public_geom,
         sp.public_geom_type,
         sp.public_accuracy_m,
         sp.public_geom_method,
         sp.exact_location_visible,
         sp.location_elevation,
         sp.projects,
         sp.networks,
         sp.sub_location_id,
         sp.sub_location_name,
         sp.sub_location_latitude,
         sp.sub_location_longitude,
         sp.media_id,
         sp.media_type,
         sp.depth_height_m,
         sp.datetime,
         sp.target_datetime,
         sp.collection_method_id,
         sp.collection_method,
         sp.sample_type_id,
         sp.sample_type,
         sp.sample_grade_id,
         sp.sample_grade_code,
         sp.sample_grade_description,
         sp.sample_approval_id,
         sp.sample_approval_code,
         sp.sample_approval_description,
         sp.sample_qualifier_id,
         sp.sample_qualifier_code,
         sp.sample_qualifier_description,
         sp.owner_id AS sample_owner_id,
         sp.owner_name AS sample_owner_name,
         sp.contributor_id AS sample_contributor_id,
         sp.contributor_name AS sample_contributor_name,
         sp.import_source AS sample_import_source,
         sp.import_source_id AS sample_import_source_id,
         sp.note AS sample_note,
         r.parameter_id,
         p.param_name AS parameter_name,
         p.cas_number,
         r.matrix_state_id,
         ms.matrix_state_code,
         ms.matrix_state_name,
         public.get_parameter_unit_name(
           r.parameter_id,
           r.matrix_state_id
         ) AS units,
         r.sample_fraction_id,
         sf.sample_fraction,
         r.result_type AS result_type_id,
         rt.result_type,
         r.result,
         r.result_condition AS result_condition_id,
         rc.result_condition,
         r.result_condition_value,
         r.result_value_type AS result_value_type_id,
         rvt.result_value_type,
         r.result_speciation_id,
         rs.result_speciation,
         r.protocol_method AS protocol_method_id,
         pm.protocol_name AS protocol_method,
         pm.protocol_description,
         pm.url AS protocol_url,
         r.laboratory AS lab_id,
         lab.lab_name,
         r.analysis_datetime,
         sp.no_update AS sample_no_update,
         r.no_update AS result_no_update,
         sp.share_with AS sample_share_with,
         r.share_with AS result_share_with,
         sp.private_expiry AS sample_private_expiry,
         r.private_expiry AS result_private_expiry,
         r.created,
         r.created_by,
         r.modified,
         r.modified_by
       FROM discrete.results r
       JOIN discrete.samples_public sp
         ON r.sample_id = sp.sample_id
       LEFT JOIN public.parameters p
         ON r.parameter_id = p.parameter_id
       LEFT JOIN public.matrix_states ms
         ON r.matrix_state_id = ms.matrix_state_id
       LEFT JOIN discrete.sample_fractions sf
         ON r.sample_fraction_id = sf.sample_fraction_id
       LEFT JOIN discrete.result_types rt
         ON r.result_type = rt.result_type_id
       LEFT JOIN discrete.result_conditions rc
         ON r.result_condition = rc.result_condition_id
       LEFT JOIN discrete.result_value_types rvt
         ON r.result_value_type = rvt.result_value_type_id
       LEFT JOIN discrete.result_speciations rs
         ON r.result_speciation_id = rs.result_speciation_id
       LEFT JOIN discrete.protocols_methods pm
         ON r.protocol_method = pm.protocol_id
       LEFT JOIN discrete.laboratories lab
         ON r.laboratory = lab.lab_id
       WHERE r.share_with @> ARRAY['public_reader']::text[]
          OR EXISTS (
            SELECT 1
            FROM unnest(r.share_with) AS roles(role_name)
            JOIN pg_roles pg_role
              ON pg_role.rolname = roles.role_name
            WHERE pg_has_role(current_user, pg_role.rolname, 'member')
          )"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON VIEW discrete.results_public IS
       'Public-safe English-language discrete result metadata view using public-safe sample and location geometry.'"
    )

    public_tables <- c(
      "public.location_public_geometries",
      "public.location_reporting_areas",
      "public.location_reporting_area_members",
      "public.locations_public",
      "discrete.samples_public",
      "discrete.results_public"
    )
    for (table_name in public_tables) {
      grant_table_privileges(table_name, "SELECT", view_select_roles)
    }

    admin_tables <- c(
      "public.location_public_geometries",
      "public.location_reporting_areas",
      "public.location_reporting_area_members"
    )
    for (table_name in admin_tables) {
      grant_table_privileges(
        table_name,
        "SELECT, INSERT, UPDATE, DELETE",
        write_roles
      )
    }

    grant_sequence_privileges(
      "public.location_public_geometries_public_geom_id_seq",
      "USAGE, SELECT",
      write_roles
    )
    grant_sequence_privileges(
      "public.location_reporting_areas_reporting_area_id_seq",
      "USAGE, SELECT",
      write_roles
    )
    grant_sequence_privileges(
      "public.location_reporting_area_members_membership_id_seq",
      "USAGE, SELECT",
      write_roles
    )
    grant_function_privileges(
      "public.location_exact_visible(text[])",
      "EXECUTE",
      view_select_roles
    )
    grant_function_privileges(
      "public.location_masked_point(numeric,numeric,numeric,numeric,text)",
      "EXECUTE",
      intersect(c("admin"), existing_roles)
    )

    message("Checking public location representation objects...")
    verification <- DBI::dbGetQuery(
      con,
      "SELECT
         to_regclass('public.location_public_geometries') IS NOT NULL AS has_public_geometries,
         to_regclass('public.location_reporting_areas') IS NOT NULL AS has_reporting_areas,
         to_regclass('public.location_reporting_area_members') IS NOT NULL AS has_reporting_members,
         to_regclass('public.locations_public') IS NOT NULL AS has_locations_public,
         to_regclass('discrete.samples_public') IS NOT NULL AS has_samples_public,
         to_regclass('discrete.results_public') IS NOT NULL AS has_results_public,
         to_regprocedure('public.location_exact_visible(text[])') IS NOT NULL AS has_exact_visible,
         to_regprocedure('public.location_masked_point(numeric,numeric,numeric,numeric,text)') IS NOT NULL AS has_masked_point"
    )

    if (!all(vapply(verification, isTRUE, logical(1)))) {
      stop(
        "Patch 48 verification failed: one or more expected objects were not created."
      )
    }

    backfill_check <- DBI::dbGetQuery(
      con,
      "SELECT count(*)::integer AS missing_count
       FROM public.locations loc
       WHERE NOT EXISTS (
         SELECT 1
         FROM public.location_public_geometries lpg
         WHERE lpg.location_id = loc.location_id
           AND lpg.active
       )"
    )

    if (backfill_check$missing_count[[1]] > 0) {
      stop(
        "Patch 48 verification failed: ",
        backfill_check$missing_count[[1]],
        " locations do not have an active public geometry."
      )
    }

    if ("public_reader" %in% existing_roles) {
      restrictive_exact_role <- DBI::dbGetQuery(
        con,
        "SELECT r.rolname
         FROM pg_roles r
         WHERE r.rolcanlogin = false
           AND r.rolname <> 'public'
           AND r.rolname !~ '^pg_'
           AND NOT pg_has_role('public_reader', r.rolname, 'member')
         ORDER BY r.rolname
         LIMIT 1"
      )
      public_location_target <- DBI::dbGetQuery(
        con,
        "SELECT location_id
         FROM public.locations
         WHERE share_with @> ARRAY['public_reader']::text[]
         ORDER BY location_id
         LIMIT 1"
      )

      if (
        nrow(restrictive_exact_role) > 0 && nrow(public_location_target) > 0
      ) {
        DBI::dbExecute(con, "SAVEPOINT restrictive_exact_visibility_check")
        DBI::dbExecute(
          con,
          "UPDATE public.locations
           SET exact_share_with = ARRAY[$1]::text[]
           WHERE location_id = $2",
          params = list(
            restrictive_exact_role$rolname[[1]],
            public_location_target$location_id[[1]]
          )
        )

        DBI::dbExecute(con, "SET LOCAL ROLE public_reader")
        restrictive_exact_check <- DBI::dbGetQuery(
          con,
          "SELECT count(*)::integer AS bad_count
           FROM public.locations_public
           WHERE location_id = $1
             AND (
               exact_location_visible
               OR public_geom_type = 'exact_point'
               OR latitude IS NOT NULL
               OR longitude IS NOT NULL
             )",
          params = list(public_location_target$location_id[[1]])
        )
        DBI::dbExecute(con, "RESET ROLE")
        DBI::dbExecute(
          con,
          "ROLLBACK TO SAVEPOINT restrictive_exact_visibility_check"
        )
        DBI::dbExecute(
          con,
          "RELEASE SAVEPOINT restrictive_exact_visibility_check"
        )

        if (restrictive_exact_check$bad_count[[1]] > 0) {
          stop(
            "Patch 48 public visibility check failed: public_reader can see exact coordinates for an exact_share_with-restricted location."
          )
        }
      }

      DBI::dbExecute(con, "SET LOCAL ROLE public_reader")

      public_checks <- DBI::dbGetQuery(
        con,
        "SELECT check_name, bad_count
         FROM (
           SELECT
             'exact geometry hidden when exact_share_with is restrictive' AS check_name,
             count(*)::integer AS bad_count
           FROM public.locations_public lp
           WHERE NOT lp.exact_location_visible
             AND lp.public_geom_type = 'exact_point'

           UNION ALL

           SELECT
             'location public view rows have public_location_id' AS check_name,
             count(*)::integer AS bad_count
           FROM public.locations_public lp
           WHERE lp.public_location_id IS NULL

           UNION ALL

           SELECT
             'sample public view rows have public_location_id' AS check_name,
             count(*)::integer AS bad_count
           FROM discrete.samples_public sp
           WHERE sp.public_location_id IS NULL

           UNION ALL

           SELECT
             'sample public view rows have location rows' AS check_name,
             count(*)::integer AS bad_count
           FROM discrete.samples_public sp
           LEFT JOIN public.locations_public lp
             ON lp.location_id = sp.location_id
           WHERE lp.location_id IS NULL

           UNION ALL

           SELECT
             'result public view rows have sample rows' AS check_name,
             count(*)::integer AS bad_count
           FROM discrete.results_public rp
           LEFT JOIN discrete.samples_public sp
             ON sp.sample_id = rp.sample_id
           WHERE sp.sample_id IS NULL
         ) checks
         WHERE bad_count > 0"
      )

      if (nrow(public_checks) > 0) {
        stop(
          "Patch 48 public visibility check failed: ",
          paste(
            paste0(public_checks$check_name, "=", public_checks$bad_count),
            collapse = ", "
          )
        )
      }

      if ("public_reader" %in% coordinate_restricted_roles) {
        raw_latitude_denied <- FALSE

        DBI::dbExecute(con, "SAVEPOINT raw_location_coordinate_privilege_check")
        raw_latitude_denied <- tryCatch(
          {
            DBI::dbGetQuery(
              con,
              "SELECT latitude FROM public.locations LIMIT 1"
            )
            FALSE
          },
          error = function(e) {
            TRUE
          }
        )
        DBI::dbExecute(
          con,
          "ROLLBACK TO SAVEPOINT raw_location_coordinate_privilege_check"
        )
        DBI::dbExecute(
          con,
          "RELEASE SAVEPOINT raw_location_coordinate_privilege_check"
        )

        if (!isTRUE(raw_latitude_denied)) {
          stop(
            "Patch 48 public visibility check failed: public_reader can still query public.locations.latitude directly."
          )
        }

        DBI::dbGetQuery(con, "SELECT location_id FROM public.locations LIMIT 1")
        DBI::dbGetQuery(
          con,
          "SELECT latitude, longitude FROM public.locations_public LIMIT 1"
        )
      }

      DBI::dbExecute(con, "RESET ROLE")
    }

    if (dry_run) {
      DBI::dbExecute(con, "ROLLBACK;")
      message(
        "DEV_locs_masking location representation dry run completed successfully. The transaction was rolled back, so the database was not changed."
      )
    } else {
      DBI::dbExecute(con, "COMMIT;")
      message(
        "DEV_locs_masking location representation section applied successfully. Public location representations and public-safe discrete views are available."
      )
    }
  },
  error = function(e) {
    message("Error detected. Rolling back transaction...")
    try(DBI::dbExecute(con, "ROLLBACK;"), silent = TRUE)
    stop(
      "Patch 48 failed and the database has been rolled back to its previous state. ",
      e$message
    )
  }
)

# -----------------------------------------------------------------------------

message("Starting DEV_locs_masking section 2: update public.get_csw_layer() for public-safe location geometry.")

dry_run <- exists("DEV_locs_masking_dry_run", inherits = TRUE) &&
  isTRUE(get("DEV_locs_masking_dry_run", inherits = TRUE))

if (!exists("con") || !DBI::dbIsValid(con)) {
  con <- AquaConnect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
}

DBI::dbExecute(con, "BEGIN;")

tryCatch(
  {
    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION public.get_csw_layer()
       RETURNS TABLE(
         location text,
         station_name text,
         station_name_fr text,
         latitude numeric,
         longitude numeric,
         type text,
         owner_name text,
         owner_name_fr text,
         timeseries_id integer,
         parameter_id integer,
         param_name text,
         param_name_fr text,
         param_units text,
         date date,
         value numeric,
         percent_historic_range numeric,
         mean numeric,
         min numeric,
         max numeric,
         doy_count integer,
         drainage_area_km2 numeric,
         datum_name_en text,
         datum_name_fr text
       )
       LANGUAGE sql
       AS $function$
       WITH core AS (
         SELECT
           l.location_id,
           l.public_location_id,
           l.location_code,
           l.name,
           l.name_fr,
           COALESCE(
             l.latitude,
             ST_Y(ST_PointOnSurface(l.public_geom))::numeric
           ) AS latitude,
           COALESCE(
             l.longitude,
             ST_X(ST_PointOnSurface(l.public_geom))::numeric
           ) AS longitude,
           l.location_type_name AS type,
           t.timeseries_id,
           t.parameter_id,
           t.sub_location_id,
           p.param_name,
           p.param_name_fr,
           public.get_parameter_unit_name(
             t.parameter_id,
             t.matrix_state_id
           ) AS param_units,
           mcd.date,
           mcd.value,
           mcd.percent_historic_range,
           mcd.mean,
           mcd.min,
           mcd.max,
           mcd.doy_count
         FROM public.locations_public l
         JOIN continuous.timeseries t
           ON t.location_id = l.location_id
         JOIN public.parameters p
           ON p.parameter_id = t.parameter_id
         JOIN continuous.measurements_calculated_daily mcd
           ON mcd.timeseries_id = t.timeseries_id
         WHERE
           l.location_type IN (1, 2, 16)
           AND l.jurisdictional_relevance IS TRUE
           AND p.parameter_id IN (1150, 1165, 21, 1220)
           AND mcd.date >= NOW() - INTERVAL '30 days'
           AND COALESCE(
             l.latitude,
             ST_Y(ST_PointOnSurface(l.public_geom))::numeric
           ) IS NOT NULL
           AND COALESCE(
             l.longitude,
             ST_X(ST_PointOnSurface(l.public_geom))::numeric
           ) IS NOT NULL
       ),
       core_locs AS (
         SELECT DISTINCT
           c.location_id,
           c.location_code
         FROM core c
       ),
       loc_owner AS (
         SELECT DISTINCT ON (t2.location_id)
           t2.location_id,
           org.name AS owner_name,
           org.name_fr AS owner_name_fr
         FROM continuous.timeseries t2
         JOIN core_locs cl
           ON cl.location_id = t2.location_id
         JOIN continuous.owners o
           ON o.timeseries_id = t2.timeseries_id
         JOIN public.organizations org
           ON org.organization_id = o.organization_id
         ORDER BY
           t2.location_id,
           o.start_dt DESC,
           o.end_dt DESC,
           o.owner_id DESC
       ),
       datum_current AS (
         SELECT DISTINCT ON (dc.location_id)
           dc.location_id,
           dc.conversion_m,
           dl.datum_name_en,
           dl.datum_name_fr
         FROM public.datum_conversions dc
         JOIN core_locs cl
           ON cl.location_id = dc.location_id
         LEFT JOIN public.datum_list dl
           ON dl.datum_id = dc.datum_id_to
         WHERE dc.current IS TRUE
         ORDER BY
           dc.location_id,
           dc.modified DESC NULLS LAST,
           dc.conversion_id DESC
       ),
       drainage AS (
         SELECT
           v.feature_name,
           (ST_Area(v.geom::geography) / 1000000)::numeric AS drainage_area_km2
         FROM spatial.vectors v
         JOIN core_locs cl
           ON cl.location_code = v.feature_name
         WHERE v.layer_name = 'Drainage basins'
       )
       SELECT
         c.location_code AS location,
         CASE
           WHEN sl.sub_location_name IS NOT NULL
             THEN CONCAT(c.name, ' - ', sl.sub_location_name)
           ELSE c.name
         END AS station_name,
         CASE
           WHEN sl.sub_location_name_fr IS NOT NULL
             THEN CONCAT(c.name_fr, ' - ', sl.sub_location_name_fr)
           ELSE c.name_fr
         END AS station_name_fr,
         c.latitude,
         c.longitude,
         c.type,
         lo.owner_name,
         lo.owner_name_fr,
         c.timeseries_id,
         c.parameter_id,
         c.param_name,
         c.param_name_fr,
         c.param_units,
         c.date,
         CASE
           WHEN c.param_name = 'water level'
             THEN c.value + COALESCE(dc.conversion_m, 0)
           ELSE c.value
         END AS value,
         c.percent_historic_range,
         CASE
           WHEN c.param_name = 'water level'
             THEN c.mean + COALESCE(dc.conversion_m, 0)
           ELSE c.mean
         END AS mean,
         CASE
           WHEN c.param_name = 'water level'
             THEN c.min + COALESCE(dc.conversion_m, 0)
           ELSE c.min
         END AS min,
         CASE
           WHEN c.param_name = 'water level'
             THEN c.max + COALESCE(dc.conversion_m, 0)
           ELSE c.max
         END AS max,
         c.doy_count,
         dr.drainage_area_km2,
         CASE
           WHEN c.param_name = 'water level'
             THEN dc.datum_name_en
           ELSE NULL
         END AS datum_name_en,
         CASE
           WHEN c.param_name = 'water level'
             THEN dc.datum_name_fr
           ELSE NULL
         END AS datum_name_fr
       FROM core c
       LEFT JOIN public.sub_locations sl
         ON sl.sub_location_id = c.sub_location_id
       LEFT JOIN loc_owner lo
         ON lo.location_id = c.location_id
       LEFT JOIN datum_current dc
         ON dc.location_id = c.location_id
       LEFT JOIN drainage dr
         ON dr.feature_name = c.location_code
       ORDER BY c.location_code, c.param_name, c.date;
       $function$"
    )

    check <- DBI::dbGetQuery(
      con,
      "SELECT to_regprocedure('public.get_csw_layer()') IS NOT NULL AS ok"
    )
    if (!isTRUE(check$ok[[1]])) {
      stop("public.get_csw_layer() was not created.")
    }

    if (dry_run) {
      DBI::dbExecute(con, "ROLLBACK;")
      message("DEV_locs_masking CSW layer dry run completed successfully and was rolled back.")
    } else {
      DBI::dbExecute(con, "COMMIT;")
      message("DEV_locs_masking CSW layer section applied successfully.")
    }
  },
  error = function(e) {
    DBI::dbExecute(con, "ROLLBACK;")
    stop("Patch 49 failed: ", e$message)
  }
)
