-- Resolve whole-document sharing without exposing hidden sample records.
-- Actual document updates remain in the invoking user's transaction/context.
DO $owner$
BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'admin' AND (rolsuper OR rolbypassrls)) THEN
    RAISE EXCEPTION 'Complete document sharing requires admin to have BYPASSRLS.';
  END IF;
END;
$owner$;

-- Grant the owner the columns needed for reads and row locks, including on
-- restored databases where admin does not already own these tables.
GRANT USAGE ON SCHEMA files, discrete, public TO admin;
GRANT SELECT(document_id, share_with), UPDATE(share_with) ON files.documents TO admin;
GRANT SELECT(sample_id, document_id), UPDATE(document_id) ON discrete.sample_documents TO admin;
GRANT SELECT(sample_id, location_id, share_with), UPDATE(share_with) ON discrete.samples TO admin;
GRANT SELECT(location_id, share_with), UPDATE(share_with) ON public.locations TO admin;

CREATE OR REPLACE FUNCTION discrete.get_document_sharing_groups(_document_ids integer[])
RETURNS TABLE(document_id integer, allowed_groups text[])
LANGUAGE plpgsql
VOLATILE
SECURITY DEFINER
SET search_path = pg_catalog, pg_temp
SET row_security = off
AS $function$
DECLARE
  -- SECURITY DEFINER changes current_user. Honor an explicit SET ROLE, too,
  -- rather than accidentally authorizing its more privileged session_user.
  caller_name name := CASE current_setting('role')
    WHEN 'none' THEN session_user ELSE current_setting('role')::name END;
  caller_bypass boolean;
  requested_ids integer[];
  locked_count integer;
  decisions jsonb;
BEGIN
  IF EXISTS (SELECT 1 FROM unnest(_document_ids) id WHERE id IS NULL OR id <= 0) THEN
    RAISE EXCEPTION 'Document IDs must be positive integers.' USING ERRCODE = '22023';
  END IF;
  SELECT array_agg(DISTINCT id ORDER BY id) INTO requested_ids
  FROM unnest(_document_ids) id;
  IF requested_ids IS NULL THEN RETURN; END IF;

  IF NOT has_column_privilege(caller_name, 'files.documents', 'document_id', 'SELECT')
     OR NOT has_column_privilege(caller_name, 'files.documents', 'share_with', 'SELECT')
     OR NOT has_column_privilege(caller_name, 'files.documents', 'share_with', 'UPDATE') THEN
    RAISE EXCEPTION 'A linked document is unavailable or cannot be modified.'
      USING ERRCODE = '42501';
  END IF;
  SELECT r.rolsuper OR r.rolbypassrls INTO caller_bypass
  FROM pg_roles r WHERE r.rolname = caller_name;

  -- These locks also block FK checks for new document links until commit.
  PERFORM d.document_id FROM files.documents d
  WHERE d.document_id = ANY(requested_ids)
    AND (caller_bypass OR 'public_reader' = ANY(d.share_with) OR EXISTS (
      SELECT 1 FROM pg_roles r WHERE r.rolname = ANY(d.share_with)
        AND pg_has_role(caller_name, r.oid, 'MEMBER')
    ))
  ORDER BY d.document_id FOR UPDATE;
  GET DIAGNOSTICS locked_count = ROW_COUNT;
  IF locked_count <> cardinality(requested_ids) OR EXISTS (
    SELECT 1 FROM files.documents d
    WHERE d.document_id = ANY(requested_ids)
      AND NOT (caller_bypass OR 'public_reader' = ANY(d.share_with) OR EXISTS (
        SELECT 1 FROM pg_roles r
        WHERE r.rolname = ANY(d.share_with)
          AND pg_has_role(caller_name, r.oid, 'MEMBER')
      ))
  ) THEN
    RAISE EXCEPTION 'A linked document is unavailable or cannot be modified.'
      USING ERRCODE = '42501';
  END IF;

  -- Freeze existing links, sample permissions/location assignments, and parent
  -- permissions. Sorting each lock set keeps concurrent saves predictable.
  PERFORM sd.sample_id FROM discrete.sample_documents sd
  WHERE sd.document_id = ANY(requested_ids)
  ORDER BY sd.document_id, sd.sample_id FOR SHARE;
  PERFORM s.sample_id FROM discrete.samples s
  WHERE EXISTS (SELECT 1 FROM discrete.sample_documents sd
                WHERE sd.sample_id = s.sample_id AND sd.document_id = ANY(requested_ids))
  ORDER BY s.sample_id FOR SHARE;
  PERFORM l.location_id FROM public.locations l
  WHERE EXISTS (SELECT 1 FROM discrete.sample_documents sd
                JOIN discrete.samples s USING (sample_id)
                WHERE s.location_id = l.location_id AND sd.document_id = ANY(requested_ids))
  ORDER BY l.location_id FOR SHARE;

  WITH linked AS MATERIALIZED (
    SELECT sd.document_id, s.sample_id, s.share_with, s.location_id,
           l.location_id AS parent_id, l.share_with AS parent_groups
    FROM discrete.sample_documents sd
    JOIN discrete.samples s USING (sample_id)
    LEFT JOIN public.locations l ON l.location_id = s.location_id
    WHERE sd.document_id = ANY(requested_ids)
  ), access_lists AS MATERIALIZED (
    SELECT x.document_id, x.share_with AS groups FROM linked x
    UNION ALL
    SELECT DISTINCT x.document_id, x.parent_groups FROM linked x
    WHERE x.location_id IS NOT NULL
  ), restrictions AS MATERIALIZED (
    SELECT a.document_id, a.groups FROM access_lists a
    WHERE NOT coalesce('public_reader' = ANY(a.groups), false)
  ), counts AS (
    SELECT a.document_id, count(*) FILTER (
             WHERE NOT coalesce('public_reader' = ANY(a.groups), false)
           ) AS restricted_count,
           bool_or(a.groups IS NULL OR cardinality(a.groups) = 0
                   OR array_position(a.groups, NULL) IS NOT NULL) AS invalid
    FROM access_lists a GROUP BY a.document_id
  ), common_groups AS (
    SELECT r.document_id, g.role_name
    FROM restrictions r
    CROSS JOIN LATERAL (SELECT DISTINCT unnest(r.groups) AS role_name) g
    JOIN counts c ON c.document_id = r.document_id
    GROUP BY r.document_id, g.role_name, c.restricted_count
    HAVING count(*) = c.restricted_count
  ), resolved AS (
    SELECT id AS document_id,
      CASE WHEN c.document_id IS NULL OR c.invalid OR EXISTS (
                  SELECT 1 FROM linked x WHERE x.document_id = id
                    AND x.location_id IS NOT NULL AND x.parent_id IS NULL
                ) THEN ARRAY[]::text[]
           WHEN c.restricted_count = 0 THEN ARRAY['public_reader']::text[]
           ELSE ARRAY(SELECT g.role_name FROM common_groups g
                      WHERE g.document_id = id ORDER BY g.role_name)
      END AS groups
    FROM unnest(requested_ids) id LEFT JOIN counts c ON c.document_id = id
  )
  SELECT jsonb_agg(jsonb_build_object('document_id', r.document_id, 'groups', r.groups)
                   ORDER BY r.document_id) INTO decisions FROM resolved r;

  IF EXISTS (SELECT 1 FROM jsonb_array_elements(decisions) d
             WHERE jsonb_array_length(d->'groups') = 0) THEN
    RAISE EXCEPTION 'A document has no access group shared by all linked samples and locations. Give them a shared group or use separate workbooks.'
      USING ERRCODE = '22023';
  END IF;
  RETURN QUERY SELECT (d->>'document_id')::integer,
    ARRAY(SELECT jsonb_array_elements_text(d->'groups'))
  FROM jsonb_array_elements(decisions) d ORDER BY (d->>'document_id')::integer;
END;
$function$;

ALTER FUNCTION discrete.get_document_sharing_groups(integer[]) OWNER TO admin;
REVOKE ALL ON FUNCTION discrete.get_document_sharing_groups(integer[]) FROM PUBLIC;
-- Match existing document-editing privileges, including custom editor roles.
-- The function independently authorizes every document on every invocation.
DO $grant$
DECLARE editor record;
BEGIN
  FOR editor IN SELECT r.rolname FROM pg_roles r
    WHERE has_column_privilege(r.oid, 'files.documents', 'share_with', 'UPDATE')
  LOOP
    EXECUTE format('GRANT EXECUTE ON FUNCTION discrete.get_document_sharing_groups(integer[]) TO %I', editor.rolname);
  END LOOP;
END;
$grant$;

COMMENT ON FUNCTION discrete.get_document_sharing_groups(integer[]) IS
'Returns only the common explicit sharing groups for complete linked sample documents, including location restrictions. Requires caller access to and update permission on every requested document. Public access is unrestricted; missing links or incompatible groups reject the batch. Omitted workbook content is ignored. Locks persist until the caller transaction ends; document updates remain subject to caller privileges.';
