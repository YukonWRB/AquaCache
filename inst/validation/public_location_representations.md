# Public Location Representations

Patch 48 adds a database-side public representation layer for locations, samples,
and results. It keeps raw `public.locations`, `discrete.samples`, and
`discrete.results` tied to true locations, while public-facing code can switch to
views that expose exact coordinates only when the current database role is
allowed by `public.locations.exact_share_with`.

## Objects

- `public.locations.exact_share_with`: role array controlling exact coordinate
  visibility in public-safe views. This follows the same role-array pattern as
  `share_with`, but it controls exact latitude/longitude rather than row
  visibility.
- `public.location_public_geometries`: one active public geometry per true
  location. The active geometry can be `exact_point`, `masked_point`, or
  `reporting_polygon`.
- `public.location_reporting_areas`: metadata for reporting polygons that
  represent one or more exact locations.
- `public.location_reporting_area_members`: RLS-limited mapping between true
  locations and reporting areas. It lets the public-safe views expose reporting
  area grouping without exposing coordinates.
- `public.locations_public`, `discrete.samples_public`,
  `discrete.results_public`: application-facing views that expose public-safe
  location geometry.

## Raw Coordinate Protection

PostgreSQL RLS filters rows, not individual cells. That means
`public.locations.latitude` and `public.locations.longitude` cannot be hidden for
some visible rows and exposed for other visible rows directly in the base table.

Patch 48 adds a stronger practical boundary for public access:

- `public_reader` keeps column-level `SELECT` access to non-coordinate
  `public.locations` columns, so normal row visibility can still follow
  `share_with`.
- `public_reader` loses direct column access to `public.locations.latitude` and
  `public.locations.longitude`.
- `public.locations_public` is the conditional coordinate access path. It
  enforces `share_with` row visibility itself, then returns exact coordinates
  only when `exact_share_with` allows the current role. Otherwise it returns the
  stored masked point, reporting polygon, or null coordinate fields.

Internal/admin roles that need full maintenance access can keep direct raw table
access. Public-facing application code should use the public-safe views.

## Deployment

Run the patch in dry-run mode first:

```r
patch_48_dry_run <- TRUE
source("inst/patches/patch_48.R", local = TRUE)
```

The dry run creates the objects, runs the patch verification queries, and then
rolls back the transaction. To apply permanently, run the patch without setting
`patch_48_dry_run`.

After applying the patch, run:

```sql
\i inst/validation/public_location_representations.sql
```

The object checks should all be true and the `bad_count` query should return
zero rows.

## Public Geometry Assignment

By default, the patch backfills an active `exact_point` geometry for existing
locations and sets `exact_share_with` to `ARRAY['public_reader']`. That preserves
current behavior until a location is deliberately restricted.

For a sensitive singleton location, update `exact_share_with` to the internal
group that may see the true coordinate, then replace the active public geometry
with a stored `masked_point`. Use `public.location_masked_point(...)` to generate
a stable point within a configured offset range.

For clustered well reporting, create a reporting polygon and use it as the active
public geometry for the relevant locations. The membership table records which
true locations were assigned to the reporting area. Its RLS policy exposes only
memberships whose true location row and reporting area are visible to the current
role.

The public-safe views expose `public_location_id`, `reporting_area_id`, and
`reporting_location_id`. Application code can use `public_location_id` for
grouping/reporting while raw samples and results remain tied to the true
`location_id` in the base tables.

## Grant Boundary

AquaCache has historically used `security_invoker` views with forced RLS on core
tables. That pattern means a role often needs underlying table privileges for
views to work. Patch 48 deliberately treats `public.locations_public` as the
public coordinate boundary instead: it has `security_barrier = true`, enforces
`share_with` visibility in the view query, and does not require `public_reader`
to have raw coordinate-column privileges.

The validation SQL includes a grant audit. Any role listed with raw coordinate,
raw-table grants, or exact metadata-view access can still bypass the public-safe
views if it is allowed to run arbitrary SQL. For public applications, switch code
to `public.locations_public`, `discrete.samples_public`, and
`discrete.results_public`.
