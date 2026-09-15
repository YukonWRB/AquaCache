# Portable report recipes

This development patch stores report intent, execution evidence and human commentary
in the existing `application` schema. It does not implement a report executor or
connect YGwater to these tables. A renderer in R, Python, JavaScript or another
language can consume the same database contract.

## Review outcome

The original five-table separation was sound. The principal gaps were an almost
unconstrained recipe document, no representation of raster/forecast sources or
processing dependencies, no typed runtime parameters, no durable comment-edit
history, and incomplete execution integrity checks. The revised patch:

- Retains the original version 1 schema as **deprecated**, and publishes version 2
  with typed inputs, datasets, ordered processing steps, criteria, ordered sections,
  annotation definitions and output configuration.
- Validates version 2 structure against the published schema in PostgreSQL as well
  as in ordinary JSON Schema clients. Adds reference, ordering and type checks.
- Adds `report_run_annotations` and an immutable `report_runs.previous_run_id`.
- Requires runs to begin queued, prevents rewriting identity/start time or deleting
  run history, and requires an artifact and provenance before success.
- Serializes artifact insertion against run completion; completed runs cannot gain
  extra files. Comments remain appendable after execution completes.
- Avoids changing permissions on unrelated application objects. Recipe visibility
  does not grant data access, run access or permission to retrieve a linked file.

The reference workbook has seven sheets: comments, levels, flows, snow, bridges,
temperature and precipitation. Besides station values it contains lagged changes,
historical comparisons, freshness, basin precipitation estimates/forecasts and
current/previous commentary. `YGwater::tabularReport()` remains its existing
implementation. The hydrometric example below illustrates **level and precipitation
sections**, not an exact seven-sheet recreation or a claim that its calculations
match that function. Workbook cell contents were treated as source material.

## Relational model

| Table | Responsibility |
| --- | --- |
| `report_recipe_schemas` | Immutable versioned JSON Schema documents; status can change. |
| `report_recipes` | Stable identity, owner, sharing, tags and active flag. |
| `report_recipe_revisions` | Immutable recipe snapshots, authoring client and revision number. |
| `report_runs` | One execution of a specific revision; effective arguments, resolved inputs, executor and state. |
| `report_run_outputs` | Immutable references to report files, input snapshots or supporting artifacts. |
| `report_run_annotations` | Append-only versions of human commentary, scoped by run and annotation definition. |

Recipes remain publicly readable by default (`public_reader` sharing); use NULL
sharing for a private recipe. Runs, artifacts and annotations use the requester
role's visibility. A team needing shared commentary should use a suitable group
requester role; there is no separate reviewer workflow in this patch. Existing
application comments/documents are not migrated or replaced.

IDs are local to a database. JSON exports use `contract` to identify their schema;
the enclosing revision row selects the actual stored schema document. The supplied
version 2 requires `urn:aquacache:report-recipe:2`. The database-generated `recipe_md5`
is change detection over PostgreSQL JSONB text, not a portable serialization hash
or a signature; clients should read it rather than independently reserialize JSON.

## Version 2 interpretation rules

`schema-v2.json` is a [JSON Schema 2020-12](https://json-schema.org/draft/2020-12/json-schema-core)
document. It is also stored verbatim as JSONB in the database. Use the stored
version for a revision, not whatever schema happens to be current in the client.

1. Inputs have stable IDs, explicit types and a required flag. An omitted default
   is distinct from JSON null. Run insertion rejects unknown arguments, applies
   declared defaults, checks types/required values, and stores the resulting
   effective arguments immutably. Dates use ISO calendar dates; timestamps require
   an explicit offset or Z. Input binding is `{"input":"id"}`, not substitution
   into SQL, formulas, templates or source code. String fields are literal text.
2. Dataset source, processing operation, criterion provider and section type use
   `{code, version}` capability identities. Recognizing a name is insufficient:
   the executor must support that exact version and its options. Unknown versions,
   source types, formats, extension semantics or options must fail before execution;
   they must never cause silent omission or a best-effort compliance result.
3. Dataset IDs and step IDs share one namespace. Steps execute in array order and
   reference only datasets or preceding steps. Section order is presentation order.
   Section columns also preserve array order. The database rejects collisions,
   forward references, cycles and dangling section references.
4. Selection mode is `all`, `explicit`, or `input`. Explicit mode requires location,
   series, sample or licence identifiers. Input mode requires both `input` and
   `input_field`; the latter states which identifier collection the input supplies.
   Supplied identifier collections and parameter/media/fraction filters intersect;
   members within a collection are alternatives. `parameter_ids` are database IDs;
   `parameters` are exact `public.parameters.param_name` values. Locations use
   exact `public.locations.location_code` values. Media/fraction labels must be
   resolved unambiguously by the adapter, not through fuzzy matching.
5. Selections are re-resolved at run time under the data user's permissions.
   Record exact resolved IDs, rejected/missing identifiers and any policy decisions.
   Empty resolution does not mean all data. Locationless QC samples may be included
   through associated sample groups; missing scope association must not pull in
   unrelated blanks. A recipe's licence selection does not establish a legal
   relationship between a licence and any guideline.
6. Periods are half-open `[start,end)` intervals. Literal endpoints and input-bound
   endpoints are absolute timestamps. Relative offsets are integer elapsed seconds
   from a datetime input. The executor must validate resolved start < end, calendar
   values, timezone names, source compatibility and availability before extraction.
   Use a separate baseline dataset for historical comparisons; the example does so.
   Calendar-day aggregations use `output.timezone`; elapsed durations do not change
   with daylight saving. UTC offsets in the example are explicit.
7. Quality policy is explicit. `retain` keeps missing/censored/flagged records with
   their condition metadata; it does not substitute zero. `omit` excludes them from
   applicable calculations and records counts/reasons. `fail` stops that operation.
   Approved-only selection applies to each observation/result and relevant approval
   interval. Censored values must retain their bound, qualifier and condition.
8. `metadata` and `extensions` hold data. They have no implicit computational effect.
   To add a report family or extension, publish its versioned semantic contract and
   option schemas; clients must explicitly implement them. No R object, SQL query,
   executable expression or arbitrary template program belongs in a recipe.

The database deliberately does not enforce foreign keys from every selector into
current source tables: selectors may be dynamic, cross-source, or resolved by a
future application. Execution must resolve them and retain the result. An empty
dataset list is valid for a narrative-only report; sections still cannot reference
nonexistent data.

## Core capability conventions supplied with these examples

These names specify **new executor contracts**, not implemented adapters and not
aliases that automatically invoke existing R functions. The example recipes are
storage/validation fixtures. Validate support before offering them as runnable.
Known version 1 processing options are structurally constrained in the schema;
new capability names remain storable for future clients.

- `aquacache.continuous/1`: returns accepted observations with series/location/
  parameter identity, timestamp, value, unit and quality context. `single_or_fail`
  requires one series per selected location/parameter; `all` keeps series separate.
  Do not pick an arbitrary series or silently combine units/datums.
- `aquacache.discrete/1`: returns canonical results with sample, parameter, unit,
  fraction, medium, speciation, condition and QC context. Optional component
  measurements are supporting detail; do not double-count canonical and component
  rows in an assessment. Preserve real sample identity, including locationless QC.
- `aquacache.raster/1`: resolves the named historical and forecast products, selected
  locations' basin masks, raster series and valid/issue times. Historical observations
  and forecast cycles remain distinguishable. Names must resolve uniquely.
- `aquacache.conditions/1`: consumes observation and baseline datasets in that order,
  matched by series identity and unit/datum. For each series, take its last nonmissing
  accepted observation in the requested interval. `value` is that value; data age is
  `(interval_end - observation_time)/3600`. For each lag, choose the most recent
  nonmissing observation at or before `(last_observation_time - lag)` within
  `lag_tolerance_seconds`; change is current minus lag value, in the original unit.
  Missing lag matches remain null. Staleness flags values older than the supplied
  threshold; it does not replace them with zero. The same-calendar-day baseline
  consists of one arithmetic daily mean per baseline year in the output timezone,
  limited to the supplied inclusive year range. Historical range percentage is
  `100*(value-min)/(max-min)` and mean percentage is `100*value/mean` over those daily
  means. Require the stated minimum number of years. Zero denominators, insufficient
  baseline or February 29 with `leap_day=omit` produce null plus a reason. Do not clamp
  out-of-range percentages. Baseline extraction is explicit, never a hidden query.
- `aquacache.basin_precipitation/1`: consumes one raster dataset. Produce area-weighted
  mean total liquid-equivalent precipitation over each basin for each requested
  elapsed-hour window, in mm. Retrospective windows end at the run anchor; forecast
  windows start there. Choose the latest forecast issued at or before that anchor,
  retaining its issue time; do not mix forecast cycles to fill missing lead times.
  Avoid double-counting cumulative forecast fields. Do not prorate partially covered
  time steps silently. `minimum_coverage` applies to both spatial and temporal
  coverage; inadequate coverage yields null and a reason, not zero precipitation.
- `aquacache.criteria/1`: resolves the explicitly supplied guideline IDs from the
  `guideline_input`, including their effective-date, location, parameter, fraction,
  medium, speciation, units, rule inputs and coefficients from the `criteria` schema.
  `licence_input` optionally records the user-selected licence context; it is not a
  database-inferred licence-to-guideline mapping. Resolve applicability at sample
  time. Snapshot the definitions/inputs actually used, not just mutable catalogue IDs.
- `aquacache.exceedances/1`: consumes one discrete dataset and the named recipe criteria.
  Apply supported guideline algorithms and comparison operators with compatible
  units and averaging periods. Return the sample/result and criterion identities,
  threshold and unit, assessment (`meets`, `exceeds`, `indeterminate`,
  `not_applicable`), and reason. Missing dependencies, unresolvable units or ambiguous
  applicability cannot become `meets`. `indeterminate` censored policy never treats
  a bound as a measured value; `upper_bound` assesses a censored interval only when
  its bound proves the outcome, otherwise returns indeterminate. Omitted censored
  results remain accounted for in exclusion counts. QC results are identified as
  such and must not be labelled licence compliance. The example's `basis=sample`
  supports individual results; daily/period assessment requires an executor that
  implements the associated rule's coverage and averaging requirements.
- `table/1`: renders a single referenced dataset with ordered labelled columns.
  Field names are data keys, never expressions. Unit labels cannot cause implicit
  conversion. Display rounding does not change assessment values. Additional
  section types (plots, maps, narratives), temperature statistics, presentation
  thresholds and branded templates can be introduced through explicit capabilities
  and schema versions without changing these relational tables.

All of these adapters must report ambiguities, unsupported algorithms and missing
source data explicitly. The source/algorithm capability version, source IDs,
selected dates and implementation versions belong in the run evidence. This patch
makes those contracts storable; it does not certify a regulatory interpretation.

## Commentary and reproducibility

Each recipe annotation has an ID, report/section/entity scope, label, and carry-forward
flag. `annotation_key` is that ID. An entity key must be stable and source-qualified
(e.g. `location:08AA003` or `sample:123`), never a display row number. Nonentity notes
use the empty entity key. Insert a new row to edit; the database assigns the next
revision under a run row lock. Empty body explicitly clears the note. Never update
or delete an earlier annotation. Plain text must be escaped when rendered.

`previous_run_id` can reference a visible, earlier successful run of the same recipe.
For fields marked carry-forward, a client selects the latest annotation revision
for each `(annotation_key,entity_key)` in that run, checks compatibility with the
current recipe revision, and records the exact annotation IDs copied/used. Do not
carry a note from an unrelated station because two tables happen to have the same
row order. Existing workbook edits are not automatically imported by this patch.

The minimal execution sequence is:

1. Insert recipe metadata, then a revision, in one transaction. Omit `revision` for
   automatic allocation, or supply the next expected number for stale-edit detection.
2. Read a specific revision and validate schema, capability support and effective
   input values. Insert a queued run. No data permissions are conferred by this insert.
3. Resolve data under the correct requester identity. Store `resolved_inputs` and
   executor information; transition to running and set `started_at`.
4. Render from that resolved evidence. Insert artifact references while running;
   use `artifact_role=report`, `input_snapshot`, or `supporting`. Persist original
   input data/rules as snapshots when reproducibility requires them.
5. Mark succeeded with a completion timestamp after at least one report artifact is
   recorded. A failed run requires a message; cancellation may precede execution.
   A retry or re-render is a new run, including after commentary changes.

Recommended `resolved_inputs` fields: `database_identity`, `captured_at`, `datasets`
(with exact IDs, time bounds, quality rules/counts, units, extraction revision and
forecast cycles), `criteria` (resolved definitions and coefficients), `annotations`
(exact IDs and text used), `capabilities`, and `warnings`. `executor` should include
name, version, build/dependency versions, and supported capability versions.
The database requires a nonempty resolved-input object and string executor
name/version before success; the executor is responsible for manifest completeness.
Do not store credentials or temporary bearer URLs in any of these objects.

A recipe plus IDs and a database patch number is **not** a historical data snapshot:
data, permissions and guideline definitions can change. Exact reproduction needs
retained input snapshots and templates as well as the recorded software versions.
The output table can reference these through `files.documents` or durable URIs,
with checksums. Ephemeral outputs explicitly offer no durable retrieval guarantee.
Template references pin ID, version and SHA-256; executors must verify the bytes.
The format does not duplicate binary content in recipe tables.

## Validation and application

The reporting patch is unnumbered, requires Patch 60 and leaves its version record
unchanged. Keep `inst/patches/DEV_patch_reports.R` and the entire `inst/report-recipes`
directory together. Normal `source()` locates the companion directory relative to
the sourced patch. For `sys.source()`, set `DEV_patch_reports_contract_dir` explicitly.
Do not load a stale installed-package contract when applying a checkout patch.
The patch refuses to overwrite existing reporting objects; an already-applied
first-pass patch requires a separate data-preserving migration, not dropping tables.

The SQL validator deliberately supports only the JSON Schema vocabulary used here,
including local `$defs` references and the supplied simple regular expressions.
It is not a general JSON Schema engine. Unsupported keywords fail validation.
Only `aquacache-report/2` gets its full structural and cross-reference checks in
this patch; custom schema codes/versions need their own client validators (and a
subsequent patch if equivalent database validation is required). The database
checks input calendar validity at run insertion; clients also validate defaults,
literal dates, resolved intervals, operation arity, source compatibility and output
fields before execution. New code must not assume that a successful INSERT proves
scientific correctness or that a renderer exists.

`tests/report-recipes/extract-patch.R` captures the actual R-generated statements
without opening a connection. `tests/report-recipes/validate.mjs` executes them in
a disposable PGlite PostgreSQL instance and cross-checks JSON with AJV. Its minimal
Patch-60 audit/sharing/timestamp helper fixtures are explicitly stand-ins: this is
isolated reporting-SQL validation, not validation against a deployed AquaCache DB.
The test runtime dependencies are temporary tools, not package dependencies.

Run from the AquaCache checkout (supply your temporary runtime directory):

```powershell
# Install test-only @electric-sql/pglite and ajv in the temporary runtime directory.
& 'C:\Program Files\R\R-4.6.0\bin\Rscript.exe' tests/report-recipes/extract-patch.R '<runtime>\patch.json'
node tests/report-recipes/validate.mjs '<runtime>' 'C:\Users\gtdelapl\Documents\AquaCache'
```

A future authorized integration test should source the patch with
`DEV_patch_reports_dry_run <- TRUE` on the intended Patch-60 test database and
exercise its real shared helpers, grants and concurrent writers. The dry run
performs DDL inside a transaction before rollback; it is not a read-only operation.
No dev or production database was modified for this review.
