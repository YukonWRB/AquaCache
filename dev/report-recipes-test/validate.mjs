// Usage: node validate.mjs <temporary-runtime-directory> <AquaCache-checkout>
// Runtime directory needs pglite + ajv and patch.json from extract-patch.R.
// These are test-only dependencies; no application dependency is introduced.
import fs from 'node:fs/promises';
import path from 'node:path';
import { createRequire } from 'node:module';
import assert from 'node:assert/strict';
const [runtime, repo] = process.argv.slice(2);
const require = createRequire(path.join(runtime, 'package.json'));
const { PGlite } = require('@electric-sql/pglite');
const Ajv = require('ajv/dist/2020');
const db = new PGlite();
const read = async name => JSON.parse(await fs.readFile(path.join(repo, 'inst/report-recipes', name), 'utf8'));
const schema = await read('schema-v2.json');
const hydro = await read('hydrometric.example.json');
const water = await read('water-quality.example.json');
const validate = new Ajv({strict: false}).compile(schema);
let checks = 0;
const ok = (condition, message) => { assert.ok(condition, message); checks++; };
const query = async (sql, params = []) => (await db.query(sql, params)).rows;
const reject = async (sql, params = [], code = '23514') => {
  try { await db.query(sql, params); assert.fail('Expected SQL failure: ' + sql); }
  catch (e) { assert.equal(e.code, code, e.message); checks++; }
};
try {
  // Minimal Patch-60 prerequisites. Shared audit/share/timestamp helpers are
  // intentionally fixtures: this tests reporting SQL, not those implementations.
  await db.exec(`
    CREATE ROLE admin;
    CREATE ROLE public_reader;
    CREATE ROLE alice;
    CREATE ROLE bob;
    CREATE SCHEMA application;
    GRANT USAGE ON SCHEMA application TO PUBLIC;
    CREATE SCHEMA information;
    GRANT USAGE ON SCHEMA information TO PUBLIC;
    CREATE TABLE information.version_info(item text, version text);
    INSERT INTO information.version_info VALUES ('Last patch number', '60');
    GRANT SELECT ON information.version_info TO PUBLIC;
    CREATE SCHEMA files;
    CREATE TABLE files.documents(document_id integer PRIMARY KEY);
    CREATE SCHEMA audit;
    CREATE TABLE audit.table_registry(schema_name text, table_name text,
      capture_mode text, rationale text, history_started_at timestamptz, updated_at timestamptz);
    CREATE FUNCTION audit.if_modified_func() RETURNS trigger LANGUAGE plpgsql
      AS $$ BEGIN RETURN NULL; END; $$;
    CREATE FUNCTION public.update_modified() RETURNS trigger LANGUAGE plpgsql
      AS $$ BEGIN NEW.modified := clock_timestamp(); RETURN NEW; END; $$;
    CREATE FUNCTION public.user_modified() RETURNS trigger LANGUAGE plpgsql
      AS $$ BEGIN NEW.modified_by := CURRENT_USER; RETURN NEW; END; $$;
    CREATE FUNCTION public.validate_share_with() RETURNS trigger LANGUAGE plpgsql
      AS $$ BEGIN RETURN NEW; END; $$;
  `);
  const patch = JSON.parse(await fs.readFile(path.join(runtime, 'patch.json'), 'utf8'));
  await db.exec('BEGIN');
  for (const statement of patch.statements) {
    if (statement.params) await db.query(statement.sql, statement.params);
    else await db.exec(statement.sql);
  }
  const flags = (await query(patch.verification))[0];
  for (const [name, value] of Object.entries(flags)) ok(value === true, name);
  ok((await query("SELECT version FROM information.version_info"))[0].version === '60', 'patch level');
  const schemaId = (await query("SELECT report_recipe_schema_id AS id FROM application.report_recipe_schemas WHERE schema_version=2"))[0].id;
  for (const recipe of [hydro, water]) {
    ok(validate(recipe), JSON.stringify(validate.errors));
    await query('SELECT application.validate_report_recipe_v2($1::jsonb, $2::jsonb)', [JSON.stringify(recipe), JSON.stringify(schema)]);
    checks++;
  }
  // Database and standard JSON Schema validator must agree on malformed shapes.
  for (const mutate of [
    r => r.datasets = null,
    r => r.datasets[0].source = null,
    r => r.inputs[0].required = 'yes',
    r => r.output.formats = [],
    r => r.sections[0].typo = true,
    r => r.steps[0].operation.version = 1.5,
    r => r.steps[0].options.lags_seconds = [0],
    r => r.steps[0].options.typo = true,
    r => delete r.datasets[0].options,
  ]) {
    const bad = structuredClone(hydro); mutate(bad);
    ok(!validate(bad), 'AJV should reject malformed recipe');
    await reject('SELECT application.validate_report_recipe_v2($1::jsonb,$2::jsonb)', [JSON.stringify(bad), JSON.stringify(schema)]);
  }
  // Cross-reference and topology checks go beyond structural JSON Schema.
  for (const mutate of [
    r => r.datasets.push(structuredClone(r.datasets[0])),
    r => r.steps[0].inputs = ['unknown'],
    r => r.steps[0].inputs = [r.steps[1].id],
    r => r.steps[0].id = r.datasets[0].id,
    r => r.sections[0].datasets = ['unknown'],
    r => r.datasets[0].selection.input = 'unknown',
    r => r.datasets[0].period.anchor.input = 'locations',
    r => r.annotations[0].section = 'levels',
    r => r.inputs[0].default = 123,
    r => r.datasets[0].period.start_offset_seconds = 1,
    r => r.steps[0].options.baseline_start_year = 2030,
  ]) {
    const bad = structuredClone(hydro); mutate(bad);
    await reject('SELECT application.validate_report_recipe_v2($1::jsonb,$2::jsonb)', [JSON.stringify(bad), JSON.stringify(schema)]);
  }
  await db.exec('SET ROLE alice');
  const recipeId = (await query("INSERT INTO application.report_recipes(recipe_name,share_with) VALUES ('Private report',NULL) RETURNING report_recipe_id AS id"))[0].id;
  const rev = (await query('INSERT INTO application.report_recipe_revisions(report_recipe_id,report_recipe_schema_id,recipe) VALUES ($1,$2,$3::jsonb) RETURNING report_recipe_revision_id AS id, revision', [recipeId,schemaId,JSON.stringify(hydro)]))[0];
  ok(rev.revision === 1, 'revision allocation');
  ok((await query('SELECT current_revision FROM application.report_recipes WHERE report_recipe_id=$1',[recipeId]))[0].current_revision === 1, 'revision publication');
  await reject('INSERT INTO application.report_recipe_revisions(report_recipe_id,report_recipe_schema_id,recipe,revision) VALUES ($1,$2,$3::jsonb,1)',[recipeId,schemaId,JSON.stringify(hydro)]);
  await reject('UPDATE application.report_recipe_revisions SET recipe=recipe WHERE report_recipe_revision_id=$1',[rev.id],'42501');
  await db.exec('SET ROLE bob');
  ok((await query('SELECT * FROM application.report_recipes')).length === 0, 'private recipe hidden');
  await reject("INSERT INTO application.report_recipes(recipe_name,owner_role) VALUES ('Spoof','alice')",[],'42501');
  await db.exec('SET ROLE alice');
  const args = {as_of:'2026-09-04T07:40:00-07:00',locations:['08AA003']};
  await reject("INSERT INTO application.report_runs(report_recipe_revision_id,runtime_arguments) VALUES ($1,'{}')",[rev.id]);
  await reject('INSERT INTO application.report_runs(report_recipe_revision_id,runtime_arguments) VALUES ($1,$2::jsonb)',[rev.id,JSON.stringify({...args,as_of:'2026-99-99T00:00:00Z'})]);
  await reject('INSERT INTO application.report_runs(report_recipe_revision_id,runtime_arguments) VALUES ($1,$2::jsonb)',[rev.id,JSON.stringify({...args,typo:true})]);
  await reject("INSERT INTO application.report_runs(report_recipe_revision_id,runtime_arguments,status) VALUES ($1,$2::jsonb,'running')",[rev.id,JSON.stringify(args)]);
  const run = (await query('INSERT INTO application.report_runs(report_recipe_revision_id,runtime_arguments) VALUES ($1,$2::jsonb) RETURNING report_run_id AS id',[rev.id,JSON.stringify(args)]))[0].id;
  await reject("INSERT INTO application.report_run_outputs(report_run_id,output_name,output_format,storage_kind) VALUES ($1,'report','xlsx','ephemeral')",[run]);
  await query("UPDATE application.report_runs SET status='running',started_at=clock_timestamp() WHERE report_run_id=$1",[run]);
  await reject("UPDATE application.report_runs SET status='succeeded',completed_at=clock_timestamp() WHERE report_run_id=$1",[run]);
  await query("INSERT INTO application.report_run_outputs(report_run_id,output_name,output_format,storage_kind) VALUES ($1,'report','xlsx','ephemeral')",[run]);
  await query(`UPDATE application.report_runs SET status='succeeded',completed_at=clock_timestamp(),
    executor='{"name":"test","version":"1"}',resolved_inputs='{"datasets":[]}' WHERE report_run_id=$1`,[run]);
  await reject("INSERT INTO application.report_run_outputs(report_run_id,output_name,output_format,storage_kind) VALUES ($1,'late','pdf','ephemeral')",[run]);
  await reject("UPDATE application.report_runs SET status_message='changed' WHERE report_run_id=$1",[run],'55000');
  for (const body of ['First comment','Corrected comment','']) {
    await query("INSERT INTO application.report_run_annotations(report_run_id,annotation_key,body) VALUES ($1,'general',$2)",[run,body]);
  }
  ok((await query('SELECT revision FROM application.report_run_annotations ORDER BY revision')).map(r=>r.revision).join(',') === '1,2,3','append-only commentary');
  await reject("INSERT INTO application.report_run_annotations(report_run_id,annotation_key,body) VALUES ($1,'station','Missing entity')",[run]);
  await reject("UPDATE application.report_run_annotations SET body='rewrite'",[],'42501');
  await db.exec('SET ROLE bob');
  ok((await query('SELECT * FROM application.report_runs')).length === 0, 'private runs hidden');
  ok((await query('SELECT * FROM application.report_run_outputs')).length === 0, 'private outputs hidden');
  ok((await query('SELECT * FROM application.report_run_annotations')).length === 0, 'private annotations hidden');
  await reject("INSERT INTO application.report_run_annotations(report_run_id,annotation_key,body) VALUES ($1,'general','Unauthorized')",[run],'42501');
  await db.exec('SET ROLE alice');
  await query("UPDATE application.report_recipes SET share_with=ARRAY['public_reader'] WHERE report_recipe_id=$1",[recipeId]);
  await db.exec('SET ROLE bob');
  ok((await query('SELECT * FROM application.report_recipes')).length === 1, 'public recipe visible');
  ok((await query('SELECT * FROM application.report_runs')).length === 0, 'sharing recipe does not share runs');
  await db.exec('SET ROLE alice');
  await query('INSERT INTO application.report_runs(report_recipe_revision_id,previous_run_id,runtime_arguments) VALUES ($1,$2,$3::jsonb)',[rev.id,run,JSON.stringify(args)]);
  checks++;
  console.log(`PASS: ${checks} checks; ${patch.statements.length} patch statements executed in isolated PostgreSQL.`);
  console.log((await query('SELECT version()'))[0].version);
} catch (e) { console.error(e.message, e.code, e.where || ""); process.exitCode = 1; } finally { await db.close(); }

