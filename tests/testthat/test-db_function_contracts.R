test_that("DB function manifest matches live catalog and contract coverage", {
  skip_if_no_db_contracts()

  con <- connect_test()
  on.exit(cleanup_postgres_session(con), add = TRUE)

  ensure_pgtap(con)

  live <- db_function_inventory(con)
  manifest <- read_db_function_manifest()

  required_manifest_cols <- c(
    "signature",
    "function_kind",
    "smoke_contract"
  )
  expect_true(
    all(required_manifest_cols %in% names(manifest)),
    info = "Manifest must contain signature, function_kind, and smoke_contract columns."
  )

  expect_false(anyDuplicated(manifest$signature) > 0)

  missing_in_manifest <- setdiff(live$signature, manifest$signature)
  stale_in_manifest <- setdiff(manifest$signature, live$signature)

  expect_equal(
    length(missing_in_manifest),
    0,
    info = paste(
      "Missing functions in manifest:",
      paste(missing_in_manifest, collapse = ", ")
    )
  )
  expect_equal(
    length(stale_in_manifest),
    0,
    info = paste(
      "Stale manifest entries not found in DB:",
      paste(stale_in_manifest, collapse = ", ")
    )
  )

  merged <- merge(
    live,
    manifest[, c("signature", "function_kind")],
    by = "signature",
    suffixes = c("_live", "_manifest")
  )

  kind_mismatch <- merged$signature[
    merged$function_kind_live != merged$function_kind_manifest
  ]
  expect_equal(
    length(kind_mismatch),
    0,
    info = paste(
      "Function kind mismatch between DB and manifest:",
      paste(kind_mismatch, collapse = ", ")
    )
  )

  contracts <- db_contract_files()
  contract_basenames <- basename(contracts)
  expect_true(
    all(manifest$smoke_contract %in% contract_basenames),
    info = "Each manifest smoke_contract must map to an existing SQL contract file."
  )

  coverage <- read_db_contract_coverage()
  missing_coverage <- setdiff(manifest$signature, unique(coverage$signature))
  extra_coverage <- setdiff(unique(coverage$signature), manifest$signature)

  expect_equal(
    length(missing_coverage),
    0,
    info = paste(
      "Missing @covers entries for manifest signatures:",
      paste(missing_coverage, collapse = ", ")
    )
  )
  expect_equal(
    length(extra_coverage),
    0,
    info = paste(
      "@covers entries refer to unknown signatures:",
      paste(extra_coverage, collapse = ", ")
    )
  )

  manifest_pairs <- unique(paste(
    manifest$signature,
    manifest$smoke_contract,
    sep = "@@"
  ))
  coverage_pairs <- unique(paste(
    coverage$signature,
    coverage$contract_file,
    sep = "@@"
  ))
  missing_pairs <- setdiff(manifest_pairs, coverage_pairs)

  expect_equal(
    length(missing_pairs),
    0,
    info = paste(
      "Manifest signatures missing @covers in their declared smoke contract:",
      paste(missing_pairs, collapse = ", ")
    )
  )
})


test_that("DB function pgTAP contract suites run cleanly", {
  skip_if_no_db_contracts()

  con <- connect_test()
  on.exit(cleanup_postgres_session(con), add = TRUE)

  ensure_pgtap(con)

  contract_dir <- testthat::test_path("fixtures", "sql", "contracts")
  contract_files <- file.path(
    contract_dir,
    c(
      "callable_smoke.sql",
      "trigger_smoke.sql",
      "semantic_high_risk.sql"
    )
  )

  expect_true(all(file.exists(contract_files)))

  for (file in contract_files) {
    run_pgtap_contract_file(con, file, statement_timeout = "20s")
  }
})


test_that("get_csw_layer is time-bounded in constrained and bypass modes", {
  skip_if_no_db_contracts()

  con <- connect_test()
  on.exit(cleanup_postgres_session(con), add = TRUE)

  run_get_csw <- function(setup_sql = character()) {
    DBI::dbExecute(con, "BEGIN;")
    on.exit(
      suppressWarnings(try(DBI::dbExecute(con, "ROLLBACK;"), silent = TRUE)),
      add = TRUE
    )

    DBI::dbExecute(con, "SET LOCAL statement_timeout = '8s';")
    for (sql in setup_sql) {
      DBI::dbExecute(con, sql)
    }

    t0 <- proc.time()[["elapsed"]]
    DBI::dbGetQuery(con, "SELECT count(*) AS n FROM public.get_csw_layer();")
    proc.time()[["elapsed"]] - t0
  }

  quote_ident <- function(role_name) {
    as.character(DBI::dbQuoteIdentifier(con, role_name))
  }

  constrained_elapsed <- NA_real_
  bypass_elapsed <- NA_real_

  has_public_reader <- DBI::dbGetQuery(
    con,
    "SELECT 1 AS ok FROM pg_roles WHERE rolname = 'public_reader' LIMIT 1"
  )

  if (nrow(has_public_reader) > 0) {
    constrained_elapsed <- tryCatch(
      run_get_csw(c(
        paste0("SET LOCAL ROLE ", quote_ident("public_reader"), ";"),
        "SET LOCAL row_security = on;"
      )),
      error = function(e) NA_real_
    )
  }

  if (is.na(constrained_elapsed)) {
    constrained_elapsed <- tryCatch(
      run_get_csw("SET LOCAL row_security = on;"),
      error = function(e) NA_real_
    )
  }

  expect_false(is.na(constrained_elapsed))
  expect_lt(constrained_elapsed, 8)

  bypass_elapsed <- tryCatch(
    run_get_csw("SET LOCAL row_security = off;"),
    error = function(e) NA_real_
  )

  if (is.na(bypass_elapsed)) {
    bypass_roles <- DBI::dbGetQuery(
      con,
      "
      SELECT rolname
      FROM pg_roles
      WHERE rolbypassrls
      ORDER BY CASE WHEN rolname = 'bypassrls' THEN 0 ELSE 1 END, rolname
      "
    )

    if (nrow(bypass_roles) > 0) {
      for (role_name in bypass_roles$rolname) {
        bypass_elapsed <- tryCatch(
          run_get_csw(c(
            paste0("SET LOCAL ROLE ", quote_ident(role_name), ";"),
            "SET LOCAL row_security = on;"
          )),
          error = function(e) NA_real_
        )
        if (!is.na(bypass_elapsed)) {
          break
        }
      }
    }
  }

  if (is.na(bypass_elapsed)) {
    skip("Unable to run a bypass-RLS mode query in this test environment.")
  }

  expect_lt(bypass_elapsed, 8)
})
