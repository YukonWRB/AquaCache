test_that("the seed database contains complete Patch 60 discrete fixtures", {
  testthat::skip_on_cran()

  con <- connect_test()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  if (aquacache_db_patch_number(con) < 60L) {
    skip("Composite discrete fixtures require database Patch 60.")
  }
  fixture_exists <- DBI::dbGetQuery(
    con,
    "SELECT EXISTS (
       SELECT 1
       FROM discrete.samples
       WHERE import_source = 'synthetic_fixture'
         AND import_source_id = 'SYN-S9'
     ) AS fixture_exists"
  )$fixture_exists[[1]]
  if (!isTRUE(fixture_exists)) {
    skip("The checked-in seed dump has not yet been regenerated.")
  }

  sample_associations <- DBI::dbGetQuery(
    con,
    "SELECT
       count(DISTINCT sq.qualifier_type_id) AS qualifier_count,
       count(DISTINCT so.observer_id) AS observer_count,
       array_agg(DISTINCT so.observer_role ORDER BY so.observer_role)
         FILTER (WHERE so.observer_role IS NOT NULL) AS observer_roles
     FROM discrete.samples s
     LEFT JOIN discrete.sample_qualifiers sq USING (sample_id)
     LEFT JOIN discrete.sample_observers so USING (sample_id)
     WHERE s.import_source = 'synthetic_fixture'
       AND s.import_source_id = 'SYN-S9'"
  )
  expect_equal(sample_associations$qualifier_count, 2)
  expect_equal(sample_associations$observer_count, 2)
  expect_identical(
    sample_associations$observer_roles[[1]],
    c("recorder", "sampler")
  )

  aggregations <- DBI::dbGetQuery(
    con,
    "SELECT
       summary.aggregation_type,
       summary.stored_result,
       summary.result_is_current,
       summary.component_count,
       summary.expected_count,
       summary.missing_component_count,
       summary.has_component_shortfall,
       summary.included_component_count,
       summary.excluded_component_count,
       summary.missing_or_conditioned_component_count,
       summary.non_detect_component_count,
       summary.included_weight_sum,
       summary.contributing_component_count,
       summary.excluded_observation_numbers
     FROM discrete.samples s
     JOIN discrete.result_aggregation_summary summary
       ON summary.sample_id = s.sample_id
     WHERE s.import_source = 'synthetic_fixture'
       AND s.import_source_id = 'SYN-S9'
     ORDER BY summary.aggregation_type"
  )
  expect_identical(aggregations$aggregation_type, c("mean", "weighted_mean"))
  expect_equal(aggregations$stored_result, c(100, 102.5))
  expect_true(all(aggregations$result_is_current))

  mean_result <- aggregations[aggregations$aggregation_type == "mean", ]
  expect_equal(mean_result$component_count, 10)
  expect_equal(mean_result$expected_count, 10)
  expect_equal(mean_result$included_component_count, 9)
  expect_equal(mean_result$excluded_component_count, 1)
  expect_identical(mean_result$excluded_observation_numbers[[1]], 10L)

  weighted_result <- aggregations[
    aggregations$aggregation_type == "weighted_mean",
  ]
  expect_equal(weighted_result$component_count, 3)
  expect_equal(weighted_result$expected_count, 4)
  expect_equal(weighted_result$missing_component_count, 1)
  expect_true(weighted_result$has_component_shortfall)
  expect_equal(weighted_result$missing_or_conditioned_component_count, 1)
  expect_equal(weighted_result$non_detect_component_count, 1)
  expect_equal(weighted_result$included_weight_sum, 4)
  expect_equal(weighted_result$contributing_component_count, 3)

  metadata_cardinality <- DBI::dbGetQuery(
    con,
    "SELECT
       (SELECT count(*)
        FROM discrete.samples_metadata_en
        WHERE import_source = 'synthetic_fixture'
          AND import_source_id = 'SYN-S9') AS sample_rows,
       (SELECT count(*)
        FROM discrete.results_metadata_en
        WHERE sample_import_source = 'synthetic_fixture'
          AND sample_import_source_id = 'SYN-S9') AS result_rows"
  )
  expect_equal(metadata_cardinality$sample_rows, 1)
  expect_equal(metadata_cardinality$result_rows, 2)
})
