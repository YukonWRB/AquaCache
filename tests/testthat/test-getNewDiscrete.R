test_that("getNewDiscrete inserts composites without updating existing samples", {
  testthat::skip_on_cran()

  con <- connect_test()
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = TRUE)

  if (aquacache_db_patch_number(con) < 60L) {
    skip("Composite discrete results require database Patch 60.")
  }

  dbTransBegin(con)
  on.exit(DBI::dbExecute(con, "ROLLBACK;"), add = TRUE, after = FALSE)

  ids <- DBI::dbGetQuery(
    con,
    "SELECT
       (SELECT media_id FROM public.media_types
        WHERE media_type = 'snow' LIMIT 1) AS media_id,
       (SELECT collection_method_id FROM discrete.collection_methods
        ORDER BY collection_method_id LIMIT 1) AS collection_method,
       (SELECT sample_type_id FROM discrete.sample_types
        WHERE NOT requires_location AND requires_sample_group
        ORDER BY sample_type_id LIMIT 1) AS sample_type,
       (SELECT default_owner FROM discrete.sample_series
        ORDER BY sample_series_id LIMIT 1) AS owner,
       (SELECT result_type_id FROM discrete.result_types
        WHERE result_type = 'field' LIMIT 1) AS result_type,
       (SELECT parameter_id FROM public.parameters
        WHERE param_name = 'snow water equivalent' LIMIT 1) AS parameter_id,
       (SELECT sample_fraction_id FROM discrete.sample_fractions
        WHERE sample_fraction = 'total' LIMIT 1) AS sample_fraction_id,
       (SELECT result_value_type_id FROM discrete.result_value_types
        WHERE result_value_type = 'Calculated' LIMIT 1) AS result_value_type,
       (SELECT matrix_state_id FROM public.matrix_states
        WHERE matrix_state_code = 'solid' LIMIT 1) AS matrix_state_id"
  )
  expect_false(anyNA(ids))

  qualifier_ids <- DBI::dbGetQuery(
    con,
    "SELECT qualifier_type_id
     FROM public.qualifier_types
     ORDER BY qualifier_type_id
     LIMIT 2"
  )$qualifier_type_id
  expect_length(qualifier_ids, 2L)

  observer_id <- DBI::dbGetQuery(
    con,
    "INSERT INTO instruments.observers (
       observer_first, observer_last, organization
     ) VALUES ('Composite', 'Import Test', $1)
     RETURNING observer_id",
    params = list(ids$owner[[1]])
  )$observer_id[[1]]

  DBI::dbExecute(
    con,
    "UPDATE discrete.sample_series
     SET synch_to = NULL
     WHERE sample_series_id = 1"
  )
  DBI::dbExecute(
    con,
    "UPDATE discrete.sample_series_source_adapters
     SET source_fx = 'downloadECCCwq',
         source_fx_args = NULL,
         fetch_priority = 1,
         active = true
     WHERE sample_series_id = 1"
  )

  source_tag <- format(Sys.time(), "%Y%m%d%H%M%OS6")
  existing_source_id <- paste0("existing-composite-", source_tag)
  new_source_id <- paste0("new-composite-", source_tag)
  old_group_code <- paste0("old-group-", source_tag)
  replacement_group_code <- paste0("replacement-group-", source_tag)
  new_group_code <- paste0("new-group-", source_tag)

  sample_row <- data.frame(
    location_id = NA_integer_,
    media_id = ids$media_id,
    datetime = as.POSIXct("2030-03-15 15:00:00", tz = "UTC"),
    collection_method = ids$collection_method,
    sample_type = ids$sample_type,
    owner = ids$owner,
    note = "Original existing sample",
    import_source = "downloadECCCwq",
    import_source_id = existing_source_id
  )
  direct_result <- data.frame(
    result_type = ids$result_type,
    parameter_id = ids$parameter_id,
    sample_fraction_id = ids$sample_fraction_id,
    result = 5,
    result_value_type = ids$result_value_type,
    matrix_state_id = ids$matrix_state_id
  )
  existing_sample_id <- addNewDiscrete(
    con = con,
    sample = sample_row,
    results = direct_result,
    sample_groups = data.frame(
      group_type = "trip",
      group_code = old_group_code
    ),
    sample_qualifiers = data.frame(
      qualifier_type_id = qualifier_ids[[1]],
      note = "Original qualifier"
    )
  )

  aggregate_result <- direct_result
  aggregate_result$result <- NA_real_
  aggregations <- data.frame(
    result_row = 1L,
    aggregation_type = "mean",
    expected_count = 2L
  )
  components <- data.frame(
    result_row = 1L,
    observation_number = 1:2,
    result = c(9, 11)
  )
  changed_existing_sample <- sample_row
  changed_existing_sample$note <- "Source attempted to replace existing data"
  changed_existing_sample$import_source <- NULL
  new_sample <- changed_existing_sample
  new_sample$datetime <- new_sample$datetime + 60
  new_sample$note <- "New composite sample"
  new_sample$import_source_id <- new_source_id

  source_records <- list(
    list(
      sample = changed_existing_sample,
      results = aggregate_result,
      sample_groups = data.frame(
        group_type = "trip",
        group_code = replacement_group_code
      ),
      sample_qualifiers = data.frame(
        qualifier_type_id = qualifier_ids[[2]],
        note = "Replacement qualifier"
      ),
      sample_observers = data.frame(
        observer_id = observer_id,
        observer_role = "sampler"
      ),
      result_aggregations = aggregations,
      result_components = components
    ),
    list(
      sample = new_sample,
      results = aggregate_result,
      sample_groups = data.frame(
        group_type = "trip",
        group_code = new_group_code
      ),
      sample_qualifiers = data.frame(
        qualifier_type_id = qualifier_ids[[2]],
        note = "New composite qualifier"
      ),
      sample_observers = data.frame(
        observer_id = observer_id,
        observer_role = "sampler"
      ),
      result_aggregations = aggregations,
      result_components = components
    )
  )
  local_mocked_bindings(
    downloadECCCwq = function(...) source_records,
    getSourceAdapterCapabilities = function(...) {
      data.table::data.table(source_fx = "downloadECCCwq")
    },
    .package = "AquaCache"
  )

  imported <- getNewDiscrete(con = con, sample_series_id = 1L)

  expect_identical(imported$action, c("existing", "inserted"))
  expect_identical(imported$sample_id[[1]], existing_sample_id)

  existing_state <- DBI::dbGetQuery(
    con,
    "SELECT
       s.note,
       r.result,
       count(DISTINCT sq.qualifier_type_id) AS qualifier_count,
       count(DISTINCT so.observer_id) AS observer_count,
       count(DISTINCT ra.result_id) AS aggregation_count,
       count(DISTINCT rc.result_component_id) AS component_count
     FROM discrete.samples s
     JOIN discrete.results r USING (sample_id)
     LEFT JOIN discrete.sample_qualifiers sq USING (sample_id)
     LEFT JOIN discrete.sample_observers so USING (sample_id)
     LEFT JOIN discrete.result_aggregations ra USING (result_id)
     LEFT JOIN discrete.result_components rc USING (result_id)
     WHERE s.sample_id = $1
     GROUP BY s.sample_id, s.note, r.result",
    params = list(existing_sample_id)
  )
  expect_identical(existing_state$note, "Original existing sample")
  expect_equal(existing_state$result, 5)
  expect_equal(existing_state$qualifier_count, 1)
  expect_equal(existing_state$observer_count, 0)
  expect_equal(existing_state$aggregation_count, 0)
  expect_equal(existing_state$component_count, 0)
  expect_equal(
    DBI::dbGetQuery(
      con,
      "SELECT count(*) FROM discrete.sample_groups WHERE group_code = $1",
      params = list(replacement_group_code)
    )[[1]],
    0
  )

  new_state <- DBI::dbGetQuery(
    con,
    "SELECT
       s.note,
       summary.stored_result,
       summary.component_count,
       count(DISTINCT sq.qualifier_type_id) AS qualifier_count,
       count(DISTINCT so.observer_id) AS observer_count
     FROM discrete.samples s
     JOIN discrete.results r USING (sample_id)
     JOIN discrete.result_aggregation_summary summary
       ON summary.result_id = r.result_id
     LEFT JOIN discrete.sample_qualifiers sq ON sq.sample_id = s.sample_id
     LEFT JOIN discrete.sample_observers so ON so.sample_id = s.sample_id
     WHERE s.sample_id = $1
     GROUP BY s.sample_id, s.note, summary.stored_result,
       summary.component_count",
    params = list(imported$sample_id[[2]])
  )
  expect_identical(new_state$note, "New composite sample")
  expect_equal(new_state$stored_result, 10)
  expect_equal(new_state$component_count, 2)
  expect_equal(new_state$qualifier_count, 1)
  expect_equal(new_state$observer_count, 1)
})
