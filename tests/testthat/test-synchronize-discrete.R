test_that("discrete synchronization does not reassign sample provenance", {
  local_mocked_bindings(
    dbTransBegin = function(...) FALSE,
    link_discrete_sample_groups = function(...) integer(),
    .package = "AquaCache"
  )

  database_sample <- data.frame(
    sample_id = 1L,
    owner = 1L,
    contributor = 1L,
    source_adapter_function = "adapter_a"
  )
  remote_sample <- data.frame(source_adapter_function = "adapter_b")

  changed <- synchronize_discrete_sample_metadata(
    con = structure(list(), class = "mock_con"),
    database_sample = database_sample,
    remote_sample = remote_sample,
    valid_sample_names = "source_adapter_function",
    sample_groups = NULL,
    default_owner = 1L,
    default_contributor = 1L
  )

  expect_false(changed)
})
