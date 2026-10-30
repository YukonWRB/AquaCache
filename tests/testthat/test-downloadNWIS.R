test_that("downloadNWIS fetches data", {
  testthat::skip_on_cran()

  con <- connect_test()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  local_mocked_bindings(
    readNWISdata = function(sites, ...) {
      datetime <- seq(
        as.POSIXct("2025-09-09 00:00:00", tz = "UTC"),
        by = "15 min",
        length.out = 96L
      )
      data.frame(
        agency_cd = rep("USGS", length(datetime)),
        site_no = rep(sites, length.out = length(datetime)),
        datetime = datetime,
        value = seq_along(datetime),
        qualifier = rep("A", length(datetime)),
        check.names = FALSE
      )
    },
    .package = "dataRetrieval"
  )

  res <- downloadNWIS(
    location = "15041200",
    parameter = "00060",
    start_datetime = "2025-09-09 00:00:00",
    end_datetime = "2025-09-09 23:59:59",
    con = con
  )

  expect_equal(nrow(res), 96)
  # res$datetime is POSIXct
  expect_s3_class(res$datetime, "POSIXct")
  # res$datetime is in UTC timezone
  expect_equal(attr(res$datetime, "tzone"), "UTC")
  expect_named(
    res,
    c(
      "datetime",
      "value",
      "approval",
      "qualifier"
    )
  )
})
