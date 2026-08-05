test_that("dailyUpdate relies on getNewContinuous for continuous stats updates", {
  captured <- new.env(parent = emptyenv())

  local_mocked_bindings(
    getNewContinuous = function(con, timeseries_id, stats = FALSE, ...) {
      captured$getnew_args <- list(
        con = con,
        timeseries_id = timeseries_id
      )
      data.frame(timeseries_id = timeseries_id)
    },
    .package = "AquaCache"
  )
  local_mocked_bindings(
    dbGetQuery = function(con, statement, ...) {
      if (
        grepl(
          "FROM continuous.timeseries t",
          statement,
          fixed = TRUE
        )
      ) {
        return(data.frame(
          timeseries_id = c(101L, 202L),
          last_daily_calculation = as.POSIXct(
            c(NA, NA),
            tz = "UTC"
          ),
          active = c(TRUE, TRUE)
        ))
      }

      stop(sprintf("Unexpected dbGetQuery statement in test: %s", statement))
    },
    dbExecute = function(con, statement, ...) 1L,
    .package = "DBI"
  )

  expect_no_error(
    dailyUpdate(
      con = structure(list(), class = "mock_con"),
      continuous = TRUE,
      discrete = FALSE,
      hydat = FALSE,
      images = FALSE,
      rasters = FALSE
    )
  )

  expect_equal(captured$getnew_args$timeseries_id, c(101L, 202L))
  expect_true(isTRUE(captured$getnew_args$stats))
})
