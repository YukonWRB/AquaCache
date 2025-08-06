skip_if_offline()

start_dt <- Sys.time() - 60 * 60 * 24 * 7 # 7 days ago
end_dt <- Sys.time() # now

# Ensure that WSC can be reached using fread

baseurl <- "https://wateroffice.ec.gc.ca/services/real_time_data/csv/inline?"
location_string <- "stations[]=09EA004"
parameters_string <- "parameters[]=47"
datetime_string <- paste0("start_date=", substr(start_dt, 1, 10), "%20", substr(start_dt, 12, 19), "&end_date=", substr(end_dt, 1, 10), "%20", substr(end_dt, 12, 19))
url <- paste0(baseurl, location_string, "&", parameters_string, "&", datetime_string)

suppressWarnings(
  tryCatch({
  test <- data.table::fread(url)
  if (nrow(test) == 0) {
    skip_test <- TRUE
  } else {
      skip_test <- FALSE
  }
}, error = function(e) {
  skip_test <<- TRUE
})
)

if (skip_test) skip("WSC data cannot be fetched, skipping downloadWSC tests")

test_that("downloadWSC fetches data", {
  con <- create_test_con()

  res <- downloadWSC(location = "09EA004", parameter = 47, start_datetime = start_dt, end_datetime = end_dt, con)
  
  expect_gt(nrow(res), 0)
  expect_s3_class(res$datetime, "POSIXct")
  expect_named(res, c("datetime", "value", "grade", "approval", "qualifier", "owner", "contributor"))
  DBI::dbDisconnect(con)
})
