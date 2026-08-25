test_that("downloadWSCImages saves a copy without changing response content", {
  cache_dir <- file.path(tempdir(), "downloadWSCImages")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)
  save_dir <- tempfile("wsc-images-")
  image_name <- "09AA001_camera_20260812T120000Z.jpg"
  image_content <- as.raw(c(10, 20, 30))

  local_mocked_bindings(
    session = function(...) structure(list(), class = "mock_session"),
    html_elements = function(x, ...) x,
    html_attr = function(x, ...) image_name,
    .package = "rvest"
  )
  local_mocked_bindings(
    GET = function(url, ...) {
      structure(
        list(url = url, content = image_content),
        class = "response"
      )
    },
    .package = "httr"
  )

  result <- downloadWSCImages(
    location = "09AA001",
    start_datetime = as.POSIXct("2026-08-12 11:00:00", tz = "UTC"),
    username = "user",
    password = "password",
    url = "https://example.test/images",
    save_path = save_dir
  )

  saved_file <- file.path(save_dir, image_name)
  expect_true(file.exists(saved_file))
  expect_identical(
    readBin(saved_file, "raw", n = file.size(saved_file)),
    image_content
  )
  expect_identical(result[[1]]$content, image_content)
})
