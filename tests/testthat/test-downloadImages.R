test_that("downloadNupointImages accepts explicit connection arguments", {
  cache_dir <- file.path(tempdir(), "downloadNupointImages")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)
  save_dir <- tempfile("nupoint-images-")

  withr::local_envvar(c(
    nupointUser = NA,
    nupointPass = NA,
    nupointServer = NA,
    nupointPort = NA,
    nupointFolder = NA
  ))
  local_mocked_bindings(
    sftp_connect = function(...) structure(list(), class = "mock_sftp"),
    sftp_listfiles = function(...) {
      data.frame(name = "SITE_20260812120000.jpg")
    },
    sftp_download = function(file, tofolder, ...) {
      dir.create(dirname(file.path(tofolder, file)), recursive = TRUE, showWarnings = FALSE)
      writeBin(as.raw(c(1, 2, 3)), file.path(tofolder, file))
      1L
    },
    .package = "sftp"
  )

  result <- downloadNupointImages(
    location = "SITE",
    start_datetime = as.POSIXct("2026-08-12 11:00:00", tz = "UTC"),
    username = "explicit-user",
    password = "explicit-password",
    url = "sftp.example.test",
    port = 22,
    folder = "images",
    save_path = save_dir,
    delete = FALSE
  )

  expect_equal(
    normalizePath(dirname(result$file[[1]]), winslash = "/"),
    normalizePath(save_dir, winslash = "/")
  )
  expect_true(file.exists(result$file[[1]]))
})

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
  expect_identical(readBin(saved_file, "raw", n = file.size(saved_file)), image_content)
  expect_identical(result[[1]]$content, image_content)
})
