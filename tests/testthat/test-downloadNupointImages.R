test_that("downloadNupointImages accepts explicit connection arguments", {
  cache_dir <- file.path(tempdir(), "downloadNupointImages")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)
  save_dir <- tempfile("nupoint-images-")
  image_name <- "SITE_20260812120000.jpg"
  image_content <- as.raw(c(1, 2, 3))

  withr::local_envvar(c(
    nupointUser = NA,
    nupointPass = NA,
    nupointServer = NA,
    nupointPort = NA,
    nupointFolder = NA
  ))
  local_mocked_bindings(
    curl_fetch_memory = function(url, handle = NULL) {
      list(
        status_code = 200,
        content = charToRaw(paste0(image_name, "\n"))
      )
    },
    curl_fetch_disk = function(url, path, handle = NULL) {
      writeBin(image_content, path)
      list(status_code = 200)
    },
    .package = "curl"
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
  expect_identical(
    readBin(result$file[[1]], "raw", n = file.size(result$file[[1]])),
    image_content
  )
})
