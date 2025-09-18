mock_tidyhydat <- function(mocks, code) {
  ns <- "tidyhydat"
  loadNamespace(ns)
  old <- lapply(names(mocks), function(name) getFromNamespace(name, ns))
  names(old) <- names(mocks)
  on.exit({
    for (name in names(old)) {
      assignInNamespace(name, old[[name]], ns = ns)
    }
  }, add = TRUE)
  for (name in names(mocks)) {
    assignInNamespace(name, mocks[[name]], ns = ns)
  }
  force(code)
}



test_that("hydat_check does not download when versions match", {
  skip_if_not_installed("tidyhydat")
  temp_file <- tempfile(fileext = ".sqlite")
  writeLines("fake", temp_file)
  download_called <- FALSE

  mock_tidyhydat(list(
    hy_downloaded_db = function() temp_file,
    hy_version = function(path) data.frame(Date = as.Date("2024-01-01")),
    hy_remote = function() "20240101",
    download_hydat = function(ask = FALSE) download_called <<- TRUE
  ), {
    expect_false(hydat_check(silent = TRUE))
    expect_false(download_called)
  })
})


test_that("hydat_check downloads when local copy missing", {
  skip_if_not_installed("tidyhydat")
  state <- new.env(parent = emptyenv())
  state$downloaded <- FALSE
  downloaded_path <- tempfile(fileext = ".sqlite")

  mock_tidyhydat(list(
    hy_downloaded_db = function() {
      if (state$downloaded) {
        downloaded_path
      } else {
        file.path(tempdir(), "missing_hydat.sqlite")
      }
    },
    hy_version = function(path) data.frame(Date = as.Date("2024-02-02")),
    hy_remote = function() "20240202",
    download_hydat = function(ask = FALSE) {
      state$downloaded <- TRUE
      writeLines("fake", downloaded_path)
    }
  ), {
    expect_true(hydat_check(silent = TRUE))
    expect_true(state$downloaded)
  })
})
