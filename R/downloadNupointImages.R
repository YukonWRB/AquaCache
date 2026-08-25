#' Get images from NuPoint SFTP server
#'
#' Fetches images from a NuPoint SFTP server. To fetch from the WSC instead see
#' [downloadWSCImages()]. Intended to be called by function [getNewImages()].
#' Use caution: this function will delete files from the SFTP server after
#' fetching if `delete = TRUE` (the default).
#'
#' @param location The location for which to get images.
#' @param start_datetime The earliest datetime to start pulling images from.
#' @param username Username to use for password-protected login.
#' @param password Password to use for protected login.
#' @param url The URL or IP address from which to get new images.
#' @param port The port on which to connect to the SFTP server.
#' @param folder The folder in which to look for new images.
#' @param save_path Optional; path in which to save the image.
#' @param delete Should the files be deleted from the SFTP site once they've
#'   been successfully fetched?
#'
#' @return A data.frame containing downloaded file paths, datetimes, and
#'   locations, or NULL if no matching files are found.
#' @export
#'

downloadNupointImages <- function(
  location,
  start_datetime,
  username = Sys.getenv("nupointUser"),
  password = Sys.getenv("nupointPass"),
  url = Sys.getenv("nupointServer"),
  port = Sys.getenv("nupointPort"),
  folder = Sys.getenv("nupointFolder"),
  save_path = NULL,
  delete = TRUE
) {
  if (!inherits(start_datetime, "POSIXct")) {
    stop("Parameter start_datetime must be a POSIXct.")
  }

  connection_values <- list(
    username = username,
    password = password,
    url = url,
    port = port,
    folder = folder
  )

  missing_values <- vapply(
    connection_values,
    function(value) {
      length(value) != 1L ||
        is.na(value) ||
        !nzchar(trimws(as.character(value)))
    },
    logical(1)
  )

  if (any(missing_values)) {
    stop(
      "Missing required NuPoint connection argument(s): ",
      paste(names(connection_values)[missing_values], collapse = ", "),
      ". Set the corresponding nupoint environment variables or provide ",
      "the arguments explicitly."
    )
  }

  # Normalize SFTP connection values
  server <- sub("^sftp://", "", trimws(as.character(url)))
  server <- sub("/+$", "", server)

  port <- as.integer(port)
  if (is.na(port)) {
    stop("Parameter port must be an integer.")
  }

  folder <- gsub("^/+|/+$", "", trimws(as.character(folder)))

  # URL-encode each directory component independently.
  encode_path <- function(x) {
    parts <- strsplit(x, "/", fixed = TRUE)[[1]]
    paste(
      vapply(parts, curl::curl_escape, character(1)),
      collapse = "/"
    )
  }

  folder_url <- encode_path(folder)

  root_url <- sprintf(
    "sftp://%s:%d/",
    server,
    port
  )

  folder_url <- sprintf(
    "%s%s/",
    root_url,
    folder_url
  )

  # Set up cache/download directories
  cache_dir <- file.path(tempdir(), "downloadNupointImages")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  download_dir <- cache_dir

  if (!is.null(save_path)) {
    if (
      length(save_path) != 1L ||
        !is.character(save_path) ||
        is.na(save_path) ||
        !nzchar(trimws(save_path))
    ) {
      stop("Parameter save_path must be a single non-empty directory path.")
    }

    if (!dir.exists(save_path)) {
      dir.create(save_path, recursive = TRUE, showWarnings = FALSE)
    }

    if (!dir.exists(save_path)) {
      stop("Could not create save_path directory: ", save_path)
    }

    download_dir <- normalizePath(save_path, mustWork = TRUE)
  }

  # One handle is reused for listing and downloads. Reusing the handle allows
  # libcurl to reuse the SSH connection.
  handle <- curl::new_handle(
    username = username,
    password = password,
    timeout = 120
  )

  # Check for a recent cached directory listing.
  #
  # Restrict this explicitly to .rds files: when save_path is NULL,
  # downloaded images also live in cache_dir.
  saved_files <- list.files(
    cache_dir,
    pattern = "^[0-9]{12}\\.rds$"
  )

  file_exists <- FALSE

  if (length(saved_files) > 0L) {
    saved_datetimes <- as.POSIXct(
      sub("\\.rds$", "", saved_files),
      format = "%Y%m%d%H%M",
      tz = "UTC"
    )

    fresh <- !is.na(saved_datetimes) &
      saved_datetimes > Sys.time() - 2 * 60

    if (any(fresh)) {
      fresh_files <- saved_files[fresh]
      fresh_datetimes <- saved_datetimes[fresh]

      target_file <- fresh_files[
        which.max(fresh_datetimes)
      ]

      tbl <- readRDS(
        file.path(cache_dir, target_file)
      )

      file_exists <- TRUE
    }
  }

  # Fetch a new directory listing if necessary.
  if (!file_exists) {
    curl::handle_setopt(
      handle,
      dirlistonly = TRUE
    )

    response <- curl::curl_fetch_memory(
      folder_url,
      handle = handle
    )

    links <- strsplit(
      rawToChar(response$content),
      "\n",
      fixed = TRUE
    )[[1]]

    # Handle either Unix or Windows-style line endings.
    links <- sub("\r$", "", links)

    # Remove empty entries and directory aliases.
    links <- links[
      nzchar(links) &
        !links %in% c(".", "..")
    ]

    tbl <- data.frame(
      link = links,
      datetime = as.POSIXct(
        sub(".*_(\\d{14}).*", "\\1", links),
        format = "%Y%m%d%H%M%S",
        tz = "UTC"
      ),
      location = sub(
        "^(.*)_\\d{14}.*$",
        "\\1",
        links
      ),
      stringsAsFactors = FALSE
    )

    saveRDS(
      tbl,
      file.path(
        cache_dir,
        paste0(
          format(
            Sys.time(),
            "%Y%m%d%H%M",
            tz = "UTC"
          ),
          ".rds"
        )
      )
    )

    # Turn directory-listing mode back off before downloading files.
    curl::handle_setopt(
      handle,
      dirlistonly = FALSE
    )
  }

  # Select requested images.
  tbl <- tbl[
    !is.na(tbl$datetime) &
      tbl$location == location &
      tbl$datetime >= start_datetime,
    ,
    drop = FALSE
  ]

  if (nrow(tbl) == 0L) {
    return(NULL)
  }

  # Use a separate persistent handle for deletion.
  #
  # The delete request is made only after curl_fetch_disk() succeeds.
  if (delete) {
    delete_handle <- curl::new_handle(
      username = username,
      password = password,
      timeout = 120,
      nobody = TRUE
    )
  }

  downloaded_files <- character(nrow(tbl))

  for (i in seq_len(nrow(tbl))) {
    file <- tbl$link[[i]]

    remote_url <- paste0(
      folder_url,
      curl::curl_escape(file)
    )

    local_file <- file.path(
      download_dir,
      file
    )

    # Download directly to disk rather than loading the JPEG into memory.
    curl::curl_fetch_disk(
      remote_url,
      local_file,
      handle = handle
    )

    downloaded_files[[i]] <- local_file

    # Delete only after a successful download.
    if (delete) {
      remote_path <- paste0(
        "/",
        folder,
        "/",
        file
      )

      curl::handle_setopt(
        delete_handle,
        quote = sprintf(
          'rm "%s"',
          remote_path
        )
      )

      invisible(
        curl::curl_fetch_memory(
          root_url,
          handle = delete_handle
        )
      )
    }
  }

  data.frame(
    file = downloaded_files,
    datetime = tbl$datetime,
    location = tbl$location,
    stringsAsFactors = FALSE
  )
}
