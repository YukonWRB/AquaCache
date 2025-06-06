#' Check and update HYDAT
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Checks and, if necessary, updates the local version of the WSC HYDAT database. Intended for use on a schedule to ensure the user always has the latest version.
#'
#' @param silent Should messages be printed to the console?
#'
#' @return TRUE if hydat was updated, FALSE if not. Also an updated local copy of hydat, if indicated by the age of the remote HYDAT being younger than the local hydat.
#' @export
#'

hydat_check <- function(silent = FALSE){
  
  tryCatch({
    hydat_path <- tidyhydat::hy_downloaded_db() #Attempts to get the hydat path, in case it's downloaded already.
    if (!file.exists(hydat_path)) {
      hydat_path <- NULL
    }
  }, error = function(e) {
    hydat_path <- NULL
  })
  
  new_hydat <- FALSE
  if (!is.null(hydat_path)) { #If hydat already exists, compare version numbers
    local_hydat <- as.Date(tidyhydat::hy_version(hydat_path)$Date)
    local_hydat <- gsub("-", "", as.character(local_hydat))
    remote_hydat <- tidyhydat::hy_remote()
    if (local_hydat != remote_hydat) { #if remote version is not the same, download new version
      try(tidyhydat::download_hydat(ask = FALSE))
      hydat_path <- tidyhydat::hy_downloaded_db() #reset the hydat path just in case the new DB is not named exactly as the old one (guard against tidyhydat package changes in future)
      local_hydat <- as.Date(tidyhydat::hy_version(hydat_path)$Date) #check the HYDAT version again just in case. It can fail to update without actually creating an error and stopping.
      local_hydat <- gsub("-", "", as.character(local_hydat))
      if (local_hydat == remote_hydat) {
        new_hydat <- TRUE
        if (!silent) {
          message("The local WSC HYDAT database was updated.")
        }
        updated <- TRUE
      } else {
        warning("Failed to update the local HYDAT database. There is probably an active connection to the database preventing an overwrite.")
        updated <- FALSE
      }
    } else {
      updated <- FALSE
    }
  } else if (is.null(hydat_path)) {# if hydat does not already exist, download fresh to the default location
    tidyhydat::download_hydat(ask = FALSE)
    hydat_path <- tidyhydat::hy_downloaded_db()
    new_hydat <- TRUE
    local_hydat <- as.Date(tidyhydat::hy_version(hydat_path)$Date)
    local_hydat <- gsub("-", "", as.character(local_hydat))
    if (!silent) {
      message("A local copy of the WSC HYDAT database was installed.")
    }
    updated <- TRUE
  } else {
    updated <- FALSE
  }
  
  if (!silent) {
    message("hydat_check completed.")
  }
  return(updated)
}
