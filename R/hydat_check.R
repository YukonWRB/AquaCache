#' Check and update HYDAT
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Checks and, if necessary, updates the local version of the WSC HYDAT database. Intended for use on a schedule to ensure the user always has the latest version.
#'
#' @param silent Should messages be printed to the console?
#'
#' @return An updated local copy of hydat, if indicated by the age of the remote HYDAT.
#' @export
#'

hydat_check <- function(silent = FALSE){

  #initial checks
  rlang::check_installed("tidyhydat", reason = "Package tidyhydat is required to use function hydat_check") #This is here because tidyhydat is not a 'depends' of this package; it is only necessary for this function and is therefore in "suggests"

  tryCatch({hydat_path <- tidyhydat::hy_downloaded_db() #Attempts to get the hydat path, in case it's downloaded already.
  }, error = function(e) {hydat_path <- NULL})
  new_hydat <- FALSE
  if (!is.null(hydat_path)){ #If hydat already exists, compare version numbers
    local_hydat <- as.Date(tidyhydat::hy_version(hydat_path)$Date)
    local_hydat <- gsub("-", "", as.character(local_hydat))
    remote_hydat <- tidyhydat::hy_remote()
    if (local_hydat != remote_hydat){ #if remote version is not the same, download new version
      try(tidyhydat::download_hydat(ask=FALSE))
      hydat_path <- tidyhydat::hy_downloaded_db() #reset the hydat path just in case the new DB is not named exactly as the old one (guard against tidyhydat package changes in future)
      local_hydat <- as.Date(tidyhydat::hy_version(hydat_path)$Date) #check the HYDAT version again just in case. It can fail to update without actually creating an error and stopping.
      local_hydat <- gsub("-", "", as.character(local_hydat))
      if (local_hydat == remote_hydat){
        new_hydat <- TRUE
        if (!silent){
          message("The local WSC HYDAT database was updated.")
        }
      } else {
        if (!silent){
          warning("Failed to update the local HYDAT database. There is probably an active connection to the database preventing an overwrite.")
        }
      }
    }
  } else if (is.null(hydat_path)) {# if hydat does not already exist, download fresh to the default location
    tidyhydat::download_hydat(ask=FALSE)
    hydat_path <- tidyhydat::hy_downloaded_db()
    new_hydat <- TRUE
    local_hydat <- as.Date(tidyhydat::hy_version(hydat_path)$Date)
    local_hydat <- gsub("-", "", as.character(local_hydat))
    if (!silent){
      message("A local copy of the WSC HYDAT database was installed.")
    }
  }

  if (!silent){
    message("hydat_check completed.")
  }
}
