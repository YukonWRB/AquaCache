#' Get realtime data from the WSC
#'
#'@description
#'`r lifecycle::badge("stable")`
#'
#' A fast, pared down method of fetching WSC realtime data (at least compared to tidyhydat and tidyhydat.ws options). Dispenses with extra columns that those packages include and uses data.table::fread to speed up parsing.
#'
#' @param location A WSC station number.
#' @param parameter_id A WSC parameter code. 47 for discharge primary (sensor derived), 8 for discharge (sensor measured), 46 for level, 5 for water temperature, 4 for air temperature. See the full list using [tidyhydat::param_id].
#' @param start_datetime Specify as class Date, POSIXct OR as character string which can be interpreted as POSIXct. If character, UTC offset of 0 will be assigned, otherwise conversion to UTC 0 will be performed on POSIXct class input. If date, time will default to 00:00 to capture whole day.
#' @param end_datetime Specify as class Date, POSIXct OR as character string which can be interpreted as POSIXct. If character, UTC offset of 0 will be assigned, otherwise conversion to UTC 0 will be performed on POSIXct class input. If Date, time will default to 23:59:59 to capture whole day.
#' @param con A connection to the AquaCache database, necessary to allow for the mapping of Aquarius approvals, grades, and qualifiers to the database. If left NULL connection will be made and closed automatically.
#'
#' @return A data.table object of hydrometric data, with datetimes in UTC-0.
#' @export

downloadWSC <- function(location, parameter_id, start_datetime, end_datetime = Sys.time(), con = NULL)
{
  
  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }
  
  # Checking start_datetime parameter
  tryCatch({
    if (inherits(start_datetime, "character") & nchar(start_datetime) > 10) { #Does not necessarily default to 0 hour.
      start_datetime <- as.POSIXct(start_datetime, tz = "UTC")
    } else if (inherits(start_datetime, "POSIXct")) {
      attr(start_datetime, "tzone") <- "UTC"
    } else if (inherits(start_datetime, "Date") | (inherits(start_datetime, "character") & nchar(start_datetime) == 10)) { #defaults to 0 hour
      start_datetime <- as.POSIXct(start_datetime, tz = "UTC")
    } else {
      stop("Parameter start_datetime could not be coerced to POSIXct.")
    }
  }, error = function(e) {
    stop("Failed to convert parameter start_datetime to POSIXct.")
  })

  # Checking end_datetime parameter
  tryCatch({
    if (inherits(end_datetime, "character") & nchar(end_datetime) > 10) { #Does not necessarily default to 0 hour.
      end_datetime <- as.POSIXct(end_datetime, tz = "UTC")
    } else if (inherits(end_datetime, "POSIXct")) {
      attr(end_datetime, "tzone") <- "UTC"
    } else if (inherits(end_datetime, "Date") | (inherits(end_datetime, "character") & nchar(end_datetime) == 10)) { #defaults to very end of day
      end_datetime <- as.POSIXct(end_datetime, tz = "UTC")
      end_datetime <- end_datetime + 60*60*23.9999
    } else {
      stop("Parameter end_datetime could not be coerced to POSIXct.")
    }
  }, error = function(e) {
    stop("Failed to convert parameter end_datetime to POSIXct.")
  })

  if (nchar(as.character(start_datetime)) == 10) {
    start_datetime <- paste0(start_datetime, " 00:00:00")
  }
  if (nchar(as.character(end_datetime)) == 10) {
    end_datetime <- paste0(end_datetime, " 00:00:00")
  }

  # Pull data from WSC
  baseurl <- "https://wateroffice.ec.gc.ca/services/real_time_data/csv/inline?"
  location_string <- paste0("stations[]=", location)
  parameters_string <- paste0("parameters[]=", parameter_id)
  datetime_string <- paste0("start_date=", substr(start_datetime, 1, 10), "%20", substr(start_datetime, 12, 19), "&end_date=", substr(end_datetime, 1, 10), "%20", substr(end_datetime, 12, 19))
  url <- paste0(baseurl, location_string, "&", parameters_string, "&", datetime_string)

  data <- data.table::fread(url, showProgress = FALSE, data.table = FALSE, select = c("Date" = "POSIXct", "Value/Valeur" = "numeric", "Symbol/Symbole" = "character", "Approval/Approbation" = "character", "Qualifier/Qualificatif" = "character"), col.names = c("datetime", "value", "grade", "approval", "qualifier"))
  
  if (nrow(data) > 0) {
    qualifiers_DB <- DBI::dbGetQuery(con, "SELECT * FROM qualifier_types")
    qualifier_mapping <- c("-1" = qualifiers_DB[qualifiers_DB$qualifier_type_code == "UNS", "qualifier_type_id"],
                           "10" = qualifiers_DB[qualifiers_DB$qualifier_type_code == "ICE", "qualifier_type_id"],
                           "20" = qualifiers_DB[qualifiers_DB$qualifier_type_code == "EST", "qualifier_type_id"],
                           "30" = qualifiers_DB[qualifiers_DB$qualifier_type_code == "UNK", "qualifier_type_id"],
                           "40" = qualifiers_DB[qualifiers_DB$qualifier_type_code == "DRY", "qualifier_type_id"],
                           "50" = qualifiers_DB[qualifiers_DB$qualifier_type_code == "UNK", "qualifier_type_id"],
                           "-2" = qualifiers_DB[qualifiers_DB$qualifier_type_code == "UNK", "qualifier_type_id"],
                           "0" = qualifiers_DB[qualifiers_DB$qualifier_type_code == "UNK", "qualifier_type_id"])
    data$qualifier <- ifelse(data$qualifier %in% names(qualifier_mapping),
                         qualifier_mapping[data$qualifier],
                         qualifiers_DB[qualifiers_DB$qualifier_type_code == "UNK", "qualifier_type_id"])
    data$qualifier <- as.integer(data$qualifier)
    
    approvals_DB <- DBI::dbGetQuery(con, "SELECT * FROM approval_types")    
    approval_mapping <- c("Final/Finales" = approvals_DB[approvals_DB$qualifier_type_code == "A", "approval_type_id"],
                          "Approved/Approuv\u00E9e" = approvals_DB[approvals_DB$qualifier_type_code == "A", "approval_type_id"],
                          "Provisional/Provisoire" = approvals_DB[approvals_DB$qualifier_type_code == "N", "approval_type_id"],
                          "Preliminary/Pr\u00E9liminaire" = approvals_DB[approvals_DB$qualifier_type_code == "N", "approval_type_id"],
                          "Checked/Verifi\u00E9e" = approvals_DB[approvals_DB$qualifier_type_code == "R", "approval_type_id"],
                          "Unspecified/Non sp\u00E9cifi\u00E9" = approvals_DB[approvals_DB$qualifier_type_code == "UNS", "approval_type_id"],
                          "Undefined/Non d\u00E9fini" = approvals_DB[approvals_DB$qualifier_type_code == "UNS", "approval_type_id"])
    
    data$approval <- ifelse(data$approval %in% names(approval_mapping),
                            approval_mapping[data$approval],
                            "6")
    data$approval <- as.integer(data$approval)
    
    grade_DB <- DBI::dbGetQuery(con, "SELECT grade_type_id FROM grades WHERE grade_type_code = 'UNS'")[1,1]
    data$grade <- grade_DB
    
    # Get owner_contributor_id for 'Water Survey of Canada'
    owner_contributor_id <- DBI::dbGetQuery(con, "SELECT owner_contributor_id FROM owners_contributors WHERE name = 'Water Survey of Canada'")[1,1]
    if (is.na(owner_contributor_id)) {
      df <- data.frame(name = 'Water Survey of Canada')
      DBI::dbAppendTable(con, "owner_contributors", df)
    }
    
    data$owner <- owner_contributor_id
    data$contributor <- owner_contributor_id
    
    return(data)
  } else {
    data <- data.table::data.table()
    return(data)
  }

} #End of function
