addACLocation <- function(df = NULL, location = NA, name = NA, name_fr = NA, latitude = NA, longitude = NA, visibility_public = NA, share_with = NA, owner = NA, data_sharing_agreement_id = NA, location_type = NA, note = NA, contact = NA, datum_id_from = NA, datum_id_to = NA, conversion_m = NA, current = NA) {
  
  
  df <- data.frame(location = "Yukon_Abv_YDA",
                   name = "Yukon River Above Dawson",
                   name_fr = "Riviere Yukon en amont de Dawson",
                   latitude = 63.99564,
                   longitude = -139.65884,
                   visibility_public = "exact",
                   share_with = 1,
                   owner = NA,
                   data_sharing_agreement_id = NA,
                   location_type = 1,
                   datum_id_from = 10,
                   datum_id_to = 10,
                   conversion_m = 0,
                   current = TRUE,
                   note = "NuPoint camera maintained by WSC, no flow/level monitoring.",
                   contact = NA)
  
  location <- NA
  name <- NA
  name_fr <- NA
  latitude <- NA
  longitude <- NA
  visibility_public <- NA
  share_with <- NA
  owner <- NA
  data_sharing_agreement_id <- NA
  location_type <- NA
  note <- NA
  contact <- NA
  datum_id_from <- NA
  datum_id_to <- NA
  conversion_m <- NA
  current <- NA
  
  
  if (!is.null(df)){
    # Check that all other parameters are NA
    if (!all(is.na(c(location, name, name_fr, latitude, longitude, visibility_public, share_with, owner, data_sharing_agreement_id, location_type, note, contact, datum_id_from, datum_id_to, conversion_m, current)))) {
      stop("You cannot provide a data.frame and other parameters at the same time.")
    }
    
    # Check that there is a column name for each function parameter that is not 'df'
    if (!all(c("location", "name", "name_fr", "latitude", "longitude", "visibility_public", "share_with", "owner", "data_sharing_agreement_id", "location_type", "note", "contact", "datum_id_from", "datum_id_to", "conversion_m", "current") %in% colnames(df))) {
      stop("The data.frame provided does not contain all the necessary columns.")
    }
    # Check that the data.frame is not empty
    if (nrow(df) == 0) {
      stop("The data.frame provided is empty.")
    }
    # Assign each column of the data.frame to the corresponding function parameter
    location <- df$location
    name <- df$name
    name_fr <- df$name_fr
    latitude <- df$latitude
    longitude <- df$longitude
    visibility_public <- df$visibility_public
    share_with <- df$share_with
    owner <- df$owner
    data_sharing_agreement_id <- df$data_sharing_agreement_id
    location_type <- df$location_type
    note <- df$note
    contact <- df$contact
    datum_id_from <- df$datum_id_from
    datum_id_to <- df$datum_id_to
    conversion_m <- df$conversion_m
    current <- df$current
  }
  
    # Some parameters can be NA, in which case they get default values
  if (is.na(share_with)) {
    share_with <- 1
  }
  
  # Check that the length of each parameter vector is equal, if not stop

  

}
