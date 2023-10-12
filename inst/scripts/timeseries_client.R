# Create a simple AQUARIUS Time-Series API client.
timeseriesClient <- methods::setRefClass("timeseriesClient",
                                         fields = list(
                                           version = "character",
                                           publishUri = "character",
                                           acquisitionUri = "character",
                                           provisioningUri = "character",
                                           legacyPublishUri = "character",
                                           legacyAcquisitionUri = "character",
                                           isLegacy = "logical"),
                                         methods = list(
                                           connect = function(hostname, username, password) {
                                             # Support schemeless and schemed hosts for convenience
                                             prefix <- "http://"
                                             if (startsWith(hostname, "http://") || startsWith(hostname, "https://")) {
                                               url <- httr::parse_url(hostname)
                                               hostname <- paste0(url$scheme, "://", url$hostname)
                                               prefix <- ""
                                             }
                                             # Grab the version of the AQTS server
                                             r <- httr::GET(paste0(prefix, hostname, "/AQUARIUS/apps/v1/version"))
                                             httr::stop_for_status(r, "detecting AQTS version")
                                             j <- jsonlite::fromJSON(httr::content(r, "text"))
                                             version <<- j$ApiVersion
                                             # Anything earlier than 14.3 is considered legacy code
                                             isLegacy <<- .self$isVersionLessThan("14.3")
                                             # Compose the base URI for all API endpoints
                                             publishUri <<- paste0(prefix, hostname, "/AQUARIUS/Publish/v2")
                                             acquisitionUri <<- paste0(prefix, hostname, "/AQUARIUS/Acquisition/v2")
                                             provisioningUri <<- paste0(prefix, hostname, "/AQUARIUS/Provisioning/v1")
                                             if (isLegacy) {
                                               legacyPublishUri <<- paste0(prefix, hostname, "/AQUARIUS/Publish/AquariusPublishRestService.svc")
                                               legacyAcquisitionUri <<- paste0(prefix, hostname, "/AQUARIUS/AQAcquisitionService.svc")
                                             }
                                             # Try to authenticate using the supplied credentials
                                             credentials <- list(Username = username, EncryptedPassword = password)
                                             if (isLegacy) {
                                               # Authenticate via the older operation, so that a session cookie is set
                                               r <- httr::GET(paste0(publishUri, "/GetAuthToken"), query = credentials)
                                             } else {
                                               # Authenticate via the preferred endpoint
                                               r <- httr::POST(paste0(publishUri, "/session"), body = credentials, encode = "json")
                                             }
                                             httr::stop_for_status(r, "authenticate with AQTS")
                                           },
                                           disconnect = function() {
                                             if (isLegacy) {
                                               # 3.X doesn't support proper disconnection, so just abandon the session below
                                             } else {
                                               # Delete the session immediately, like we should
                                               r <- httr::DELETE(paste0(publishUri, "/session"))
                                               httr::stop_for_status(r, "disconnect from AQTS")
                                             }
                                             # Abandon all session cookies associated with the connection
                                             httr::handle_reset(publishUri)
                                           },
                                           isVersionLessThan = function(targetVersion, sourceVersion) {
                                             if (missing(sourceVersion)) {
                                               sourceVersion <- version
                                             }
                                             # Create the vectors of integers using this local sanitizing function
                                             createIntegerVector <- function(versionText) {
                                               if (versionText == "0.0.0.0") {
                                                 # Force unreleased developer builds to act as latest-n-greatest
                                                 versionText <- "9999.99"
                                               }
                                               # Convert the text into a vector of integers
                                               v <- as.integer(strsplit(versionText, ".", fixed = TRUE)[[1]])
                                               if (length(v) > 0 && v[1] >= 14 && v[1] <= 99) {
                                                 # Adjust the leading component to match the 20xx.y release convention
                                                 v[1] = v[1] + 2000
                                               }
                                               v
                                             }
                                             # Convert to vectors of integers
                                             target <- createIntegerVector(targetVersion)
                                             source <- createIntegerVector(sourceVersion)
                                             # Take the difference of the common parts
                                             minlength <- min(length(target), length(source))
                                             diff <- head(target, minlength) - head(source, minlength)
                                             if (all(diff == 0)) {
                                               # All the common parts are identical
                                               length(source) < length(target)
                                             } else {
                                               # Assume not less than
                                               lessThan <- FALSE
                                               for (d in diff) {
                                                 if (d < 0) {
                                                   break
                                                 } else if (d > 0) {
                                                   lessThan <- TRUE
                                                   break
                                                 }
                                               }
                                               lessThan
                                             }
                                           },
                                           getTimeSeriesUniqueId = function(timeSeriesIdentifier) {
                                             if (isLegacy | !grepl("@", timeSeriesIdentifier)) {
                                               # It's not in Param.Label@Location format, so just leave it as-is
                                               timeSeriesIdentifier
                                             } else {
                                               # Parse out the location identifier
                                               location <- .self$getLocationIdentifier(timeSeriesIdentifier)

                                               # Ask for all the time-series at that location
                                               r <- httr::GET(paste0(publishUri, "/GetTimeSeriesDescriptionList"), query = list(LocationIdentifier = location))
                                               httr::stop_for_status(r, paste("retrieve time-series at location", location))

                                               # Find the unique ID by matching the full identifier
                                               j <- jsonlite::fromJSON(httr::content(r, "text"))
                                               uniqueId <- j$TimeSeries$UniqueId[which(j$TimeSeries$Identifier == timeSeriesIdentifier)]

                                               if (length(uniqueId) <= 0) {
                                                 # Throw on the brakes
                                                 stop("Can't find time-series '", timeSeriesIdentifier, "' in location '", location, "'.")
                                               }
                                               uniqueId
                                             }
                                           },
                                           getLocationIdentifier = function(timeSeriesIdentifier) {
                                             if (!grepl("@", timeSeriesIdentifier)) {
                                               stop(timeSeriesIdentifier, " is not a <Parameter>.<Label>@<Location> time-series identifier")
                                             }
                                             strsplit(timeSeriesIdentifier, "@")[[1]][[2]]
                                           },
                                           formatIso8601 = function(datetime) {
                                             isoText <- strftime(datetime, "%Y-%m-%dT%H:%M:%OS%z", "UTC")
                                             len <- nchar(isoText)
                                             if (substr(isoText, len, len) != "Z") {
                                               # Inject the missing colon in the zone offset, so "+HHMM" becomes "+HH:MM"
                                               isoText = paste0(substr(isoText, 1, len - 2), ":", substr(isoText, len - 1, len))
                                             }
                                             isoText
                                           },
                                           getLocationData = function(locationIdentifier) {
                                             locationData <- jsonlite::fromJSON(httr::content(httr::stop_for_status(
                                               httr::GET(paste0(publishUri, "/GetLocationData"), query = list(LocationIdentifier = locationIdentifier))
                                               , paste("get location data for", locationIdentifier)), "text"))
                                           },
                                           getMetadataChangeTransactionList = function(timeSeriesIdentifier, queryFrom, queryTo) {
                                             if (missing(queryFrom))     { queryFrom <- NULL }
                                             if (missing(queryTo))       { queryTo <- NULL }
                                             # Coerce native R dates to an ISO 8601 string
                                             if (is.double(queryFrom)) { queryFrom <- timeseries$formatIso8601(queryFrom) }
                                             if (is.double(queryTo))   { queryTo   <- timeseries$formatIso8601(queryTo) }
                                             # Build the query
                                             q <- list(
                                               TimeSeriesUniqueId = .self$getTimeSeriesUniqueId(timeSeriesIdentifier),
                                               QueryFrom = queryFrom,
                                               QueryTo = queryTo
                                             )
                                             q <- q[!sapply(q, is.null)]
                                             # Get metadata transaction list
                                             metadataChangeList <- jsonlite::fromJSON(httr::content(httr::stop_for_status(
                                               httr::GET(paste0(.self$publishUri, "/GetMetadataChangeTransactionList"), query = q)
                                               , paste("get metadata change list for", timeSeriesIdentifier)), "text"))
                                             metadataChangeList
                                           },
                                           getTimeSeriesCorrectedData = function (timeSeriesIdentifier, queryFrom, queryTo, getParts, includeGapMarkers) {
                                             if (missing(queryFrom))         { queryFrom <- NULL }
                                             if (missing(queryTo))           { queryTo <- NULL }
                                             if (missing(getParts))          { getParts <- NULL }
                                             if (missing(includeGapMarkers)) { includeGapMarkers <- NULL }
                                             # Coerce native R dates to an ISO 8601 string
                                             if (is.double(queryFrom)) { queryFrom <- .self$formatIso8601(queryFrom) }
                                             if (is.double(queryTo))   { queryTo   <- .self$formatIso8601(queryTo) }
                                             # Build the query
                                             if (isLegacy) {
                                               q <- list(
                                                 TimeSeriesIdentifier = timeSeriesIdentifier,
                                                 QueryFrom = queryFrom,
                                                 QueryTo = queryTo,
                                                 GetParts = getParts,
                                                 IncludeGapMarkers = includeGapMarkers)
                                             } else {
                                               q <- list(
                                                 TimeSeriesUniqueId = .self$getTimeSeriesUniqueId(timeSeriesIdentifier),
                                                 QueryFrom = queryFrom,
                                                 QueryTo = queryTo,
                                                 GetParts = getParts,
                                                 IncludeGapMarkers = includeGapMarkers)
                                             }
                                             q <- q[!sapply(q, is.null)]
                                             data <- jsonlite::fromJSON(httr::content(httr::stop_for_status(
                                               httr::GET(paste0(.self$publishUri, "/GetTimeSeriesCorrectedData"), query = q)
                                               , paste("get corrected data for", timeSeriesIdentifier)), "text"))
                                           }
                                         )
)

timeseries = timeseriesClient() # Create a client in the global namespace
