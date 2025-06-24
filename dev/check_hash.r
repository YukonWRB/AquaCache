


# Function to load an image and calculate its MD5 hash (binary) using md5sum
get_image_md5sum <- function(image_path) {
    # Calculate MD5 hash using md5sum
    md5_hash <- unname(tools::md5sum(image_path))
    return(md5_hash)}


# Load required library
library(digest)

# Read the image file as binary data
image_path <- "path/to/your/image.jpg"
image_data <- readBin(image_path, what = "raw", n = file.info(image_path)$size)

# Generate the MD5 hash
image_hash <- digest(image_data, algo = "md5", serialize = FALSE)

# Print the hash
print(image_hash)


# Example usage:
image_path <- paste0(
  "C:/Users/esniede/Pictures/",
  "000e6d3c-a691-4102-ab7a-58b38a659744.jpeg"
)

md5sum <- get_image_md5sum(image_path)
md5sum <- "cdca10de8990b185c72c09705781c16a"

con <- AquaConnect(username="admin", password="SnowFa11ing")

# Insert the image into the AquaCache database
insertACImage(
    object = image_path,
    image_type=4,
    con=con,
    datetime = as.POSIXct(Sys.time(), tz = "UTC"),
    fetch_datetime = as.POSIXct(Sys.time(), tz = "UTC"),
    location=12,
    description = "Confirming overwrite functionality",
    overwrite = TRUE)


datetime <- as.POSIXct(Sys.time(), tz = "UTC")
share_with <- c("public_reader")
latitude <- 11
longitude <- 11
extension <- "jpeg"
image_type <- 4
file <- readBin(image_path, what = "raw", n = file.info(image_path)$size)
DBI::dbGetQuery(con, paste0(
    "INSERT INTO images (datetime, share_with, latitude, longitude, format, file, image_type) VALUES ('",
    datetime, "', '{", paste(share_with, collapse = ","), "}', ", latitude, ", ", longitude, ", '", extension, "', '\\x", paste0(file, collapse = ""), "', ", image_type, 
    ") ON CONFLICT (file_hash) DO UPDATE SET ",
    "datetime = EXCLUDED.datetime, ",
    "share_with = EXCLUDED.share_with, ",
    "latitude = EXCLUDED.latitude, ",
    "longitude = EXCLUDED.longitude, ",
    "format = EXCLUDED.format, ",
    "image_type = EXCLUDED.image_type ",
    "RETURNING image_id;"
))

# on conflict

query <- "SELECT * FROM files.images WHERE image_id = 16930"
results <- DBI::dbGetQuery(con, query)


results$file_hash

query <- sprintf(
    "SELECT image_id FROM files.images WHERE file_hash = '%s'",
    md5sum
)


result <- DBI::dbGetQuery(con, query)

if (nrow(result) > 0) {
    image_ids <- result$image_id
    delete_query <- sprintf(
        "DELETE FROM files.images WHERE image_id IN (%s)",
        paste(shQuote(image_ids), collapse = ",")
    )
    DBI::dbExecute(con, delete_query)
}

DBI::dbDisconnect(con)
