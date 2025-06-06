


library(AquaCache)

# Function to load an image and calculate its MD5 hash (binary) using md5sum
get_image_md5sum <- function(image_path) {
    # Calculate MD5 hash using md5sum
    md5_hash <- unname(tools::md5sum(image_path))
    return(md5_hash)}




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
    img_meta_id = 44,
    description = "Test image for MD5 hash check")

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
