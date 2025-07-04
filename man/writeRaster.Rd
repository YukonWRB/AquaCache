% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/writeRaster.R
\name{writeRaster}
\alias{writeRaster}
\title{Write raster to PostGIS database table.}
\usage{
writeRaster(
  con,
  raster,
  rast_table = "rasters",
  bit.depth = NULL,
  blocks = NULL,
  constraints = TRUE
)
}
\arguments{
\item{con}{A connection object to a PostgreSQL database.}

\item{raster}{A terra \code{SpatRaster}; objects from the raster package.}

\item{rast_table}{A character string specifying a PostgreSQL schema in the database (if necessary) and table name to hold the raster (e.g., \code{c("schema","table")}).}

\item{bit.depth}{The bit depth of the raster. Will be set to 32-bit (unsigned int, signed int, or float, depending on the data) if left null, but can be specified (as character) as one of the PostGIS pixel types (see \url{http://postgis.net/docs/RT_ST_BandPixelType.html}).}

\item{blocks}{Optional desired number of blocks (tiles) to split the raster into in the resulting PostGIS table. This should be specified as a one or two-length (columns, rows) integer vector. See also 'Details'.}

\item{constraints}{Whether to create constraints from raster data. Recommended to leave \code{TRUE} unless applying constraints manually (see \url{http://postgis.net/docs/RT_AddRasterConstraints.html}). Note that constraint notices may print to the console, depending on the PostgreSQL server settings.}
}
\value{
A list with TRUE for successful import and the rid(s) of the appended entries.
}
\description{
This function is not meant to be used by itself: in most cases use \code{\link[=insertACModelRaster]{insertACModelRaster()}} or \code{\link[=insertACRaster]{insertACRaster()}} which will populate reference tables so that your raster can be easily found later.

Sends R raster to a PostGIS database table, allowing it to be fetched later into an R environment. This function is an adaptation of \code{\link[rpostgis:pgWriteRast]{rpostgis::pgWriteRast()}}. Will create the raster table if necessary.
}
\details{
SpatRaster band names will be stored in an array in the column
"band_names", which will be restored in R when imported with the function
\code{\link[rpostgis]{pgGetRast}}.

If \code{blocks = NULL}, the number of block will vary by raster size, with
a default value of 100 copies of the data in the memory at any point in time.
If a specified number of blocks is desired, set blocks to a one or two-length
integer vector. Note that fewer, larger blocks generally results in faster
write times.

If the raster has no CRS specified, it will be assigned
\code{EPSG:4326} before being written. This avoids importing
tiles with an SRID of 0, which can lead to "Multiple SRIDs"
errors when reading the raster back.
}
\keyword{internal}
