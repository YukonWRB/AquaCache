% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getNewContinuous.R
\name{getNewContinuous}
\alias{getNewContinuous}
\title{Get new continuous-category data}
\usage{
getNewContinuous(con = NULL, timeseries_id = "all", active = "default")
}
\arguments{
\item{con}{A connection to the database, created with \code{\link[DBI:dbConnect]{DBI::dbConnect()}} or using the utility function \code{\link[=AquaConnect]{AquaConnect()}}. NULL will create a connection and close it afterwards, otherwise it's up to you to close it after.}

\item{timeseries_id}{The timeseries_ids you wish to have updated, as character or numeric vector. Defaults to "all", which means all timeseries of category 'continuous'.}

\item{active}{Sets behavior for import of new data. If set to 'default', the function will look to the column 'active' in the 'timeseries' table to determine if new data should be fetched. If set to 'all', the function will ignore the 'active' column and import all data.}
}
\value{
The database is updated in-place, and a data.frame is generated with one row per updated location.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

Retrieves new real-time data starting from the last data point in the local database, using the function specified in the timeseries table column "source_fx". Only works on stations that are ALREADY in the measurements_continuous table and that have a proper entry in the timeseries table; refer to \code{\link[=addACTimeseries]{addACTimeseries()}} for how to add new stations. Does not work on any timeseries of category "discrete": for that, use \code{\link[=getNewDiscrete]{getNewDiscrete()}}. Timeseries with no specified souce_fx will be ignored.
\subsection{Default arguments passed to 'source_fx' functions:}{

This function passes default arguments to the "source_fx" function for start_datetime, defaults to the instant after the last point already existing in the DB. The rest of the fetch parameters are set using the "source_fx_args" column in the "timeseries" table; refer to \code{\link[=addACTimeseries]{addACTimeseries()}} for a description of how to formulate these arguments.
}

\subsection{Assigning measurement periods:}{

With the exception of "instantaneous" timeseries which automatically receive a period of "00:00:00" (0 time), the period associated with measurements (ex: 1 hour precipitation sum) is derived from the interval between measurements UNLESS a period column is provided by the source function (column source_fx, may also depend on source_fx_args). This function typically fetches only a few hours of measurements at a time, so if the interval cannot be conclusively determined from the new data (i.e. hourly measurements over four hours with two measurements missed) then additional data points will be pulled from the database.

If a period supplied by any data fetch function cannot be coerced to an period object acceptable to "duration" data type, NULL values will be entered to differentiate from instantaneous periods of "00:00:00".
}

\subsection{Sharing privileges and ownership}{

This is dictated by the timeseries table, and checked prior to passing data through view tables to public users.
}
}
