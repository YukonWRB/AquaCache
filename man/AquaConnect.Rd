% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AquaConnect.R
\name{AquaConnect}
\alias{AquaConnect}
\title{Connect to the aquacache database}
\usage{
AquaConnect(
  name = "aquacache",
  host = Sys.getenv("aquacacheHost"),
  port = Sys.getenv("aquacachePort"),
  username = Sys.getenv("aquacacheAdminUser"),
  password = Sys.getenv("aquacacheAdminPass"),
  silent = FALSE
)
}
\arguments{
\item{name}{Database name.}

\item{host}{Database host address. By default searches the .Renviron file for parameter:value pair of form aquacacheHost:"hostname".}

\item{port}{Connection port. By default searches the .Renviron file for parameter:value pair of form aquacachePort:"1234".}

\item{username}{Username. By default searches the .Renviron file for parameter:value pair of form aquacacheAdminUser:"username".}

\item{password}{Password. By default searches the .Renviron file for parameter:value pair of form aquacacheAdminPass:"password".}

\item{silent}{TRUE suppresses messages except for errors and login messages.}
}
\value{
A connection to the database.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

This function exists to facilitate connecting to the hydrology database. A nearly identical function exists in package YGwater, but this one by default uses admin privileges while the other uses read-only privileges. Database superusers will have access to all database records, but other users will be asked to provide their username and password to access records other than the 'public' ones. Note that this is \emph{not necessarily} the same username and password as the one used to log into the database itself.

See Details for more information for developers.
}
\details{
\itemize{
\item To facilitate development, a dev database can be connected to by setting the dev parameter to TRUE. This will append "_dev" to the database name.
\item An attribute is added to the connection object to track if a transaction is active. This can be used by functions to determine if a transaction is already open, in which case functions can forgo opening a new transaction and instead use the existing one.
}
}
