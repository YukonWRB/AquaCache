% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/downloadWSC.R
\name{downloadWSC}
\alias{downloadWSC}
\title{Get realtime data from the WSC}
\usage{
downloadWSC(location, parameter_id, start_datetime, end_datetime = Sys.time())
}
\arguments{
\item{location}{A WSC station number.}

\item{parameter_id}{A WSC parameter code. 47 for discharge primary (sensor derived), 8 for discharge (sensor measured), 46 for level, 5 for water temperature, 4 for air temperature. See the full list using \link[tidyhydat:param_id]{tidyhydat::param_id}.}

\item{start_datetime}{Specify as class Date, POSIXct OR as character string which can be interpreted as POSIXct. If character, UTC offset of 0 will be assigned, otherwise conversion to UTC 0 will be performed on POSIXct class input. If date, time will default to 00:00 to capture whole day.}

\item{end_datetime}{Specify as class Date, POSIXct OR as character string which can be interpreted as POSIXct. If character, UTC offset of 0 will be assigned, otherwise conversion to UTC 0 will be performed on POSIXct class input. If Date, time will default to 23:59:59 to capture whole day.}
}
\value{
A data.table object of hydrometric data, with datetimes in UTC-0.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

A fast, pared down method of fetching WSC realtime data (at least compared to tidyhydat and tidyhydat.ws options). Dispenses with extra columns that those packages include and uses data.table::fread to speed up parsing.
}
