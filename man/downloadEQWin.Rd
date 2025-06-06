% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/downloadEQWin.R
\name{downloadEQWin}
\alias{downloadEQWin}
\title{Bring EQWin water quality data into the aquacache database}
\usage{
downloadEQWin(
  location,
  sub_location,
  start_datetime,
  end_datetime = Sys.time(),
  EQcon = EQConnect(silent = TRUE)
)
}
\arguments{
\item{location}{The location code (project code, i.e. the portion in parentheses) associated with the EQWin station.}

\item{sub_location}{The sub-location code (station code, i.e. the portion after the parentheses) associated with the EQWin station.}

\item{start_datetime}{Specify as class Date, POSIXct OR as character string which can be interpreted as POSIXct. If character, UTC offset of 0 will be assigned, but if POSIXct the object's tzone attribute will be preserved. If date, time will default to 00:00 to capture whole day.}

\item{end_datetime}{Same as start_datetime but not quite!}

\item{EQcon}{connection to the EQWin database. See EQConnect for details.}
}
\value{
A data.frame object with the requested data. If there are no new data points the data.frame will have 0 rows.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

Brings in water quality data from the EQWin database. <DL values are transformed to the negative of the detection limit, > instrument range is left as the value. A note is added in either case to the 'note' column.
}
