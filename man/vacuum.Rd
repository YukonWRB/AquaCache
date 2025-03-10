% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vacuum.R
\name{vacuum}
\alias{vacuum}
\title{Maintenance (vacuum) function for aquacache database.}
\usage{
vacuum(con = NULL, full = FALSE)
}
\arguments{
\item{con}{A connection to the database. If left NULL will use function AquaConnect and automatically disconnect when finished.}

\item{full}{If TRUE, performs a full vacuum. This takes longer and requires an exclusive lock, but can reclaim more space as tables are re-written without dead space.}
}
\value{
A vacuumed database.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

Performs a VACUUM (ANALYZE) operation on the database, re-organizing and compacting tables.
}
