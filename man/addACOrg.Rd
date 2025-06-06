% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addACOrg.R
\name{addACOrg}
\alias{addACOrg}
\title{Add a new organization to the aquacache}
\usage{
addACOrg(
  name,
  name_fr = NA,
  contact_name = NA,
  phone = NA,
  email = NA,
  note = NA,
  con = NULL
)
}
\arguments{
\item{name}{The name of the new owner/contributor or organization}

\item{name_fr}{The French name of the new owner/contributor or organization (optional).}

\item{contact_name}{The name of the contact person for the new owner/contributor or organization (optional).}

\item{phone}{The phone number of the contact person for the new owner/contributor or organization (optional). Specify as a character string or numeric.}

\item{email}{The email address of the contact person for the new owner/contributor or organization (optional).}

\item{note}{Any notes about the new owner/contributor or organization (optional).}

\item{con}{A connection to the database, created with \code{\link[DBI:dbConnect]{DBI::dbConnect()}} or using the utility function \code{\link[=AquaConnect]{AquaConnect()}}. NULL will create a connection and close it afterwards, otherwise it's up to you to close it after.}
}
\value{
The organization_id of the new user group, plus a new entry to the database.
}
\description{
Add a new organization to the aquacache
}
