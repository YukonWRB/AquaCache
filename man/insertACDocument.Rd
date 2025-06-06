% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/insertACDocument.R
\name{insertACDocument}
\alias{insertACDocument}
\title{Add document to aquacache database}
\usage{
insertACDocument(
  path,
  name,
  type,
  description,
  tags = NULL,
  authors = NULL,
  publish_date = NULL,
  url = NULL,
  share_with = "public_reader",
  geoms = NULL,
  con = NULL
)
}
\arguments{
\item{path}{Valid path including extension to the document to upload.}

\item{name}{A concise but descriptive name to give the document.}

\item{type}{Type of document, which must exist in the database already. Currently one of 'thesis', 'report', 'well log', 'conference paper', 'poster', 'journal article', 'map', 'graph', 'protocol', 'metadata', 'audit'.}

\item{description}{A text description of what the document is. Please be detailed!}

\item{tags}{Tags to associate with the document. Specify multiple tags as individual elements of a character vector, such as c("tag 1", "tag 2").}

\item{authors}{Document author(s) if known. Specify multiple authors as individual elements of a character vector, such as c("author 1", "author 2").}

\item{publish_date}{The date of publication, as a Date object.}

\item{url}{An optional url (could also be a DOI) for the document.}

\item{share_with}{User groups with which to share the document. Default 'public_reader' is the public group. See the table 'user_groups' for more information.}

\item{geoms}{The geom_id(s) with which to associate the document (must be in the database table 'vectors'). Leave NULL for a document with no spatial context.}

\item{con}{A connection to the database. Leave NULL to create a new connection using AquaConnect() and have it closed automatically.}
}
\value{
TRUE if a document was properly added to the database.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

This function facilitates the addition of one document at a time to the database in the 'documents' table. Each document must be linked to a specific location. Adding a document directly to the database is not possible, since the file must be converted to a binary object before loading. See \code{\link[YGwater:getDocument]{YGwater::getDocument()}} to get a document out again.
\subsection{Locations, lines, and polygons}{

Any document can be associated with locations (points), lines, polygons, or any combination thereof. Please reference the table 'vectors' to give the correct geom_id(s) for your desired geoms.
}
}
