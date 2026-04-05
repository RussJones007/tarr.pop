% Manual documentation for accessor-style metadata API
\name{source_meta}
\alias{source_meta}
\alias{source_meta<-}
\title{Get or set source metadata for a poparray or cube file}
\usage{
source_meta(x)

source_meta(x) <- value
}
\arguments{
\item{x}{A \code{poparray} or HDF5 cube path.}

\item{value}{Named list of source/provenance fields.}
}
\value{
Named list of source/provenance fields.
}
\description{
Returns source and provenance metadata from either an in-memory
\code{poparray} or an HDF5 cube path. When \code{x} is a cube path, the
replacement form writes updated source metadata back to the file and requires
elevated metadata administration privileges.
}
