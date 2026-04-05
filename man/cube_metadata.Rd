% Manual documentation for accessor-style metadata API
\name{cube_metadata}
\alias{cube_metadata}
\alias{cube_metadata<-}
\title{Get or set bundled canonical cube metadata}
\usage{
cube_metadata(x)

cube_metadata(x) <- value
}
\arguments{
\item{x}{A \code{poparray} or HDF5 cube path.}

\item{value}{Named list with \code{roles}, \code{source}, \code{data_col}, and
\code{dim_semantics}.}
}
\value{
Named list with \code{roles}, \code{source}, \code{data_col}, and
\code{dim_semantics}.
}
\description{
Returns the canonical metadata bundle from either an in-memory
\code{poparray} or an HDF5 cube path. When \code{x} is a cube path, the
replacement form writes the bundled metadata back to the file in one validated
operation and requires elevated metadata administration privileges.
}
