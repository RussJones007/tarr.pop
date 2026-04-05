% Manual documentation for accessor-style metadata API
\name{roles}
\alias{roles}
\alias{roles<-}
\title{Get or set cube roles for a poparray or cube file}
\usage{
roles(x)

roles(x) <- value
}
\arguments{
\item{x}{A \code{poparray} or HDF5 cube path.}

\item{value}{Named list with \code{time}, \code{area}, and optional
\code{strata}.}
}
\value{
Named list with \code{time}, \code{area}, and \code{strata}.
}
\description{
Returns dimension role metadata from either an in-memory \code{poparray} or an
HDF5 cube path. When \code{x} is a cube path, the replacement form writes
updated role metadata back to the file and requires elevated metadata
administration privileges.
}
