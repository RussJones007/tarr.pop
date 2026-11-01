% Manual documentation for accessor-style metadata API
\name{roles}
\alias{roles}
\alias{roles<-}
\title{Get or set cube roles for a demographic array or cube file}
\usage{
roles(x)

roles(x) <- value
}
\arguments{
\item{x}{A \code{demographic_array} subclass or HDF5 cube path.}

\item{value}{Named list with \code{time}, \code{area}, and optional
\code{strata}.}
}
\value{
Named list with \code{time}, \code{area}, and \code{strata}.
}
\description{
Shared in-memory access supports population and projection arrays. Projection
statistical axes are excluded from strata roles, and metadata replacement
validates the concrete subclass. Projection-specific provenance fields are
retained when editing canonical source metadata. HDF5 path-based editing
retains its existing storage schema and access controls.
Returns dimension role metadata from either an in-memory \code{poparray} or an
HDF5 cube path. When \code{x} is a cube path, the replacement form writes
updated role metadata back to the file and requires elevated metadata
administration privileges.
Path-based edits validate roles together with semantic metadata and create a
metadata revision. See \code{\link{cube_history}}.
}
