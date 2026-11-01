% Manual documentation for accessor-style metadata API
\name{source_meta}
\alias{source_meta}
\alias{source_meta<-}
\title{Get or set source metadata for a demographic array or cube file}
\usage{
source_meta(x)

source_meta(x) <- value
}
\arguments{
\item{x}{A \code{demographic_array} subclass or HDF5 cube path.}

\item{value}{Named list of source/provenance fields.}
}
\value{
Named list of source/provenance fields.
}
\description{
Shared in-memory access supports population and projection arrays. Projection
statistical axes are excluded from strata roles, and metadata replacement
validates the concrete subclass. Projection-specific provenance fields are
retained when editing canonical source metadata. HDF5 path-based editing
retains its existing storage schema and access controls.
Returns source and provenance metadata from either an in-memory
\code{poparray} or an HDF5 cube path. When \code{x} is a cube path, the
replacement form writes updated source metadata back to the file and requires
elevated metadata administration privileges.
Path-based updates create metadata revisions; use
\code{\link{update_cube_metadata}} for an explicit reason and
\code{\link{cube_history}} to inspect previous notes.
}
