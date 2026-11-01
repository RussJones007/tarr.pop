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
\item{x}{A \code{demographic_array} subclass or HDF5 cube path.}

\item{value}{Named list with \code{roles}, \code{source}, \code{data_col}, and
\code{dim_semantics}.}
}
\value{
Named list with \code{roles}, \code{source}, \code{data_col}, and
\code{dim_semantics}.
}
\description{
Shared in-memory access supports population and projection arrays. Projection
statistical axes are excluded from strata roles, and metadata replacement
validates the concrete subclass. Projection-specific provenance fields are
retained when editing canonical source metadata. HDF5 path-based editing
retains its existing storage schema and access controls.
Returns the canonical metadata bundle from either an in-memory
\code{poparray} or an HDF5 cube path. When \code{x} is a cube path, the
replacement form writes the bundled metadata back to the file in one validated
operation and requires elevated metadata administration privileges.
}
\details{
Optional dimension applicability travels within \code{dim_semantics}, not source
provenance. Replacement validates schema boundaries, target levels, and coverage
against current labels. Path-based updates write metadata only and do not rewrite
the population dataset. See \code{\link{dim_semantics}} for inspection.
Path-based edits record metadata revisions inside the file. Use
\code{\link{update_cube_metadata}} to supply a change reason and
\code{\link{cube_history}} to inspect coverage and provenance history.
No earlier population values are retained. In-memory edits do not create
on-disk revisions until persisted.
}
