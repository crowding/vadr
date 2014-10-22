\name{make_unique_names}
\alias{make_unique_names}
\title{Modify some character strings unique with respect with an
existing set of (unmodified) character strings.}
\usage{
make_unique_names(new, context)
}
\arguments{
  \item{new}{Initial values for the new names}

  \item{context}{Existing names to avoid collisions with.}
}
\value{
the values of \code{new} in order modified to avoid
collisions.
}
\description{
A convenience extension of \code{\link{make.unique}}.
}
\author{
Peter Meilstrup
}

