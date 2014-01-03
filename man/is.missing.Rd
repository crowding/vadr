\name{dots_missing}
\alias{dots_missing}
\alias{is.missing}
\title{Check if list members are equal to the "missing value."}
\usage{
dots_missing(...)

is.missing(x)
}
\arguments{
  \item{...}{\For \code{\link{dots_missing}}, any number of
  arguments, each being checked for missingness, without
  being evaluated.}

  \item{x}{If given a list, compares each element with the
  missing value. Given a \code{\link{dots}} object,
  determines whether each argument is empty or missing.}
}
\value{
A vector of boolean values.
}
\description{
For \code{\dots} objects as made by \code{\link{dots}},
performs this check without forcing evaluation.
}
\author{
Peter Meilstrup
}
\seealso{
missing_value
}

