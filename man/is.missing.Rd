\name{dots_missing}
\alias{dots_missing}
\alias{is.missing}
\alias{list_missing}
\title{Detect missing arguments in link{\dots} arguments}
\usage{
dots_missing(...)

list_missing(...)

is.missing(x)
}
\arguments{
  \item{...}{for \code{\link{dots_missing}}, any number of
  arguments, each being checked for missingness, without
  being evaluated.}

  \item{x}{If given a list, compares each element with the
  missing value. Given a \code{\link{dots}} object,
  determines whether each argument is empty or missing.}
}
\value{
For \code{dots_missing}, a logical vector.

For \code{list_missing}, a named list of all evaluated
arguments, where any missing arguments are set to
\code{\link{missing_value}()}.

For \code{is.missing}, a vector of boolean values.
}
\description{
These are useful for writing functions that accept any
number of arguments but some may be missing. For example,
arrays in R can have any number of dimensions, indexed by
the \code{\link{[}} function, where a missing argument
means to take all indexes on that dimension. However there
is not a good way to replicate \code{\link{[}}'s behavior
in base R; using \code{list(\dots)} to collect all
positional arguments will throw errors on missing
arguments. Instead, use \code{x <- list_missing(...)} and
\link{is.missing}(x) to detect missing arguments.

For \code{\dots} objects as made by \code{\link{dots}},
performs this check without forcing evaluation.
}
\note{
A frequently seen strategy is to use
\code{\link{match.call}(expand.dots=TRUE)} and
\code{\link{eval}(..., parent.frame())} to screen for
missing arguments while evaluating non-missing arguments.
This is not recommended because \link{match.call} does not
capture the environments of the arguments, leading to
hygeine violations.
}
\author{
Peter Meilstrup
}
\seealso{
missing_value
}

