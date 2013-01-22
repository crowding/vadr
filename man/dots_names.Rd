\name{dots_names}
\alias{dots_missing}
\alias{dots_names}
\alias{list_missing}
\alias{list_quote}
\title{Functions to work with dot-dot-dot arguments.}
\usage{
  dots_names(...)
}
\arguments{
  \item{...}{Any arguments. Usually you will pass
  \code{...} from the body of a function.}
}
\value{
  For \code{\link{dots_names}}, a character vector giving
  the names of all arguments.

  For \code{\link{dots_names}}, the names of all arguments.

  For \code{\link{dots_missing}}, a logical vector with
  TRUE for each missing argument.

  For \code{\link{list_missing}}, a named list of all
  evaluated arguments, where any missing arguments are set
  to NULL.

  For \code{\link{dots_names}}, a list of quoted arguments.
}
\description{
  These are useful for writing functions that accept any
  number of arguments but some may be missing. For example,
  arrays in R can have any number of dimensions, indexed by
  the \code{\link{"["}} function, where a missing argument
  means to take all indexes on that dimension. However
  there is not a good to replicate \code{\link{"["}}'s
  behavoior in pure R; using \code{list(...)} to collect
  all positional arguments will throw errors on missing
  arguments. These functions help extract names and values
  of positional arguments without triggering missing-value
  errors.
}
\author{
  Peter Meilstrup
}

