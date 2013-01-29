\name{dots_names}
\alias{alist}
\alias{dots_missing}
\alias{dots_names}
\alias{list_missing}
\alias{list_quote}
\title{Functions to work with dot-dot-dot (\dots) arguments.}
\usage{
  dots_names(...)
}
\arguments{
  \item{...}{Any arguments. Usually you will pass
  \code{\dots} from the body of a function.}
}
\value{
  \itemize{ \item For \code{\link{dots_names}}, the names
  of all arguments. Names are also attached to results from
  the other functions listed here. \item For
  \code{\link{dots_missing}}, a logical vector with TRUE
  for each missing argument. \item For
  \code{\link{list_missing}}, a named list of all evaluated
  arguments, where any missing arguments are set to
  missing.value. \item For \code{\link{list_quote}}, a list
  of quoted arguments. This extracts the original
  expressions from a dotlist without forcing evaluation. }
}
\description{
  These are useful for writing functions that accept any
  number of arguments but some may be missing. For example,
  arrays in R can have any number of dimensions, indexed by
  the \code{\link{"["}} function, where a missing argument
  means to take all indexes on that dimension. However
  there is not a good way to replicate \code{\link{"["}}'s
  behavior in base R; using \code{list(\dots)} to collect
  all positional arguments will throw errors on missing
  arguments. These functions help extract names and values
  of positional arguments without triggering missing-value
  errors.
}
\details{
  Parallel functions exist as methods of the \code{\dots}
  class of objects returned by \code{\link{dots}} that
  allows you to manipulate \dots argument lists as explicit
  objects.
}
\author{
  Peter Meilstrup
}
\seealso{
  is.missing dots curr
}

