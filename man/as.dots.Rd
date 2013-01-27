\name{as.dots}
\alias{as.dots}
\alias{as.dots.literal}
\title{Convert a list of expressions into a \code{\dots} object (a list of
promises.)}
\usage{
  as.dots(x, .envir = parent.frame())
}
\arguments{
  \item{x}{a vector or list.}

  \item{.envir}{The environment within which each promise
  will be evaluated.}
}
\value{
  An object of class \code{\dots}. For \code{as.dots}, the
  list items are treated as expressions to be evaluated.
  For \code{as.dots}, the items are treated as listeral
  values.
}
\description{
  Convert a list of expressions into a \code{\dots} object
  (a list of promises.)
}
\author{
  Peter Meilstrup
}
\seealso{
  dots "%<<%" "%>>%" "%()%" "[...." "[[....", "names...."
}

