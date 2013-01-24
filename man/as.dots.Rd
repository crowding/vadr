\name{as.dots}
\alias{as.dots}
\title{Convert a list of expressions into a \code{\dots} object (a list of
promises.)}
\usage{
  as.dots(x, .envir = parent.frame())
}
\arguments{
  \item{x}{a vector of expressions.}

  \item{.envir}{The environment within which each promise
  will be evaluated.}
}
\value{
  An object of class \code{\dots}.
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

