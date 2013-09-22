\name{mply}
\alias{mply}
\title{Alternative to mapply with a cleaner calling convention.}
\usage{
  mply(fn, ...)(...)
}
\arguments{
  \item{fn}{A function to apply.}

  \item{...}{(in the first set of parens) Extra arguments
  to be given to each call.}

  \item{...}{(in the second set of parens) Arguments to
  apply; they are expected to be all of the same length.}
}
\value{
  A list.
}
\description{
  Alternative to mapply with a cleaner calling convention.
}
\note{
  After more than a hundred or so elements, this has less
  overhead per loop than \code{\link{mapply}}.
}
\author{
  Peter Meilstrup
}

