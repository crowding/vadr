\name{quote_args}
\alias{quote_args}
\title{Quote all arguments, like \code{\link{alist}}. But when bare words
are given as arguments, interpret them as the argument name, rather
than the argument value. Return a pairlist.
This emulates the syntax used to specify function arguments and defaults.}
\usage{
quote_args(...)
}
\arguments{
  \item{...}{the arguments to quote.}
}
\value{
a pairlist, with quoted arguments, and barewords
inprepreted as names
}
\description{
Quote all arguments, like \code{\link{alist}}. But when
bare words are given as arguments, interpret them as the
argument name, rather than the argument value. Return a
pairlist. This emulates the syntax used to specify function
arguments and defaults.
}
\examples{
substitute(`function`(args, body), list(args=quote_args(x, y=1), body=quote(x+y)))
}

