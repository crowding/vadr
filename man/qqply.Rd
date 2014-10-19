\name{qqply}
\alias{qeply}
\alias{qqply}
\title{Repeatedly expand an expression against sequences of values.}
\usage{
qqply(...)(...)
}
\arguments{
  \item{...}{(in the first argument list) One or more
  expressions or expressions to expand. These may have
  names, which will also be expanded.}

  \item{...}{(in the second argument list) Sequences. The
  expressions will be expanded in a context that has these
  names bound to one value at a time from each sequence
  (inheriting from the calling frame).}
}
\value{
For \code{qqply}, a list of expressions. For \code{qeply},
the expressions will be evaluated in the calling frame.
}
\description{
Performs template expansion as for \code{\link{qq}}, but
evaluates over sequences (as in \code{\link{mapply}}).
}
\examples{
qqply(`.(x)` = .(y))(x=letters[1:3], y=1:3)

qe(function(
    .=...( qqply(`.(..1)`=.(..2))(letters, 1:26))) {
  ...(qqply(.(as.name(x)) <- .(as.name(y)))(y=letters[2:26], x=letters[1:25]))
  e
})
}
\author{
Peter Meilstrup
}
\seealso{
qq bquote
}

