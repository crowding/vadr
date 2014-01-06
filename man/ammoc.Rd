\name{ammoc}
\alias{\%'\%}
\alias{ammoc}
\title{Evaluate all arguments in order, but return the first.}
\usage{
ammoc(...)

x %'% y
}
\arguments{
  \item{...}{any number of arguments.}
}
\value{
The first argument.
}
\description{
Useful in where you want to clean up after a computation
but but return the result of the computation, as in
\code{ammoc(x, rm(x))}, or \code{ammoc(readLines(file),
close(file))}. It's the reverse of the C comma operator.
There is an infix alias, \code{\%\'\%}, (i.e., inverted
comma; use whichever you feel a worse pun.)
}
\note{
In most Lisps, this is called \code{prog1}.
}
\author{
Peter Meilstrup
}

