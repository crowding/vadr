\docType{data}
\name{bind}
\alias{<-.bind}
\alias{[<-.bind}
\alias{bind}
\alias{bind<-}
\title{Unpack a list and assign to multiple variables.}
\format{Class 'bind'  chr "use bind[a=x, b=y] <- c(a=1,b=2) to do parallel assignment."}
\usage{
  bind
}
\arguments{
  \item{...}{}

  \item{.envir}{The environment to bind in (defaults to the
  caller).}
}
\value{
  The list that was originally unpacked.
}
\description{
  This is a "destructuring bind" for R. It can be used to
  unpack lists into different values, or achieve the effect
  of multiple return values from a function.
}
\examples{
# For a simple example,
bind(a=x, b=y) <- c(a="foo", b="bar")
x # -> "foo"
y # -> "bar"

# Note that the assigned-to variable is on the _right_ side of each
# equals in the arglist. This is admittedly awkward but is the best
# way to remain consistent with R's argument-binding semantics.

# Element to variable matching happens according to R's argument
# binding rules, can be nested, and complicated assignment targets
# can be used. The following shows off a complicated unpacking:

bind[a=x, .rest=bind[aa=xx, bb=yy], b=y, cc=bind[a=zz[2], b=zz[1]]] <-
  list(a=1, b="two", aa="eleven", bb=22, cc=c(a=1, b=2))
x
y
xx
yy <- 22
zz <- c(2,1)

# x <- 1; y <- "two", xx <- "eleve
}
\author{
  Peter Meilstrup
}
\keyword{datasets}

