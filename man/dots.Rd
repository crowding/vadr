\name{dots}
\alias{dots}
\title{Partially apply arguments to a function.}
\usage{
  dots(...)
}
\arguments{
  \item{...}{Any number of arguments.}
}
\value{
  A dots object. This is currently just the raw DOTSXP with
  the object bit set and the class set to "..." so that
  method dispatch works.
}
\description{
  The arguments given in brackets are saved and a new
  function is constructed.
}
\details{
  The difference between \code{'[.function'} and
  \code{'[.function'} is that the single bracket applies
  arguments to the right, while the double bracket applies
  arguments to the left.

  This curry captures the arguments you give as promises;
  they will only be evaluated once. if the curried function
  chooses to do so. [For another type of curry see
  addDefaults?] Capture the list of "dot-dot-dot" arguments
  as an object.

  \code{dots} and methods of class \code{...} provide a
  more convenient interface to capturing lists of
  unevaluated arguments and applying them to functions.
}
\examples{
reverse.list <- function(...) {
 d <- dots(...)
 list \%()\% rev(d)
}
reverse.list("a", b="bee", c="see")

named.list <- function(...) {
 d <- dots(...)
 list \%()\% d[names(d) != ""]
 }
named.list(a=1, b=2*2, stop("this is not evaluated"))
}
\author{
  Peter Meilstrup
}
\seealso{
  "%<<%" "%>>%" "%()%" "[...." "[[....", "names...."
}

