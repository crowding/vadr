\name{grapes-open-paren-close-paren-grapes}
\alias{"\%()\%"}
\alias{"\%<<\%"}
\alias{"\%>>\%"}
\alias{"\%__\%"}
\alias{grapes-open-paren-close-paren-grapes}
\title{Partially and fully apply arguments to functions.}
\usage{
  %()%(f, arglist)
}
\arguments{
  \item{x}{a vector, optionally with names, or an object of
  class \code{...} as produced by \code{\link{dots}}.}

  \item{f}{a function, to be called to to have arguments
  attached to.}
}
\value{
  \itemize{ \item{For \code{%()%}, the result of calling
  the function with the arguments provided.} \item{For
  \code{%<<%} and \code{%>>%}, a new function with the
  arguments partially applied. For \code{arglist %>>% f},
  the arguments will be placed in the argument list before
  any further arguments; for \code{f %<<% arglist} thje
  arguments } \item{for \code{%__%}, the two operands
  pasted together. The result will be a list, or a
  \code{dots} object if any of the operands are \code{dots}
  objects.
}
\description{
  These operators help in passing arbitrary lists of
  arguments to functions, with a more consistent interface
  than \code{\link{do.call}}. The currying operator allows
  saving some arguments with a reference to a function so
  the resulting function can be passed elsewhere.
}
\details{
  These objects have methods for objects of class
  \code{...} produced by \code{\link{dots}}, so that you
  may apply arguments as yet unevaluated.
}
\author{
  Peter Meilstrup
}

