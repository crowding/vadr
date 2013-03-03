\name{grapes-open-paren-close-paren-grapes}
\alias{\%()\%}
\alias{\%<<<\%}
\alias{\%<<\%}
\alias{\%__\%}
\alias{curl}
\alias{curr}
\alias{grapes-open-paren-close-paren-grapes}
\title{Partially and fully apply arguments to functions.}
\usage{
  %()%(f, arglist)
}
\arguments{
  \item{x}{a vector, optionally with names, or an object of
  class \code{...} as produced by \code{\link{dots}}.}

  \item{f}{a function, to be called, or to to have
  arguments attached to.}
}
\value{
  \itemize{ \item For \code{\%()\%}, the result of calling
  the function with the arguments provided. When \code{x}
  is a \code{\dots} object, its contents are passed
  inithout evaluating. When \code{x} is another type of
  sequence its elements are put in the value slots of
  already-evaluated promises. This is slightly different
  behavior from \code{\link{do.call}(f, as.list(x),
  quote=TRUE)}, which passes unevaluated promises with
  expressions wrapped in \code{link{quote}}. This makes a
  difference if \code{f} performs nonstandard evaluation.
  \item For \code{\%<<\%} and \code{\%<<<\%}, a new
  function with the arguments partially applied. For
  \code{f \%<<<\% arglist}, the arguments will be placed in
  the argument list before any further arguments; for
  \code{f \%<<\% arglist} the arguments will be placed
  afterwards. \item \code{curr} and \code{curl} are
  standalone functions that partially apply arguments to
  functions; \code{curr(f, a=1, b=2)} is equivalent to
  \code{f \%<<\% dots(a=1, b=2)}, and \code{curl} is the
  "left curry" corresponding to \code{\%>>\%}. \item For
  \code{\%__\%}, the two operands concatenated together.
  The result will be a list, or a \code{dots} object if any
  of the operands are \code{dots} objects.  }
}
\description{
  These operators help in passing arbitrary lists of
  arguments to functions, with a more convenient interface
  than \code{\link{do.call}}. The partial application
  operator allows saving some arguments with a reference to
  a function so the resulting function can be passed
  elsewhere.
}
\details{
  These objects have methods for objects of class
  \code{...} produced by \code{\link{dots}}, so that you
  may partially apply argument lists without arguments as
  yet unevaluated.
}
\note{
  "Curry" is a slight misnomer for partial function
  application.
}
\author{
  Peter Meilstrup
}

