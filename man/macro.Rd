\name{macro}
\alias{macro}
\title{Turn an expression-substituting function into a
nonstandard-evaluating function.}
\usage{
  macro(fn, cache = TRUE, verbose = FALSE)
}
\arguments{
  \item{fn}{A function which takes some arguments and
  returns a trane}
}
\value{
  the wrapper function. It will have an identical argument
  list to the wrapped function. It will transform all
  arguments into expressions, pass the expressions to the
  wrapped function, then evaluate the result it gets back.

  (*) The author believes that there is one way to properly
  do nonstandard evaluation in R, and that is to quote ALL
  of your arguments and perform purely lexical operations
  on them, evaluating the result in the caller. In other
  words, to behave as a macro. Functions which evaluate
  different arguments in different scopes (e.g.
  \code{\link{transform}} cause problems; consider writing
  these as functors, or using formula objects or
  \code{\link[plyr]{.}()} to capture environments
  explicitly.
}
\description{
  This just places a wrapper around the function so that
  you do not have to remember how to use substitute, and
  you are not tempted to mix nonstandard with standard
  evaluation(*).
}
\author{
  Peter Meilstrup
}
\seealso{
  template
}

