\name{macro}
\alias{macro}
\title{Turn an expression-substituting function into a
nonstandard-evaluating function.}
\usage{
macro(fn, cache = TRUE, JIT = cache)
}
\arguments{
  \item{fn}{A function which takes some arguments and
  returns a trane}

  \item{cache}{Whether to store already-compiled macros for
  faster evaluation. Defaults to TRUE. This requires that
  the macro function be a pure function not referring to
  any outside state.}

  \item{JIT}{Whether to compile expressions (using the
  "compiler" package) before executing. Defaults to TRUE.}
}
\value{
the wrapper function. It will have an identical argument
list to the wrapped function. It will transform all
arguments into expressions, pass the expressions to the
wrapped function, then evaluate the result it gets back.

The advantage of macros versus usual nonstandard evaluation
using \code{\link{substitute}}, \code{link{eval}} and
friends is that it encourages separating "computing on the
language" from "computing on the data." Because code is
usually static while data is variable, the language
transformations only need to happen once per each call
site. Thus the expansions of macros can be cached, enabling
complicated code transformations with smaller performance
penalties.
}
\description{
This just places a wrapper around the function that
un-quotes all arguments and evaluates the result.
}
\author{
Peter Meilstrup
}
\seealso{
qq
}

