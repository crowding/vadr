\name{expand_macros}
\alias{expand_macros}
\alias{expand_macros_q}
\title{Expand any macros in the quoted expression.}
\usage{
expand_macros(expr, macros = NULL, where = arg_env(expr, environment()),
  recursive = FALSE)
}
\arguments{
  \item{expr}{An expression. For \code{expand_macros_q},
  this argument is quoted. For \code{expand_macros}, itis a
  language object.}

  \item{macros}{a named list of macros. By default searches
  for all macros.}
}
\value{
The expansion of the given expression.
}
\description{
This searches for macro functions referred to in the quoted
expression and substitutes their equivalent expansions.
Not guaranteed to give exact results.
}
\author{
Peter Meilstrup

This is intended for interactive/debugging use; in general,
its results are not correct. For example, expressions
appearing inside of \code{link{quote}()} will get expanded
anyway.
}

