\name{expressions}
\alias{dots_expressions}
\alias{expressions}
\alias{expressions....}
\alias{expressions<-}
\alias{list_quote}
\title{Extract unevaluated expressions.}
\usage{
expressions(x)

\method{expressions}{...}(x)

list_quote(...)

dots_expressions(...)

expressions(x) <- value
}
\arguments{
  \item{x}{A dots object (see \code{\link{dots}}}

  \item{...}{Any arguments.}
}
\value{
A named list of expressions. The mutator \{expressions<-}
applies new expressions to the given promises (which must
be unevaluated.)
}
\description{
From any set of arguments (typically passing \code{\dots},
\code{dots_expressions} retreives the associated
expressions.) The corresponding method \code{expressions}
method of \code{\link{dots}} objects extracts the dots
argument.
}
\note{
dots_expressions is the same as \code{\link{list_quote}}.
}
\seealso{
dots_unpack dots_environments
}

