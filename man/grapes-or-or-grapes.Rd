\name{\%||\%}
\alias{\%||\%}
\title{Evaluate the first argument; if null, evaluate and return the
second argument.}
\usage{
a \%||\% b
}
\arguments{
  \item{a}{The first argument to evaluate.}

  \item{b}{The second argument to evaluate. Only evaluated
  if A evaluates to NULL.}
}
\value{
the value of \code{a} if not null, else \code{b}
}
\description{
Evaluate the first argument; if null, evaluate and return
the second argument.
}
\author{
Peter Meilstrup
}

