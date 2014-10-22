\name{\%<~\%}
\alias{\%<~\%}
\title{Modify a variable.}
\usage{
x %<~% y
}
\arguments{
  \item{x}{A variable to assign to.}

  \item{y}{An expression, interpreted in the manner of
  \code{\link{chain}}.}
}
\value{
The rvalue of the assignment.
}
\description{
Assigns to a variable the result of a transformation
applied to that variable.
}
\examples{
library(stringr)
x <- 'a'
x \%<~\% toupper
x \%<~\% str_dup(3)
x \%<~\% rep(3)
x \%<~\% .[2:3]
}
\author{
Peter Meilstrup
}
\seealso{
chain put alter
}

