\name{arg_dots}
\alias{arg_dots}
\usage{
arg_list(\dots)
arg_list[envir](\dots)
}
\arguments{
  \item{...}{Variable names (unevaluated). Arguments may be
  named; these names determine the names on the dots list
  (and not the variable names)}
}
\value{
a \code{\link{dots}} object containing the promises that
are bound to those variables in the calling environment.
}
\note{
The tags on the dots object are determined by argument
names; variable names are discarded.
}
\author{
Peter Meilstrup
}
\seealso{
dots
}

