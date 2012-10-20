\name{missing.value}
\alias{missing.value}
\title{Return an empty symbol.}
\usage{
  missing.value(n)
}
\arguments{
  \item{n}{Optional; a number. If provided, will return a
  list of missing values with this many elements.}
}
\value{
  A symbol with empty name, or a list of such.
}
\description{
  The empty symbol is used to represent missing values in
  the R language; for instance in the value slot of a
  function argument when there is no default; and in the
  expression slot of a promise when a missing argument is
  given.
}
\examples{
# These statements are equivalent:
quote(function(x, y=1) x+y)
call("function", as.pairlist(x=missing.value(), y=1), quote(x+y))

# These statements are also equivalent:
quote(df[,1])
substitute(df[row,col], list(row = missing.value(), col = 1))

# These statements are equivalent
quote(function(a, b, c, d, e) print("hello"))
call("function", as.pairlist(setNames(missing.value(5), letters[1:5])), quote(print("hello")))
}

