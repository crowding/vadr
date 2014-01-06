\name{missing_value}
\alias{missing_value}
\title{Return an empty symbol.}
\usage{
missing_value(n)
}
\arguments{
  \item{n}{Optional; a number. If provided, will return a
  list of missing values with this many elements.}
}
\value{
A symbol with empty name, or a list of such.
}
\description{
The empty symbol is used to represent missing values in the
R language; for instance in the value of formal function
arguments when there is no default; in the expression slot
of a promise when a missing argument is given; and bound to
the value of a variable when it is called with a missing
value. When computing on the language, then, you may need
to explicitly invoke the "missing" value.
}
\examples{
# These statements are equivalent:
quote(function(x, y=1) x+y)
call("function", pairlist(x=missing_value(), y=1), quote(x+y))

# These statements are also equivalent:
quote(df[,1])
substitute(df[row,col], list(row = missing_value(), col = 1))

# These statements are also equivalent:
quote(function(a, b, c, d, e) print("hello"))
call("function", as.pairlist(put(missing_value(5), names, letters[1:5])),
                 quote(print("hello")))
}
\seealso{
list_missing dots_missing
}

