\name{list_with_missing}
\alias{list_with_missing}
\title{Construct a list, but allow missing arguments.}
\usage{
  list_with_missing(..., *default* = quote(quote(expr = )),
    *envir* = parent.frame())
}
\arguments{
  \item{...}{Objects, possibly named, possibly missing.}

  \item{`*default*`}{What to fill. Should be an
  _expression_ that will be _evaluated_ to fill in missing
  values. Default is an expression that evaluates to the
  empty symbol.}

  \item{`*envir*`}{The environment in which *default* will
  be evaluated.}
}
\value{
  A list, with any non-missing arguments evaluated, any
  other arguments substituted with the default expression
  and evaluated.
}
\description{
  This is particularly useful for supporting array-indexing
  functions, that work like built-in \code{\link{`[`}}; it
  allows you to tell when arguments are missing, but also
  evaluate any numnber of non-missing arguments in their
  correct environments. choking on missing arguments. That
  is, it lets you implement a \code{`[`} accessor that
  currectly supports usage like
  \code{function(a,b)(array[a*b, ])(4,2*e)}. It is also
  used in \code{\link{expand_macros()}} and for other
  computing-on-the-language purposes.
}
\section{Notes
This is probably rather slow.}{

}
\examples{
#unlike alist, arguments are evaluated in context.
y <- 4
alist(2*y, , x=12+24, d=)
list.with.missing(2*y, , x=12+24, d=)

#unlike with `list`, missing arguments are detected and handled.
\dontrun{
list(2*y, , x=12+24, d=) #produces an error.
}
}
\author{
  Peter Meilstrup
}

