\name{fun}
\alias{fun}
\title{A very compact way to define a function. Generally, you can write
\code{fun(x+y)} where you would have written \code{function(x,y) x+y}.}
\usage{
  fun(..., .all.names = FALSE, .envir = parent.frame())
}
\arguments{
  \item{.all.names}{Defaults to \code{FALSE}, in which case
  the formal arguments of the function are only the parts
  of the expression that "look like" variables (i.e. names
  that do not head calls). If set to \code{TRUE}, all
  symbols are wrapped in formal arguments. This includes
  all things that R treats as calls, like \code{`+`},
  \code{`(`} and \code{`{`}. Note that "in the order of
  appearance" means in the s-expression order, so that
  \code{"fun(x+y, .all.names=TRUE)"} will have arguments
  named "+", "x" and "y" in that order.}
}
\description{
  "\code{fun}" is used with "\code{dm_ply}" the way that
  "\code{with}" is used with "\link[plyr]{d_ply}".
}
\author{
  Peter Meilstrup
}
\seealso{
  Other function creation: \code{\link{mutator}},
  \code{\link{summariser}}
}

