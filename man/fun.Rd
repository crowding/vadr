\name{fun}
\alias{fun}
\title{A very compact way to define a function.}
\usage{
  fun(expr, .all.names = FALSE, .envir = parent.frame())
}
\arguments{
  \item{expr}{The expression to use as the function's
  body.}

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

  \item{.envir}{The environment the function should be
  enclosed in. Defaults to the environment that called
  \code{fun}.}
}
\value{
  A newly constructed function.
}
\description{
  \code{fun} captures its first argument unevaluated and
  turns it into a function. Every name used in the
  expression becomes an argument, unless it looks like a
  function call. If you don't intend to capture a
  particular variable, you can not provide it, and it will
  use a default value that pulls from the enclosing scope.
  For example:
}
\details{
  \code{ > f <- fun(x/y) > f(10,2) [1] 5 > f function (x =
  evalq(x,parent.frame(), y = evalq(y,parent.frame()) x/y}

  "\code{fun}" is used with "\code{dm_ply}" the way that
  "\code{with}" is used with "\link[plyr]{d_ply}".

  "\code{...}" is supported in the function definitions and
  should behave as you expect.
}
\author{
  Peter Meilstrup
}
\seealso{
  fsummarise fmutate dm_ply
}

