\name{mutator}
\alias{mutator}
\title{Create a function of several arguments returning a data frame. The
function evaluates all expressions with respect to all arguments
given, then returns the data frame}
\usage{
  mutator(...)
}
\arguments{
  \item{...}{A list of expressions to evaluate
  sequentially.  For code{\link{summariser}} and
  \code{link{mutator}} the argument names will used as
  column names of the output data frame.}

  \item{.envir}{The environment the function should be
  enclosed in. Defaults to the calling environment.}
}
\value{
  A newly constructed function.
}
\description{
  For example, mutate(a=b/c, b=a+mean(c)) returns a
  function taking arguments 'a', 'b', 'c', and ellipsis,
  and returns a data frame with columns "a", "b", "c" and
  any others provided in ...
}
\details{
  The contents of the arguemnts determine the new
  function's arguments in the same way that \link{fun}
  does. The arguments are evaluated in order. Any symbols
  not provided in the environment will be taken from the
  data frame.

  The function \code{fmutate} is intended to be used with
  \link{dm_ply} in a similar way that \code{mutate} is used
  with \link[plyr]{d_ply}
}
\section{Function Creation}{
  The arguments given are captured unevaluated. Every name
  used as a variable (that is, not as a call) is noted and
  the resulting function will have an argument matching
  each name. If values for the argument are not given, the
  default will look up the argument in the environment from
  which the function was defined.

  Ellipsis arguments (\code{...}) are also noted, and a
  matching ellipsis argument will be given to the resulting
  function.
}
\author{
  Peter Meilstrup
}
\seealso{
  summariser fun dm_ply

  Other function creation: \code{\link{fun}},
  \code{\link{summariser}}
}

