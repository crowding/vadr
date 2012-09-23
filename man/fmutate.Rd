\name{fmutate}
\alias{fmutate}
\title{Create a function of several arguments returning a data frame.}
\usage{
  fmutate(..., _envir = parent.frame())
}
\arguments{
  \item{...}{A series of named arguments.}

  \item{.envir}{The environment to create the function in;
  defaults to the caller.}
}
\value{
  The newly created function.
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
\author{
  Peter Meilstrup
}
\seealso{
  fsummarise fun dm_ply
}

