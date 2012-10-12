\name{with_arg}
\alias{with_arg}
\title{Inject named arguments into several calls and evaluate those calls.}
\usage{
  with_arg(..., .collect = list, .envir = parent.frame(),
    .override = FALSE)
}
\arguments{
  \item{...}{Named arguments are interpreted as arguments
  to inject. Unnamed arguments are interpreted as calls to
  evaluate.}

  \item{.collect}{Which function to use to collect the
  results of all the subcalls. Default is `list'.}

  \item{.envir}{The environment to evaluate in. Defaults to
  the environment that called with_arg.}

  \item{.override}{Whether to override arguments that
  appear to conflict. Default is FALSE.}
}
\value{
  The results of the evaluated calls, collected using
  \code{.collect}.
}
\description{
  For a simple example, writing \code{with_args(a="repeated
  argument", func(b=2), func(c=3), .collect=list)}
}
\details{
  is equivalent to writing

  \code{list(fun(a="repeated argument",b=2),
  func(a="repeated argument",c=3))}

  so that with_args handles the job of distributing the
  repeated 'a' argument. This can save some typing in some
  situations, like heavily layered ggplot constructions.

  We try to interpret each subcall according to R argument
  matching rules, by looking up the function's formal
  arguments using \code{\link{match.fun}}.  This might run
  into problems with generic functions. On second thought,
  we should be more like a macro and not do this.
}
\author{
  Peter Meilstrup
}

