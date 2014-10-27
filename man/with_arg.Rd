\name{with_arg}
\alias{with_arg}
\title{Inject named arguments into several calls and evaluate those calls.}
\usage{
with_arg(..., .collect = quote(list))
}
\arguments{
  \item{...}{Named arguments are interpreted as arguments
  to inject. Unnamed arguments are interpreted as calls to
  evaluate.}

  \item{.collect}{Which function to use to collect the
  results of all the subcalls. Default is `list'.}
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

\code{list(fun(a="repeated argument",b=2), func(a="repeated
argument",c=3))}

so that with_args handles the job of distributing the
repeated 'a' argument. This can save some typing in some
situations, like heavily layered ggplot constructions.
}
\author{
Peter Meilstrup
}

