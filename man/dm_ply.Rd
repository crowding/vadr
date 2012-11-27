\name{dm_ply}
\alias{dm_ply}
\title{Split data frame, apply columns to arguments of a function, and discard results.}
\usage{
  dm_ply(.data, .variables, .fun = NULL, ...,
    .progress = "none", .drop = TRUE, .parallel = FALSE)
}
\arguments{
  \item{.data}{data frame to be processed}

  \item{.variables}{variables to split data frame by, as
  quoted variables, a formula or character vector}

  \item{.drop}{should combinations of variables that do not
  appear in the input data be preserved (FALSE) or dropped
  (TRUE, default)}

  \item{.parallel}{if \code{TRUE}, apply function in
  parallel, using parallel backend provided by foreach}

  \item{.fun}{multiple argument function to apply to each
  piece.}

  \item{...}{other arguments passed on to \code{.fun}}

  \item{.progress}{name of the progress bar to use, see
  \code{\link{create_progress_bar}}}
}
\value{
  NULL
}
\description{
  For each subset of a data frame, apply its columns to a
  multiple-argument function.
}
\section{Function Application}{
  The columns of \code{.data} will be matched against the
  named arguments of the function \code{.fun}. Any
  arguments that do not match names will be ignored. For
  example, if \code{.data} has columns "X", "Y", and "Z",
  but \code{.fun} only has arguments "X" and "Y", then only
  "X" and "Y" will be used.  If \code{.fun} has an ellipsis
  argument, all columns of the data frame will be used.

  The functions \code{fun}, \code{mutator} and
  \code{summariser} provide some convenient shortcuts to
  make multiple-argument worker functions. In most cases
  you can replace d*ply calls using \code{link{with}},
  \code{link{summarize}} and \code{link{mutate}} with
  equivalent calls using \code{link{fun}},
  \code{link{summarizer}} and \code{link{mutator}}.
}

\section{Output}{
  This function discards its output. THis is useful for
  functions that you are calling purely for their side
  effects like display plots and saving output.
}
\references{
  Hadley Wickham (2011). The Split-Apply-Combine Strategy
  for Data Analysis. Journal of Statistical Software,
  40(1), 1-29.  \url{http://www.jstatsoft.org/v40/i01/}.
}
\seealso{
  fun mutator summariser

  Other data frame input: \code{\link{dmaply}},
  \code{\link{dmdply}}, \code{\link{dmlply}}

  Other multiarg worker: \code{\link{dmaply}},
  \code{\link{dmdply}}, \code{\link{dmlply}}
}
\keyword{manip}

