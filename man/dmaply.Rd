\name{dmaply}
\alias{dmaply}
\title{Split data frame, apply columns to arguments of a function, and return results in an array.}
\usage{
  dmaply(.data, .variables, .fun = NULL, ...,
    .progress = "none", .drop_i = TRUE, .drop_o = TRUE,
    .parallel = TRUE)
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

  \item{.drop}{should extra dimensions of length 1 in the
  output be dropped, simplifying the output.  Defaults to
  \code{TRUE}}
}
\value{
  if results are atomic with same type and dimensionality,
  a vector, matrix or array; otherwise, a list-array (a
  list with dimensions)
}
\description{
  For each subset of a data frame, apply the columns to the
  arguments of a multiple-argument function, then combione
  the results into a data frame.
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
  If there are no results, then this function will return a
  vector of length 0 (\code{vector()}).
}
\examples{
daply(baseball[, c(2, 6:9)], .(year), argwise(mean))
daply(baseball[, c(6:9)], .(baseball$year), argwise(mean))
daply(baseball, .(year), argwise(mean, .args=names(df[6:9])))
}
\references{
  Hadley Wickham (2011). The Split-Apply-Combine Strategy
  for Data Analysis. Journal of Statistical Software,
  40(1), 1-29.  \url{http://www.jstatsoft.org/v40/i01/}.
}
\seealso{
  fun mutator summariser

  Other data frame input: \code{\link{dmdply}},
  \code{\link{dmlply}}

  Other multiarg worker: \code{\link{dmdply}},
  \code{\link{dmlply}}
}
\keyword{manip}

