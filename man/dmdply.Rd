\name{dmdply}
\alias{dmdply}
\title{Split data frame, apply columns to arguments of a function, and return results in a data frame.}
\usage{
  dmdply(.data, .variables, .fun = NULL, ...,
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
  A data frame, as described in the output section.
}
\description{
  For each subset of a data frame, apply a
  multiple-argument function, then combine the results into
  a data frame.
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
  The most unambiguous behaviour is achieved when
  \code{.fun} returns a data frame - in that case pieces
  will be combined with \code{\link{rbind.fill}}.  If
  \code{.fun} returns an atomic vector of fixed length, it
  will be \code{rbind}ed together and converted to a data
  frame. Any other values will result in an error.

  If there are no results, then this function will return a
  data frame with zero rows and columns
  (\code{data.frame()}).
}
\examples{
dmdply(baseball, .(year), function(year) length(year))
#or equivalently
dmdply(baseball, .(year), fun(length(year)))

rbi <- dmdply(baseball, .(year),
     summariser(mean_rbi = mean(rbi, na.rm=TRUE)))
with(rbi, plot(year, mean_rbi, type="l"))

base2 <- dmdply(baseball, .(id),
    mutator(career_year=year - min(year) + 1))
}
\references{
  Hadley Wickham (2011). The Split-Apply-Combine Strategy
  for Data Analysis. Journal of Statistical Software,
  40(1), 1-29.  \url{http://www.jstatsoft.org/v40/i01/}.
}
\seealso{
  fun mutator summariser

  Other data frame input: \code{\link{dmaply}},
  \code{\link{dmlply}}

  Other multiarg worker: \code{\link{dmaply}},
  \code{\link{dmlply}}
}
\keyword{manip}

