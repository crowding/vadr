\name{interpolate}
\alias{\%#\%}
\alias{interply}
\alias{interpolate}
\title{Evaluate expressions within strings.}
\usage{
  interpolate(text, begin = ".(", end = ")",
    envir = parent.frame())

  interply(text, begin = ".(", end = ")", envir =
    parent.frame())(...)

  text %#% args
}
\arguments{
  \item{str}{A string. For \code{interply}, must have
  length 1.}

  \item{begin}{The beginning delimiter.}

  \item{end}{The ending delimiter.}

  \item{envir}{The environment evaluation takes place in.}
}
\value{
  A character vector.
}
\description{
  Interpolate evaluated expressions into strings.
}
\details{
  For \code{interpolate}, the argument is scanned for
  substrings that look like \code{".(expr)"}. These are
  replaced by the evaluation of the expression. Expressions
  will be matched respecting the balancing of braces and
  quotes.

  \code{interply} generates a function that performs this
  interpolation for a particular string, when called with
  an argument. The interpolation is repeated along the
  arguments applied to the function (so the usage is
  \code{interply(".(x),.(y)")(x=val, y=val)}, similar to
  \code{\link{mply}} and \code{\link{qqply}}.) Note unnamed
  arguments can be referred to as \code{..1}, \code{..2},
  etc.

  \code{\%#\%} is a shortcut for applying interpolation to
  data from a list or named vector. \code{string \%#\%
  values} is equivalent to \code{interply(string) \%()\%
  values}.
}
\note{
  If accepting a formatting string from user data, it is
  prudent to specify \code{\link{emptyenv}()} or a
  descendant of \code{emptyenv()} populated only with
  "allowed" functions and data.
}
\examples{
foo<-1; bar<-"two"; baz <- "III"
interpolate(c(".(foo),", "a .(bar)", "a .(foo) .(bar) .(baz)"))
interply("hello .(..1)")(c("world", "nurse", "kitty"))
interply("hello {{q}}", begin="{{", end="}}")(q=c("there", "you"))
"hello .(x)" \%#\% c(x="world")

# Compliant 99 Bottles implementation:
bottles <- interply(
  ".(ifelse(n\%\%100>0, n\%\%100, 'no more')) bottle.('s'[n\%\%100!=1]) of beer")
initcap <- function(x) {substr(x,1,1) <- toupper(substr(x, 1, 1)); x}
verse <- interply(
  paste0(".(bottles(n=n)) on the wall, .(bottles(n=n)).\\n",
         ".(c('Go to the store and buy some more,',",
         "    'Take one down and pass it around,')[(n\%\%100!=0)+1])",
         " .(initcap(bottles(n=n-1))) on the wall."))
cat(verse(n=99:0), sep="\\n\\n")
}
\author{
  Peter Meilstrup
}
\seealso{
  qq qqply mply
}

