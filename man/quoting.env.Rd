\name{quoting.env}
\alias{quoting.env}
\title{Given a list of names, build an environment such that evaluating
any expression using those names just gets you the expression
back.}
\usage{
  quoting.env(names, parent = emptyenv(),
    call.names = names)
}
\arguments{
  \item{names}{The names the environment should define.}

  \item{parent}{The parent environment (defaults to the
  empty environment)}

  \item{call.names}{The functions the enclosing environment
  should define. Decaults to \code{names}, but sometimes
  you want these to be different.}
}
\value{
  The environment constructed.
}
\description{
  Technically, this defines two nested environments, the
  outer containing functions and the inner containing
  names, and returns the inner.
}
\details{
  This somewhat esoteric function mostly intended to be
  used by \code{\link{expand.macro}}
}
\note{
  This will cause errors when the expression has missing
  arguments. The expression might be preprocessed
  (somewhow?) to take missing arguments out.
}
\examples{
en <- recapitulating.environment(c('+', '(', 'a', 'b', '*'), environment())

evalq(a+b, en) # a+b
evalq(a+(b*a), en) # a+(b*a)
z <- 100
evalq(a+(b*a)*z, en) #a+(b*a)*100

##We can build a function that does something like substitute() like this:
ersatz.substitute <- function(expr, envir=parent.frame()) {
  parent <- as.environment(envir)
  en <- recapitulating.env(setdiff(all.names(expr), ls(parent)), parent)
  eval(expr, en)
}

ersatz.substitute(quote(a+b+c), list(b=quote(q+y))) # returns a+(q+y)+c
}
\author{
  Peter Meilstrup
}

