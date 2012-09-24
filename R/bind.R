##' Unpack a list and assign to multiple variables.
##'
##' This is a "destructuring bind" for R. It can be used to unpack
##' lists into different values, or achieve the effect of multiple
##' return values from a function.
##'
##' @examples
##' # For a simple example,
##' bind(a=x, b=y) <- c(a="foo", b="bar")
##' x # -> "foo"
##' y # -> "bar"
##'
##' # Note that the assigned-to variable is on the _right_ side of each
##' # equals in the arglist. This is admittedly awkward but is the best
##' # way to remain consistent with R's argument-binding semantics.
##'
##' # Element to variable matching happens according to R's argument
##' # binding rules, can be nested, and complicated assignemnt targets
##' # can be used. The following shows off a complicated unpacking:
##'
##' bind[a=x, ...=bind[aa=xx, bb=yy], b=y, cc=bind[a=zz[2], b=zz[1]]] <-
##'   list(a=1, b="two", aa="eleven", bb=22, cc=c(a=1, b=2))
##' x
##' y
##' xx
##' yy <- 22
##' zz <- c(2,1)
##'
##' # x <- 1; y <- "two", xx <- "eleve
##'
##' @param ...
##' @param .envir The environment to bind in (defaults to the caller).
##' @aliases bind<- [<-.bind <-.bind
##' @return The list that was originally unpacked.
##' @author Peter Meilstrup
##' @export
bind <- "use bind[a=x, b=y] <- c(a=1,b=2) to do parallel assignment."
class(bind) <- "bind"

`[<-.bind` <- function(..., .envir=parent.frame()) {
  #now the problem is to get the "calling frame" all the way thorough
  #whatever dispatch mechanism is in place.
  print(eval(substitute(alist(...))))
  stop("not written!")
}

##' @export
`[.bind` <- function(tmp) {
  stop("bind[...] must be used as the target of an assignment.");
}
