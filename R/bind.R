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
##' # binding rules, can be nested, and complicated assignment targets
##' # can be used. The following shows off a complicated unpacking:
##'
##' bind[a=x, .rest=bind[aa=xx, bb=yy], b=y, cc=bind[a=zz[2], b=zz[1]]] <-
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

#' @export
`[<-.bind` <- function(`*tmp*`, ..., .envir=parent.frame()) {
  assignments <- eval(substitute(alist(...)))
  values <- assignments[[length(assignments)]]
  values <- eval(values, .envir)
  assignments[length(assignments)] <- NULL

  #make temp names.
  if (is.null(names(assignments))) {
    missing.names <- rep(TRUE, length(assignments))
    names(assignments) <- rep("", length(assignments))
  } else {
    missing_names <- names(assignments) == ""
    names(assignments)[missing_names] <-
      paste("*tmp*", seq(len=sum(missing_names)), sep="")
  }

  #use arg matching
  f <- function() match.call(expand.dots=FALSE)
  formals(f) <- assignments
  matched.values <- do.call("f", as.list(values))

  #assign matched values.
  if ("..." %in% names(assignments)) {
    mode(matched.values$...) <- mode(values)
  }
  for (assignFrom in names(assignments)) {
    assignTo <- assignments[[assignFrom]]
    if (!missing(assignTo)) {
      eval(substitute(target <- value,
                      list(target=assignTo,
                           value=matched.values[[assignFrom]])),
           .envir)
    }
  }

  bind
}

##' @export
`[.bind` <- function(tmp) {
  stop("bind[...] must be used as the target of an assignment.");
}
