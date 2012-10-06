##' Given a list of names, build an environment such that evaluating
##' any expression using those names just gets you the expression
##' back.
##'
##' Technically, this defines two nested environments, the outer
##' containing functions and the inner containing names, and returns
##' the inner.
##'
##' This somewhat esoteric function mostly intended to be used by
##' \code{\link{expand.macro}}
##'
##' @note This will cause errors when the expression has missing
##' arguments. The expression might be preprocessed (somewhow?) to take missing
##' arguments out. This also doesn't yet do the right thing with dots.
##'
##' @param names The names the environment should define.
##' @param parent The parent environment (defaults to the empty environment)
##' @param call.names The functions the enclosing environment should
##' define. Decaults to \code{names}, but sometimes you want these to
##' be different.
##' @return The environment constructed.
##' @author Peter Meilstrup
##' @export
##' @examples
##' en <- recapitulating.environment(c('+', '(', 'a', 'b', '*'), environment())
##'
##' evalq(a+b, en) # a+b
##' evalq(a+(b*a), en) # a+(b*a)
##' z <- 100
##' evalq(a+(b*a)*z, en) #a+(b*a)*100
##'
##' ##We can build a function that does something like substitute() like this:
##' ersatz.substitute <- function(expr, envir=parent.frame()) {
##'   parent <- as.environment(envir)
##'   en <- recapitulating.env(setdiff(all.names(expr), ls(parent)), parent)
##'   eval(expr, en)
##' }
##'
##' ersatz.substitute(quote(a+b+c), list(b=quote(q+y))) # returns a+(q+y)+c
##'
recapitulating.env <- function(names, parent=emptyenv(), call.names=names) {
  #There probably needs to be special handling
  #for function() to get the elements of the pairlist evaluated, too.
  callenv <- new.env(parent=parent)
  for (n in as.character(call.names)) {
    f <- eval(substitute(
                function(...) as.call(list(quote(x), ...)),
                list(x=as.name(n))))
    assign(n, f, envir=callenv)
  }
  if ("..." %in% names) {
    capture.dots <- function(...) environment()
    nameenv <- eval(call("capture.dots", quote(quote(...))))
    parent.env(nameenv) <- callenv
  } else {
    nameenv <- new.env(parent=callenv)
  }
  for (n in names) {
    if (n != "...") {
      assign(n, as.name(n), envir=nameenv)
    }
  }
  nameenv
}

#like all.names, but filter out only the words that are used "bare" in
#a particular expression. That is, leave out words that are used as calls.
all.bare.names <- function(expr) {
  callenv <- new.env(parent=emptyenv)
  for(n in call.names) {
    namecollector <- function(...)
      Filter(unlist(list(...)), is.name)
  }
}
