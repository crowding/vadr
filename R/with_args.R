##' Inject named arguments into several calls and evaluate those calls.
##'
##' For a simple example, writing
##' \code{with_args(a="repeated argument", func(b=2), func(c=3), .collect=list)}
##'
##' is equivalent to writing
##'
##' \code{list(fun(a="repeated argument",b=2), func(a="repeated argument",c=3))}
##'
##' so that with_args handles the job of distributing the repeated 'a'
##' argument. This can save some typing in some situations, like
##' heavily layered ggplot constructions.
##'
##' @param ... Named arguments are interpreted as arguments to
##' inject. Unnamed arguments are interpreted as calls to evaluate.
##' @param .collect Which function to use to collect the results of
##' all the subcalls. Default is `list'.
##' @param .envir The environment to evaluate in. Defaults to the
##' environment that called with_arg.
##' @return The results of the evaluated calls, collected using \code{.collect}.
##' @author Peter Meilstrup
##' @export
with_arg <- function(..., .collect=list, .envir=parent.frame())  {
  dots <- as.list(substitute(quote(...)))[-1]
  calls <- dots[names(dots) == '']
  args <- dots[names(dots) != '']

  rebuiltCalls <- list()
  for (theCall in calls) {
    theFun <- match.fun(as.list(theCall)[[1]])
    if (!is.primitive(theFun)) {
      theCall <- match.call(theFun, theCall)
    }
    theCall <- as.list(theCall)
    safe.args <- setdiff(names(args), names(call))
    theCall[safe.args] = args[safe.args]

    rebuiltCalls <- c(rebuiltCalls, as.call(theCall))
  }
  theCollection <- as.call(c(list(.collect), rebuiltCalls))
  eval(theCollection, envir=.envir)
}
