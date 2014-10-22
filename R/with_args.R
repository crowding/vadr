#' @include macro.R
NULL

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
##' @return The results of the evaluated calls, collected using \code{.collect}.
##' @author Peter Meilstrup
##' @export
with_arg <- macro(function(..., .collect=quote(list)) {
  args <- list(...)
  if (is.null(names(args))) names(args <- rep("", length(args)))
  calls <- args[names(args) == ""]
  args <- args[names(args) != ""]
  chain(calls, lapply(as.list), lapply(c, args),
        lapply(as.call), c(.collect, .), as.call)
})
