#' @export

#' Fetch promises associated with named arguments.
#'
#' @usage arg_list(\dots)
#' arg_list[envir](\dots)
#' @param ... Variable names (unevaluated). Arguments may be named; these names
#' determine the names on the dots list (and not the variable names)
#' @return a \code{\link{dots}} object containing the promises that are bound to
#' those variables in the calling environment.
#' @author Peter Meilstrup
#' @note The tags on the dots object are determined by argument names;
#' variable names are discarded.
#' @seealso dots
#' @export
#' @useDynLib vadr _getpromise_in
arg_dots <- function(...) {
  d <- unpack(dots(...))
  .Call(`_getpromise_in`, d$envir, d$expr, d$name)
}
class(arg_dots) <- "arg_dots"

#' ...
#'
#' \code{arg_get} fetches arguments from a named list.
#' @rdname arg_list
#' @param names A character vector or list of names.
get_dots <- function(names, envir=arg_env(names, environment())) {
  force(envir)
  tags <- names(names) %||% rep("", length(names))
  names <- lapply(names, as.name)
  .Call(`_getpromise_in`, rep(list(envir), length(names)), names, tags)
}

#' ...
#'
#' arg_list may be told to look in a particular environment by supplying
#' the environment in brackets.
#' @S3method [ arg_list
#' @rdname arg_list
#' @usage arg_list[envir](...)
#' @param obj N/A.
#' @param envir Where to look for the named arguments.
#' @useDynLib vadr _getpromise_in
`[.arg_list` <- function(arg_list, envir) {
  function(...) {
    names <- list_quote(...)
    .Call(`_getpromise_in`, envir, names)
  }
}

#' ...
#'
#' \code{arg_env} determines the lexical scope of an argument (which must be an
#' un-evaluated promise).
#' @rdname arg_list
#' @param name A single argument name; not evaluated.
#' @useDynLib vadr _arg_env
#' @export
arg_env <- function(name,
                     envir=arg_env(name, environment())) {
  .Call(`_arg_env`, envir, substitute(name))
}

#' ...
#'
#' \code{arg_expr} fetches the expression attached to an argument in the given
#' environment. The effect is similar to \code{substitute(name)} but more
#' specific.
#' @rdname arg_list
#' @useDynLib vadr _arg_expr
#' @export
arg_expr <- function(name,
                     envir=arg_env(name, environment())) {
  .Call(`_arg_expr`, envir, substitute(name))
}
