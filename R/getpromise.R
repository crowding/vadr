#' Fetch promises associated with named arguments.
#'
#' @param ... Variable names (unevaluated). Arguments may be named; these names
#' determine the names on the dots list (and not the variable names)
#' @return a \code{\link{dots}} object containing the promises that are bound to
#' those variables in the calling environment.
#' @author Peter Meilstrup
#' @note The tags on the dots object are determined by argument names;
#' variable names are discarded.
#' @seealso dots get_dots
#' @rdname arg_list
#' @export
#' @useDynLib vadr _getpromise_in
arg_dots <- function(...) {
  d <- unpack(dots(...))
  .Call(`_getpromise_in`, d$envir, d$expr, d$name)
}

#' ...
#'
#' \code{arg_get} fetches arguments from a named list.
#' @rdname arg_list
#' @param names A character vector or list of names.
#' @param envir The environment to look for the argument names in. By default
#' looks in the lexical environment of the \code{name} argument.
get_dots <- function(names, envir=arg_env(names, environment())) {
  force(envir)
  tags <- names(names) %||% rep("", length(names))
  names <- lapply(names, as.name)
  .Call(`_getpromise_in`, rep(list(envir), length(names)), names, tags)
}

#' Get environment or expression from a named argument.
#'
#' \code{arg_env} determines the lexical scope of an argument (which must be an
#' un-evaluated promise).
#' @rdname arg_env
#' @param name A single argument name; not evaluated.
#' @param envir The environment to look for the argument name in. By default
#' looks in the lexical environment of the \code{name} argument.
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
#' @rdname arg_env
#' @useDynLib vadr _arg_expr
#' @export
arg_expr <- function(name,
                     envir=arg_env(name, environment())) {
  .Call(`_arg_expr`, envir, substitute(name))
}



# extract all items of an environment to a dots list, without
# forcing any promises.
#' @useDynLib vadr _env_to_dots
env2dots <- function(envir) {
  .Call(`_env_to_dots`, envir, ls(envir=envir, all.names=TRUE))
}
