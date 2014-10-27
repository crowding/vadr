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



#' Convert an environment into a ... object, without forcing promises.
#'
#' All bindings in the environment will be copied into a new
#' \code{\dots} list. Bindings that are promises will be added to the
#' \dots list without forcing, while other bindings will be wrapped in
#' an already-evaluated promise.  If `...` exists in the environment,
#' all bindings it contains will be added to the \dots list. The
#' output will not be in any particular order.
#'
#' @param envir An environment.
#' @param include_missing Whether to include "missing" bindings in the dotslist.
#' @return A \link{dots} object.
#' @useDynLib vadr _env_to_dots
#' @export
env2dots <- function(envir, include_missing=FALSE) {
  .Call(`_env_to_dots`, envir, ls(envir=envir, all.names=TRUE), include_missing)
}

#' Convert an a ... object into an environment, without forcing promises.
#'
#' All named entries in the dots object will be bound to
#' variables in the given environment. Unnamed entries
#' will be appended to any existing value of `...` in the order
#' in which they appear. Promises will not be forced.
#'
#' @param dots The dots object to convert.
#' @param envir if not NULL, an environment to populate. If NULL, a new
#' environment will be created.
#' @param parent If creating a new environment, the parent to use.
#' @param size The size of the new environment.
#' @param hash Whether the new environment should use a hashtable.
#' @return An environment object.
#' @useDynLib vadr _dots_to_env
#' @export
dots2env <- function(dots, envir = NULL,
                     parent = arg_env(dots, environment()),
                     hash = (length(dots) > 100),
                     size = max(29L, length(dots))) {
  force(parent)
  if (is.null(envir))
    envir <- new.env(hash = hash, parent = parent, size = size)

  .Call(`_dots_to_env`, dots, envir)
}
