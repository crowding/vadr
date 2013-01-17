#' Partially apply arguments to a function.
#'
#' The arguments given in brackets are saved and a new function is
#' constructed.
#'
#' The difference between
#' \code{'[.function'} and \code{'[.function'} is that the single
#' bracket applies arguments to the right, while the double bracket
#' applies arguments to the left.
#'
#' This curry, captures the arguments you give as promises; they will
#' only be evaluated once. if the curried function chooses to do
#' so. For another type of curry see addDefaults.
#'
#' @examples
#' x <- paste[["prefix"]]
#' y <- paste["suffix", sep="; "]
#' x("data")
#' y("data")
#' lapply
#'
#' @param .fun A function.
#' @param ... The arguments to apply.
#' @return A function
#' @aliases "[[.function"
#' @seealso addDefaults
#' @author Peter Meilstrup
#' @method "[" function
#' @S3method "[" function
#' @useDynLib ptools
#' @export
`[.function` <- function(.fun, ...) {
  # OOPS this won't work because primitives don't dispatch on objects;
  # see help(primitiveMethods)
  stored=get("...")
  function(...) .Call("call_combining_dotlists", f, get("..."), stored)
}

#' @method "[" function
#' @S3method "[" function
#' @useDynLib ptools
#' @export
`[[.function` <- function(.fun, ...) {
  stored=get("...")
  function(...) .Call("call_combining_dotlists", f, stored, get("..."))
}
