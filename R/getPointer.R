#' Passed a list of dots arguments, builds a string that can be used
#' to check pointer-equivalence of the arguments, for example if the
#' same unevaluated arguments are being called multiple times in a
#' loop.
#'
#' @param ... A varying number of arguments.
#' @return A string based on unevauluated expressions of the arguments.
#' @author Peter Meilstrup
#' @useDynLib ptools
expression_digest <- function(...) .Call("expression_pointers", get("..."))

