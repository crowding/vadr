#' Return the raw pointer of each (evaluated) argument.
#'
#' @param ... A varying number of arguments.
#' @return A vector of integers containing pointer values, one per argument.
#' @author Peter Meilstrup
#' @export
#' @useDynLib ptools
getPointer <- function(...) .External("getPointer", ...)
