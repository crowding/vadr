##' @section Function Creation: The arguments given are captured
##' unevaluated. Every name used as a variable (that is, not as a
##' call) is noted and the resulting function will have an argument
##' matching each name. If values for the argument are not given, the
##' default will look up the argument in the environment from which
##' the function was defined.
##'
##' Ellipsis arguments (\code{...}) are also noted, and a matching
##' ellipsis argument will be given to the resulting function.
##'
##' @param ... A list of expressions to evaluate sequentially.  For
##' code{\link{summariser}} and \code{link{mutator}} the argument
##' names will used as column names of the output data frame.
##' @param .envir The environment the function should be enclosed
##' in. Defaults to the calling environment.
##'
##' @return A newly constructed function.
##' @family function creation
