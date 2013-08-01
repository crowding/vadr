# Passed a list of dots arguments, returns their expressions and, in
# names, their machine pointers
#
# @param ... A varying number of arguments.
# @return A vector of integers containing pointer values, one per argument.
# @author Peter Meilstrup
#' @useDynLib vadr _expressions_and_pointers
expressions_and_pointers <- function(...) if (!missing(...))
  .Call(`_expressions_and_pointers`, get("...")) else list("NULL"=NULL)
