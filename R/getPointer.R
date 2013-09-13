# Passed a list of dots arguments, returns their expressions and, in
# names, their machine pointers.
#
# This is used to obtain unique identifying strings for a list of arguments
#
# @param ... A varying number of arguments.
# @return A vector of integers containing pointer values, one per argument.
# @author Peter Meilstrup
#' @useDynLib vadr _expressions_and_pointers
expressions_and_pointers <- function(...) {
  if (!missing(...))
      .Call(`_expressions_and_pointers`, get("..."))
  else list("NULL"=NULL)
}

## turns out this is almost useless due to all sexps
## being duped when they go into a list. Can be useful
## for limited cases, but test them
#' @useDynLib vadr _object_pointers
object_pointers <- function(list) {
  .Call(`_object_pointers`, list)
}
