# Passed a list of dots arguments, returns their expressions and, in
# names, their machine pointers
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

#' @useDynLib vadr _object_pointers
#' # turns out this is almost useless due to all sexps
#' # being duped when they go into a list.
object_pointers <- function(list) {
  .Call(`_object_pointers`, list)
}
