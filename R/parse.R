## Extract substrings that look like .(expr), where expr is balanced.
## (following R rules for matching [], (), {}, '', "", ``, %%, \, and comments.)
## Returns a _list_ of _character arrays_ one array per each input string.
## Each character array containts an odd number of strings, alternating between
## "outside quote" and "inside quote,"
## Example:
##   find_subst_expressions("this .(is) a (.(test( '(' )))!")
##   ==> list(c("this ", "is", " a (", "test('(')", ")!"))
## Note proper paren and quote matching.
##
## Starting and ending delims may be specified. Unequal input lengths will
## recycle in the usual way.
#
# @param str The string vector to search.
# @param begin The beginning delimiter.
# @param end The ending delimiter.
#' @useDynLib vadr _find_subst_expressions_list
find_subst_expressions <- function(str, begin=".(", end=")") {
  .Call(`_find_subst_expressions_list`, str, begin, end)
}
