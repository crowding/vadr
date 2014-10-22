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

#' Interpolate evaluated expressions into strings.
#'
#' For \code{interpolate}, the argument is scanned for substrings that
#' look like \code{".(expr)"}. These are replaced by the evaluation of
#' the expression. Expressions will be matched respecting the
#' balancing of braces and quotes.
#'
#' \code{interply} generates a function that performs this
#' interpolation for a particular string. The interpolation is
#' repeated along the arguments applied to the function (so the usage
#' is \code{interply(".(x),.(y)")(x=val, y=val)}, similar to
#' \code{\link{mply}} and \code{\link{qqply}}.) Note unnamed arguments
#' can be referred to as \code{..1}, \code{..2}, etc.
#'
#' \code{\%#\%} is a shortcut for applying interpolation to data
#' from a list or named vector. \code{string \%#\% values} is equivalent to
#' \code{interply(string) \%()\% values}.
#'
#' @note If accepting a formatting string from user data, it is prudent
#' to specify \code{\link{emptyenv}()} or a descendant of \code{emptyenv()}
#' populated only with "allowed" functions and data.
#'
#' @title Evaluate expressions within strings.
#' @param str A template string. For \code{interply}, must have length 1.
#' @param begin The beginning delimiter.
#' @param end The ending delimiter.
#' @param envir The environment evaluation takes place in. Defaults to the
#' lexical environment of the template.
#' @return A character vector.
#' @seealso qq qqply mply
#' @aliases interply %#%
#' @rdname interpolate
#' @author Peter Meilstrup
#' @export
#' @examples
#' foo<-1; bar<-"two"; baz <- "III"
#' interpolate(c(".(foo),", "a .(bar)", "a .(foo) .(bar) .(baz)"))
#' interply("hello .(..1)")(c("world", "nurse", "kitty"))
#' interply("hello {{q}}", begin="{{", end="}}")(q=c("there", "you"))
#' "hello .(x)" %#% c(x="world")
#'
#' #shell-style interpolation -- "$" start and no end delim
#' interply("$hello, ${paste(rep('hello', 3), collapse=' ')}, $'hi'",
#'          begin="$", end=""
#'          )(hello="hola")
#'
#' # Compliant 99 Bottles implementation:
#' bottles <- interply(
#'   ".(ifelse(n%%100>0, n%%100, 'no more')) bottle.('s'[n%%100!=1]) of beer")
#' initcap <- function(x) {substr(x, 1, 1) <- toupper(substr(x, 1, 1)); x}
#' verse <- interply(
#'   paste0(".(initcap(bottles(n=n))) on the wall, .(bottles(n=n)).\n",
#'          ".(c('Go to the store and buy some more,',",
#'          "    'Take one down and pass it around,')[(n%%100!=0)+1])",
#'          " .(initcap(bottles(n=n-1))) on the wall."))
#' cat(verse(n=99:0), sep="\n\n")
interpolate <- function(str, begin=".(", end=")",
                        envir=arg_env(str, environment())) {
  force(envir)
  exprs <- find_subst_expressions(str, begin, end)
  chars <- lapply(exprs, interpolate_inner, envir)
  vapply(chars, paste0, "", collapse="")
}

interpolate_inner <- function(s, envir) {
  if (length(s) < 2) return(s)
  i <- seq(2, length(s), by=2)
  s[i] <- vapply(s[i],
                 function(x) as.character(
                   eval(parse(text=x), envir)), "")
  s
}

#' @rdname interpolate
#' @export
interply <- function(str, begin=".(", end=")",
                     envir=arg_env(str, environment())) {
  force(envir)
  if(length(str) != 1) stop("interpolate: need scalar character as template")
  exprs <- find_subst_expressions(str, begin, end)[[1]]
  i <- seq(2, length.out=(length(exprs)-1)/2, by=2)
  exprs[i] <- parse(text=exprs[i])
  expander <- qe(function(...) .(eval)(.(quote)(.(paste0)(..(exprs)))))
  environment(expander) <- envir
  interply_loop <- qq_applicator(expander)
  function(...) as.character(interply_loop(...))
}

#' @rdname interpolate
#' @usage str \%#\% args
#' @param args A list or named vector; interpolation happens in an environment
#' with these values bound.
#' @export
`%#%` <- function(str, args) {
  envir <- arg_env(str, environment())
  interply(str, envir=envir) %()% args
}
