#' @include macro.R
NULL

#shut up CRAN
`.<-` <- function(x, ..., value) {stop("this shouldn't actually be called")}
`..` <- function(...) {stop("this shouldn't actually be called")}
`.` <- function(...) {stop("this shouldn't actually be called")}

#Exposed user interface for quasiquotation
#the internal engine (uq and friends) are defined in qq_fast.R
qq_internal <- function(expr) {
  reg <- new_registry()
  fun <- eval(call('function',
                   pairlist(...=quote(expr=)),
                   call("[[", uq(expr, reg), 1)))
  args <- reg(op="expressions")
  names(args) <- NULL
  as.call(c(list(fun), args))
}

#' Quasiquotation. Perform template substitutions on a quoted R expressions.
#'
#' This is an extended version of the \code{\link{bquote}}
#' utility.  'qq' quotes its first argument, then scans for
#' terms wrapped in \code{.()}, \code{..()}, or names that match
#' \code{`.()`} The wrapped expressions or names are evaluated in the
#' given environment.  Expressions wrapped in \code{..()} will be
#' interpolated into the argument list in which they occur. Names
#' wrapped in `.()` will be substituted and coerced to name.
#'
#' Invocations of qq() can be nested within the \code{.()}
#' sections and they should work as promised.
#'
#' @param expr An expression, left unevaluated.
#' @return For \code{qq}, A language object; for \code{qe}, evaluates
#'         the expression in the calling environment.
#' @author Peter Meilstrup
#' @aliases template qe
#' @seealso macro bquote substitute quoting.env
#' @examples
#'
#' #Basic substitution
#' default <- 1
#' qq( function(x, y = .(default)) x+y )
#' #function(x, y = 1) x + y
#'
#' # splicing substitution:
#' paste.before <- alist("hello", "cool")
#' paste.after <- alist("!", "Now is", date())
#' qq(cat(...(paste.before), "world", ...(paste.after), '\n'))
#' #cat("hello", "cool", "world", "!", "Now is", date(), "\n")
#'
#' # Name substitution:
#' element_to_access <- "x"
#' qq(function(data) data$`.(element_to_access)`)
#' #function(data) data$x
#'
#' argument.name <- "x"
#' qq(
#'   function(`.(argument.name)`)
#'   cat(.(argument.name), " is ", `.(argument.name)`, "\n")
#' )
#' #function(x) cat("x", " is ", x, "\n"))
#'
#' # Note that in the argument list to a function, function argument
#' # names are names, and defaults are values; that is
#' function(x=1, y) x+y
#' # is equivalent to
#' function(.=...(alist(x=1, y=))) x+y
#' # or
#' function(.=...(list(x=1, y=missing_value()))) x+y
#'
#' # Building a function with an arbitrary list of arguments:
#' argnames <- letters[1:4]
#' qq(function(.=...(put(missing_value(length(argnames)), names, argnames))) {
#'   list(...(lapply(argnames, as.name)))
#' })
#' #function(a, b, c, d) list(a, b, c, d)
#'
#' # The poor .() function is overloaded. Usually can escape it by adding
#' # parens:
#' dfname <- "baseball"
#' qq(ddply(.(as.name(dfname)), (.)(id, team), identity))
#' #ddply(baseball, .(id.team), identity)
#' @export
qq <- macro(qq_internal)

#' @export
template <- qq

#' @export
qe <- macro(function(expr) {
  as.call(c(list(eval), list(qq_internal(expr))))
})

#' Repeatedly expand an expression against sequences of values.
#'
#' Performs template expansion as for \code{\link{qq}}, but evaluates over
#' sequences (as in \code{\link{mapply}}).
#'
#' @usage qqply(...)(...)
#' @usage qeply(...)(...)
#' @param ... In the first argument list, one or more expressions or expressions
#' to expand. These may have names, which will also be expanded.
#' In the second argument list, vectors with optional names. The expressions
#' will be expanded in a context that has these names bound to one value at a
#' time from each sequence (inheriting from the calling frame).
#' @return For \code{qqply}, a list of expressions. For \code{qeply}, the
#' expressions will be evaluated in the calling frame.
#' @aliases qeply
#' @seealso qq bquote
#' @author Peter Meilstrup
#' @examples
#'
#' qqply(`.(x)` = .(y))(x=letters[1:3], y=1:3)
#'
#' qe(function(
#'     .=...( qqply(`.(..1)`=.(..2))(letters, 1:26))) {
#'   ...(qqply(.(as.name(x)) <- .(as.name(y)))(y=letters[2:26], x=letters[1:25]))
#'   e
#' })
#' @export
qqply <- macro(function(...) {
  collection <- as.call(list(c, ...))
  expand_expr <- qq_internal(collection)
  expand_fn_expr <- qq(function() {
    .(`[`)(.(as.list)(.(expand_expr)), -1)
  })
  qq(.(qq_applicator)(.(expand_fn_expr)))
})

#' @export
qeply <- macro(function(...) {
  collection <- as.call(c(list(c), list(...)))
  expand_expr <- qq_internal(collection)
  expand_fn_expr <- qq(function(...) {
    .(eval)({.(expand_expr)}, .(parent.env)(.(environment)()))
  })
  qq(.(qq_applicator)(.(expand_fn_expr)))
})

qq_applicator <- function(expander) {
  function(...) {
    argnames <- dots_names(...)
    argnames <- argnames[argnames != NA]
    formals(expander) <-
        as.pairlist(c(list(...=quote(expr=)),
                      structure(missing_value(length(argnames)),
                                names=argnames)))
    unlist(mply(expander)(...), recursive=FALSE)
  }
}
