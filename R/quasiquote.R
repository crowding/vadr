
#' Quasiquotation. Perform template substitutions on a quoted R expressions.
#'
#' This is an extended version of the \code{\link{backquote}}
#' utility.  'qq' quotes its first argument, then scans for
#' terms wrapped in \code{.()}, \code{...()}, or names that match
#' \code{`.()`} The wrapped expressions or names are evaluated in the
#' given environment.  Expressions wrapped in \code{...()} will be
#' interpolated into the argument list in which they occur. Names
#' wrapped in `.()` will be substituted and coerced to name.
#'
#' Invocations of qq() can be nested within the \code{.()}'s
#' section and they should work as promised.
#'
#' @param expr A language object.
#' @param .envir An environment to evaluate the backquoted expressions in.
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
#' # interpolating substitution:
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
#' function(...(alist(x=1, y=))) x+y
#' # or
#' function(...(list(x=1, y=missing_value()))) x+y
#'
#' # Building a function with an arbitrary list of arguments:
#' argnames <- letters[1:4]
#' qq(function(.=...(setNames(missing_value(length(argnames)), argnames))) {
#'   list(...(lapply(argnames, as.name)))
#' })
#' #function(a, b, c, d) list(a, b, c, d)
#'
#' #' The poor .() function is overloaded. You can escape it thus:
#' dfname <- "baseball"
#' qq(ddply(.(as.name(dfname)), .(quote(.(id, team))), identity))
#' #ddply(baseball, .(id.team), identity)
#' @import stringr
#' @export
qq <- function(expr, .envir=parent.frame()) {
  unquote <- function(e) {
    if (is.pairlist(e)) {
      as.pairlist(unquote.list(e))
    } else if (is.call(e)) {
      if (e[[1L]] == as.name(".")) {
        eval(e[[2L]], .envir)
      } else if (e[[1L]] == as.name("...")) {
        structure(eval(e[[2L]], .envir), ...=TRUE)
      } else {
        as.call(unquote.list(e))
      }
    } else if (length(e) <= 1L) {
      if (is.name(e)) {
        ch <- unquote.char(as.character(e))
        if (ch == "") {
          quote(expr=)
        } else {
          as.name(ch)
        }
      } else {
        e
      }
    } else {
      unquote.list(e)
    }
  }

  unquote.list <- function(e) {
    r <- lapply(e, unquote)
    n <- names(e)
    to.interpolate <- vapply(r, function(x) attr(x, '...') %||% FALSE, FALSE)
    if (!is.null(n)) {
      n[to.interpolate] <- ""
      n <- vapply(n, unquote.char, "")
      names(r) <- n
    }
    if (any(to.interpolate)) {
      r[!to.interpolate] <- structure(lapply(r[!to.interpolate], list),
                                      names = names(r)[!to.interpolate])
      r <- unlist(r, recursive=FALSE)
    }
    r
  }

  unquote.char <- function(ch) {
    match <- str_match(ch, "^\\.\\((.*)\\)$")
    if (!is.na(match[2])) {
      as.character(eval(parse(text = match[2]), .envir))
    } else {
      ch
    }
  }

  unquote(substitute(expr))
}

#' @export
template <- qq

#' @export
qe <- function(expr, .envir=parent.frame()) {
  eval(do.call("qq", list(expr=substitute(expr), .envir=.envir)), .envir)
}
