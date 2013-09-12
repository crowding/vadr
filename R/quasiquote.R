#Exposed user interface for quasiquotation
#the internal engine (uq and friends) are defined in qq_fast.R
qq_internal <- function(expr) {
  reg <- new_registry()
  fun <- eval(call('function',
                   pairlist(...=quote(expr=)),
                   call("[[", uq(expr, reg), 1)))
  args <- reg(op="expressions")
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
#' @param expr A language object.
#' @param envir An environment to evaluate the backquoted expressions in.
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
qq <- macro(qq_internal)

unattr <- function(x) `attributes<-`(x, NULL)

unquote <- function(e, envir=parent.frame()) {
  if (is.pairlist(e)) {
    as.pairlist(unquote.list(e, envir))
  } else if (is.call(e)) {
    if (e[[1]] == quote(`function`) && length(e) > 3)
        e <- e[1:3] # strip srcref
    if (e[[1L]] == as.name(".")) {
      eval(e[[2L]], envir)
    } else if (e[[1L]] == as.name("...")) {
      structure(eval(e[[2L]], envir), ...=TRUE)
    } else {
      as.call(unquote.list(e, envir))
    }
  } else if (!is.recursive(e)) {
    if (is.name(e)) {
      ch <- unquote.char(as.character(e), envir)
      if (ch == "") {
        quote(expr=)
      } else {
        as.name(ch)
      }
    } else {
      e
    }
  } else {
    unquote.list(e, envir)
  }
}

unquote.list <- function(e, envir=parent.frame) {
  r <- lapply(e, unquote, envir)
  n <- names(e)
  to.interpolate <- vapply(r, function(x) attr(x, '...') %||% FALSE, FALSE)
  if (!is.null(n)) {
    n[to.interpolate] <- ""
    n <- vapply(n, unquote.char, "", envir)
    names(r) <- n
  }
  if (any(to.interpolate)) {
    r[!to.interpolate] <- structure(lapply(r[!to.interpolate], list),
                                    names = names(r)[!to.interpolate])
    r <- unlist(r, recursive=FALSE)
  }
  r
}

unquote.char <- function(ch, envir=parent.frame()) {
  match <- str_match(ch, "^\\.\\((.*)\\)$")
  if (!is.na(match[2])) {
    as.character(eval(parse(text = match[2]), envir))
  } else {
    ch
  }
}


#' @export
template <- qq

#' @export
qe <- function(expr, envir=parent.frame()) {
  eval(unquote(substitute(expr), envir), envir)
}

#' Repeatedly expand an expression against sequences of values.
#'
#' Performs template expansion as for \code{\link{qq}}, but evaluates over
#' sequences (as in \code{\link{mapply}}).
#'
#' @usage qqply(...)(...)
#' @usage qeply(...)(...)
#' @param ... (in the first argument list) One or more expressions or expressions
#' to expand. These may have names, which will also be expanded.
#' @param ... (in the second argument list) Sequences. The expressions will
#' be expanded in a context that has these names bound to one value at a time
#' from each sequences (inheriting from the calling frame).
#' @return For \code{qqply}, a list of expressions. For \code{qeply}, the
#' expressions will be evaluated in the calling frame.
#' @aliases qeply
#' @seealso qq bquote
#' @author Peter Meilstrup
#' @export
#' @examples
#'
#' qqply(`.(x)` = .(y))(x=letters[1:3], y=1:3)
#'
#' qe(function(
#'     .=...( qqply(`.(..1)`=.(..2))(letters, 1:26))) {
#'   ...(qqply(.(as.name(x)) <- .(as.name(y)))(y=letters[2:26], x=letters[1:25]))
#'   e
#' })
qqply <- function(...) {
  envir <- parent.frame()
  exprs <- expressions(dots(...))
  #we curry up a function that does the actual qqply'ing
  qqply_body(exprs, envir)
}

#' @export
qeply <- function(...) {
  envir <- parent.frame()
  exprs <- expressions(dots(...))
  f <- qqply_body(exprs, envir)
  function(...) {
    lapply(f(...), eval, envir)
  }
}

qqply_body <- function(exprs, envir) {
  function(...) {
    #create a function in the target environment based on given argument names
    anames <- names(dots(...))
    anames <- anames[anames != ""]
    f <- qe(function(
        .=...(
             structure(missing_value(length(anames)),
                      names=anames)),
        ...) {
      .(unquote)(quote(.(exprs)))
    })
    environment(f) <- envir
    dots <- list(...)
    #this is fragile(actually requires "dots" in the environment)
    #todo: replace with my own mapply
    x <- .Internal(mapply(f, dots, NULL))
    unlist(x, recursive=FALSE)
   }
}
