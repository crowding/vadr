##' Given a list of names, build an environment such that evaluating
##' any expression using those names just gets you the expression
##' back.
##'
##' Technically, this defines two nested environments, the outer
##' containing functions and the inner containing names, and returns
##' the inner.
##'
##' This somewhat esoteric function mostly intended to be used by
##' \code{\link{expand.macro}}
##'
##' @note This will cause errors when the expression has missing
##' arguments. The expression might be preprocessed (somewhow?) to take missing
##' arguments out.
##'
##' @param names The names the environment should define.
##' @param parent The parent environment (defaults to the empty environment)
##' @param call.names The functions the enclosing environment should
##' define. Decaults to \code{names}, but sometimes you want these to
##' be different.
##' @return The environment constructed.
##' @author Peter Meilstrup
##' @export
##' @examples
##' en <- recapitulating.environment(c('+', '(', 'a', 'b', '*'), environment())
##'
##' evalq(a+b, en) # a+b
##' evalq(a+(b*a), en) # a+(b*a)
##' z <- 100
##' evalq(a+(b*a)*z, en) #a+(b*a)*100
##'
##' ##We can build a function that does something like substitute() like this:
##' ersatz.substitute <- function(expr, envir=parent.frame()) {
##'   parent <- as.environment(envir)
##'   en <- recapitulating.env(setdiff(all.names(expr), ls(parent)), parent)
##'   eval(expr, en)
##' }
##'
##' ersatz.substitute(quote(a+b+c), list(b=quote(q+y))) # returns a+(q+y)+c
##'
quoting.env <- function(names, parent=emptyenv(), call.names=names) {
  #There probably needs to be special handling
  #for function() to get the elements of the pairlist evaluated, too.
  callenv <- new.env(parent=parent)
  for (n in as.character(call.names)) {
    f <- eval(substitute(
                function(...) as.call(c(quote(x), list_with_missing(...))),
                list(x=as.name(n))))
    assign(n, f, envir=callenv)
  }
  if ("..." %in% names) {
    capture.dots <- function(...) environment()
    nameenv <- eval(call("capture.dots", quote(quote(...))))
    parent.env(nameenv) <- callenv
  } else {
    nameenv <- new.env(parent=callenv)
  }
  for (n in names) {
    if (n != "...") {
      assign(n, as.name(n), envir=nameenv)
    }
  }
  nameenv
}

##' Construct a list, but allow missing arguments.
##'
##' This is particularly useful for supporting array-indexing
##' functions, that work like built-in \code{\link{`[`}}; it allows
##' you to tell when arguments are missing, but also evaluate
##' any numnber of non-missing arguments in their correct environments.
##' choking on missing arguments. That is, it lets you implement a
##' \code{`[`} accessor that currectly supports usage like
##' \code{function(a,b)(array[a*b, ])(4,2*e)}. It is also used in
##' \code{\link{expandmacro()}} and for other computing-on-the-language purposes.
##'
##' @param ... Objects, possibly named, possibly missing.
##' @param `*default*` What to fill. Should be an _expression_ that will be
##' _evaluated_ to fill in missing values. Default is an expression that
##' evaluates to the empty symbol.
##' @param `*envir*` The environment in which *default* will be evaluated.
##' @return A list, with any non-missing arguments evaluated, any
##' other arguments substituted with the default expression and
##' evaluated.
##' @export
##' @author Peter Meilstrup
##' @examples
##' #unlike alist, arguments are evaluated in context.
##' y <- 4
##' alist(2*y, , x=12+24, d=)
##' list.with.missing(2*y, , x=12+24, d=)
##'
##' #unlike with `list`, missing arguments are detected and handled.
##' \dontrun{
##' list(2*y, , x=12+24, d=) #produces an error.
##' }
list_with_missing <- function(...,
                              `*default*`=quote(quote(expr= )),
                              `*envir*`=parent.frame()) {
  #We need to check each element of "..." for missingness.
  #
  #'...' is functionally a list of promises, but it's an opaque object
  #of class '...' that is a given special treatment bu the
  #interpreter. We would like to inspect each elment of '...' to see
  #if it is missing.
  #
  #But the only thing you can do with a '...' is pass it to a call. So
  #we build a function that takes arguments whose names match those of
  #'...' (inspected wtih substitute()) and pass in; then we can check
  #each argument for missingness, eval the ones that aren't missing
  #and build a list.

  uneval.args <- eval(substitute(alist(...)))
  lexically.missing <- sapply(uneval.args,
                              function(x) is.name(x) && as.character(x) == "")
  orig.argnames <- names(uneval.args)
  if (is.null(orig.argnames)) orig.argnames <- rep("", length(uneval.args))
  empty.names <- orig.argnames == ""
  argnames <- orig.argnames
  argnames[empty.names] <- make_unique_names(rep("...anon", sum(empty.names)),
                                             argnames[!empty.names])
  arglist <- as.pairlist(
               structure(rep(list(quote(expr= )),
                             length(uneval.args)),
                         names=argnames))
  body <- mapply(FUN=`if`, !lexically.missing, lapply(argnames, as.name),
                 moreArgs=list(`*default*`))
  names(body) <- orig.argnames
  body <- as.call(c(base:::list, body))
  f <- eval(call('function', arglist, body), `*envir*`)
  f(...)
}

##' Modify some character strings unique with respect with an
##' existing set of (unmodified) character strings.
##'
##' A convenience extension of \code{\link{make.unique}}.
##'
##' @param new Initial values for the new nmes
##' @param context Existing names to avoid collisions with.
##' @return the values of \code{new} in order modified to avoid collisions.
##' @author Peter
make_unique_names <- function(new, context, sep=".") {
  uniq <- make.unique(c(context, make.names(new)))
  uniq[(length(context)+1):(length(context)+length(new))]
}

`%||%` <- function(a, b) if(is.null(a)) b else a

#' Turn an expression-substituting function into a
#' nonstandard-evaluating function.
#'
#' This just places a wrapper around the function so that you do not
#' have to remember how to substitute, and you are not tempted to mix
#' nonstandard with standard evaluation(*).
#'
#' @param fn A function which takes some arguments and returns a trane
#' @return the wrapper function. It will have an identical argument
#' list to the wrapped function. It will transform all arguments into
#' expressions, pass the expressions to the wrapped function, then
#' evaluate the result it gets back.
#'
#' (*) The author believes that there is one way to properly do
#' nonstandard evaluation in R, and that is to quote ALL of your
#' arguments and perform purely lexical operations on them, evaluating
#' the result in a data frame. In other words, to behave as a
#' macro. Functions which evaluate some argument normally and others
#' abnormally (e.g. \link{\link{transform}} cause headaches and other
#' such faux-dynamic-scope operations.
#'
#' @author Peter Meilstrup
macro <- function(fn, cache=TRUE) {
  #here, we want a caching mechanism?
  #
  f <- function (...) {
    fr <- parent.frame()
    args <- eval(substitute(alist(...)))
    args <- lapply(args, enquote)
    result <- do.call(fn, args)
    eval(result, fr)
  }
#  attr(f, "srcref") <-
#    paste("macro(", paste(attr(fn, "srcref") %||% deparse(fn), collapse="\n"), ")")

  class(f) <- c("macro", class(f))
  attr(f, "orig") <- fn
  f
}

library(stringr)
template <- function(expr, .envir=parent.frame()) {
  unquote <- function(e) {
    if (length(e) <= 1L) {
      if (is.name(e)) {
        ch = unquote.char(as.character(e))
        if (ch == "") {
          quote(expr=)
        } else {
          as.name(ch)
        }
      } else {
        e
      }
    } else if (e[[1L]] == as.name(".")) {
      eval(e[[2L]], .envir)
    } else if (is.pairlist(e)) {
      as.pairlist(unquote.list(e))
    } else if (e[[1L]] == as.name("...")) {
      x <- eval(e[[2L]], .envir)
      attr(x, "...") <- TRUE #out of band signaling
      x
    } else {
      as.call(unquote.list(e))
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
      as.character(eval(parse(text=match[2]), .envir))
    } else {
      ch
    }
  }

  unquote(substitute(expr))
}
