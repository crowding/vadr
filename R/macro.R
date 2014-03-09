#' @include cache.R
NULL

##' Given a list of names, build an environment such that evaluating
##' any expression using those names just gets you the expression
##' back.
##'
##' Technically, this defines two nested environments, the outer
##' containing functions and the inner containing names, and returns
##' the inner.
##'
##' This somewhat esoteric function mostly intended to be used by
##' \code{\link{expand_macro}}
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
##' en <- quoting.env(c('+', '(', 'a', 'b', '*'), environment())
##'
##' evalq(a+b, en) # a+b
##' evalq(a+(b*a), en) # a+(b*a)
##' z <- 100
##' evalq(a+(b*a)*z, en) #a+(b*a)*100
##'
##' ##We can build a function that does something like substitute() like this:
##' ersatz.substitute <- function(expr, envir=arg_env(expr)) {
##'   parent <- as.environment(envir)
##'   en <- quoting.env(setdiff(all.names(expr), ls(parent)), parent)
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
                function(...) as.call(c(quote(x), list_missing(...))),
                list(x=as.name(n))))
    assign(n, f, envir=callenv)
  }
  nameenv <- new.env(parent=callenv)
  for (n in names) {
    if (n == "...") {
      assign("...", as.dots.literal(quote(...)), envir=nameenv)
    } else {
      assign(n, as.name(n), envir=nameenv)
    }
  }
  nameenv
}

##' Modify some character strings unique with respect with an
##' existing set of (unmodified) character strings.
##'
##' A convenience extension of \code{\link{make.unique}}.
##'
##' @param new Initial values for the new names
##' @param context Existing names to avoid collisions with.
##' @return the values of \code{new} in order modified to avoid collisions.
##' @author Peter Meilstrup
make_unique_names <- function(new, context, sep=".") {
  uniq <- make.unique(c(context, make.names(new)))
  uniq[(length(context)+1):(length(context)+length(new))]
}

#' Turn an expression-substituting function into a
#' nonstandard-evaluating function.
#'
#' This just places a wrapper around the function that un-quotes all arguments
#' and evaluates the result.
#'
#' @param fn A function which takes some arguments and returns a trane
#' @param cache Whether to store already-compiled macros for faster
#' evaluation. Defaults to TRUE. This requires that the macro function
#' be a pure function not referring to any outside state.
#' @param JIT Whether to compile expressions (using the "compiler"
#' package) before executing. Defaults to TRUE.
#' @return the wrapper function. It will have an identical argument
#' list to the wrapped function. It will transform all arguments into
#' expressions, pass the expressions to the wrapped function, then
#' evaluate the result it gets back.
#'
#' The advantage of macros versus usual nonstandard evaluation using
#' \code{\link{substitute}}, \code{link{eval}} and friends
#' is that it encourages separating "computing on
#' the language" from "computing on the data." Because code is usually
#' static while data is variable, the language transformations only need
#' to happen once per each call site.
#' Thus the expansions of macros can be cached, enabling complicated code
#' transformations without performance penalties.
#'
#' @author Peter Meilstrup
#' @seealso qq
#' @import compiler
#' @export
macro <- function(fn, cache=TRUE, JIT=cache) {
  orig <- fn
  if(cache) {
    fn <- macro_cache(fn, JIT)
    f <- function(...) {
      fr <- dots_environments(...)[[1]]
      eval(fn(...), fr)
    }
  } else {
    f <- function(...) {
      fr <- dots_environments(...)[[1]]
      args <- dots_expressions(...)
      expr <- do.call(fn, args, quote=TRUE)
      eval(expr, fr)
    }
  }

  class(f) <- c("macro", class(fn))
  attr(f, "orig") <- orig
  # set the source to look reasonable?
  #  attr(f, "srcref") <-
  #    paste("macro(", paste(attr(fn, "srcref") %||% deparse(fn), collapse="\n"), ")")
  f
}

#' Expand any macros in the quoted expression.
#'
#' This searches for macro functions referred to in the quoted
#' expression and substitutes their equivalent expansions.  Not
#' guaranteed to give exact results.
#'
#' @aliases expand_macros_q
#' @param expr An expression. For \code{expand_macros_q}, this
#' argument is quoted. For \code{expand_macros}, itis a language object.
#' @param macros a named list of macros. By default searches for all macros.
#' @return The expansion of the given expression.
#' @author Peter Meilstrup
#'
#' This is intended for interactive/debugging use; in general, its
#' results are not correct. For example, expressions appearing inside
#' of \code{link{quote}()} will get expanded anyway.
#' @export
expand_macros <- function(expr,
                          macros=NULL,
                          where=arg_env(expr, environment()),
                          recursive=FALSE) {
  force(where)
  if (is.null(macros)) {
    macros <- find_macros(all.names(expr), where)
  }
  present_macros <- macros[names(macros) %in% all.names(expr)]
  #recast macros to return quoted results instead of eval'ing them
  redone_macros <- lapply(present_macros, function(m) {
    inner <- attr(m, "orig")
    function(...) {
      quoted.args <- eval(substitute(alist(...)))
      do.call("inner", quoted.args, quote=TRUE)
    }
  })
  envir <- as.environment(redone_macros)
  parent.env(envir) <- quoting.env(all.names(expr))
  expanded <- eval(expr, envir)
  if (recursive && any(names(macros) %in% all.names(expr))) {
    expanded <- expand_macros(expanded, macros, where, recursive)
  }
  expanded
}

#' @export
expand_macros_q <- function(expr,
                            macros=find_macros(all.names(expr), where),
                            where=arg_env(expr, environment()),
                            recursive=FALSE) {
  force(where)
  expr <- substitute(expr)
  expand_macros(expr, macros, where, recursive)
}

#' Attempts to find all macros on the search path.
#'
#' @title List all macros.
#' @return a list of macro functions, the list having names according
#' to the names they are bound to. If given, this overrides both
#' include.enclos and include.search.
#'
#' @author Peter Meilstrup
#' @export
#' @param what a list of names to try. If not specified, searches all
#' attached namespaces.
#' @param where A frame to search.
find_macros <- function(what, where=arg_env(what, environment())) {
  force(where)
  if (is.null(what)) {
    what <- apropos(".*", where=FALSE, mode="function")
  }
  functions <- sapply(what, mget, envir=where, mode="function",
                      inherits=TRUE, ifnotfound=list(NULL))
  #search for every function with class "macro"
  is.macro <- vapply(functions, function(x) "macro" %in% class(x), FALSE)
  structure(functions[is.macro], names=what[is.macro])
}

#' Quote all arguments, like \code{\link{alist}}. But when bare words
#' are given as arguments, interpret them as the argument name, rather
#' than the argument value. Return a pairlist.
#' This emulates the syntax used to specify function arguments and defaults.
#' @param ... the arguments to quote.
#' @return a pairlist, with quoted arguments, and barewords inprepreted as names
#' @examples
#' substitute(`function`(args, body), list(args=quote_args(x, y=1), body=quote(x+y)))
#' @export
quote_args <- function(...) {
  x <- substitute(list(...))[-1]
  as.pairlist(
    mapply(x, USE.NAMES=FALSE,
           if (is.null(names(x))) rep("", length(x)) else names(x),
           FUN = function(x, n) {
             if (n == "") {
               if (is.name(x) & !identical(x, quote(expr=))) {
                 structure(list(quote(expr=)), names=as.character(x))
               } else {
                 stop("'", deparse(x, nlines=1), "' doesn't look like a name")
               }
             } else {
               structure(list(x), names=n)
             }
           }))
}

.onLoad_macro <- function(libname, pkgname) {
}
