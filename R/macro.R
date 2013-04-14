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
##' @param new Initial values for the new nmes
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
#' This just places a wrapper around the function so that you do not
#' have to remember how to use substitute, and you are not tempted to
#' mix nonstandard with standard evaluation(*).
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
#' (*) The author believes that there is one way to properly do
#' nonstandard evaluation in R, and that is to quote ALL of your
#' arguments and perform purely lexical operations on them, evaluating
#' the result in the caller. In other words, to behave as a
#' macro. Functions which evaluate different arguments in different
#' scopes (e.g. \code{\link{transform}} cause problems; consider
#' writing these as functors, or using formula objects or
#' \code{\link[plyr]{.}()} to capture environments explicitly.
#'
#' @author Peter Meilstrup
#' @seealso template
#' @import compiler
#' @export
macro <- function(fn, cache=TRUE, JIT=TRUE) {
  if(cache) {
    #Cache all expansions of the macro by pointer-equality on the
    #unevaluated args.
    expansionCache <- new.env(hash=TRUE, parent=emptyenv())
    #the cache is pointer-based and becomes meaningless on
    #serialization, so we really want to make it not serialize.
    #But how?
  }

  f <- function (...) {
    fr <- parent.frame()
    args <- eval(substitute(alist(...)))

    if (cache) {
      digest <- expressions_and_pointers(...)
      key <- paste(names(digest), collapse=".")
      if (exists(key, envir=expansionCache)) {
        result <- expansionCache[[key]][[1]]
      } else {
        result <- do.call(fn, args, quote=TRUE)
        if (JIT) result <- compile(result, fr)
        #Hold on to the list of expression objects to keep them from
        #getting stale; I probably want weak references though.
        expansionCache[[key]] <- list(result, digest)
      }
    } else {
      result <- do.call(fn, args, quote=TRUE)
      #jitting is no help if you're not caching
      #if (JIT) result <- compile(result, fr)
    }
    eval(result, fr)
  }

  class(f) <- c("macro", class(f))
  attr(f, "orig") <- fn
  # set the source to look reasonable?
  #  attr(f, "srcref") <-
  #    paste("macro(", paste(attr(fn, "srcref") %||% deparse(fn), collapse="\n"), ")")
  f
}

#' Perform template substitutions on a quoted R expressions.
#'
#' This is an extended version of the \code{\link{backquote}}
#' utility.  'template' quotes its first argument, then scans for
#' terms wrapped in \code{.()}, \code{...()}, or names that match
#' \code{`.()`} The wrapped expressions or names are evaluated in the
#' given environment.  Expressions wrapped in \code{...()} will be
#' interpolated into the argument list in which they occur. Names
#' wrapped in `.()` will be substituted and coerced to name.
#'
#' Invocations of template() can be nested within the \code{.()}'s
#' section and they should work as promised.
#'
#' @param expr A language object.
#' @param .envir An environment to evaluate the backquoted expressions in.
#' @return A language object.
#' @author Peter Meilstrup
#' @seealso macro bquote substitute recpitulating.env
#' @examples
#'
#' #Basic substitution
#' default <- 1
#' template( function(x, y = .(default)) x+y )
#' #function(x, y = 1) x + y
#'
#' # interpolating substitution:
#' paste.before <- alist("hello", "cool")
#' paste.after <- alist("!", "Now is", date())
#' template(cat(...(paste.before), "world", ...(paste.after), '\n'))
#' #cat("hello", "cool", "world", "!", "Now is", date(), "\n")
#'
#' # Name substitution:
#' element_to_access <- "x"
#' template(function(data) data$`.(element_to_access)`)
#' #function(data) data$x
#'
#' argument.name <- "x"
#' template(
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
#' template(function(.=...(setNames(missing_value(length(argnames)), argnames))) {
#'   list(...(lapply(argnames, as.name)))
#' })
#' #function(a, b, c, d) list(a, b, c, d)
#'
#' #' The poor .() function is overloaded. You can escape it thus:
#' dfname <- "baseball"
#' template(ddply(.(as.name(dfname)), .(quote(.(id, team))), identity))
#' #ddply(baseball, .(id.team), identity)
#' @import stringr
#' @export
template <- function(expr, .envir=parent.frame()) {
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
      as.character(eval(parse(text = match[2]), .envir))
    } else {
      ch
    }
  }

  unquote(substitute(expr))
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
                          where=parent.frame(), recursive=FALSE) {
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
                            where=parent.frame(), recursive=FALSE) {
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
find_macros <- function(what, where=parent.frame()) {
  if (is.null(what)) {
    what <- apropos(".*", where=FALSE, mode="function")
  }
  functions <- lapply(what, getFunction, where=where, mustFind=FALSE)
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
               if (is.name(x)) {
                 structure(list(quote(expr=)), names=as.character(x))
               } else {
                 stop(deparse(x, nlines=1), " doesn't look like a name")
               }
             } else {
               structure(list(x), names=n)
             }
           }))
}
