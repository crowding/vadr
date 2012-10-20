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
##' \code{\link{expand_macros()}} and for other computing-on-the-language purposes.
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
##' @author Peter Meilstrup
make_unique_names <- function(new, context, sep=".") {
  uniq <- make.unique(c(context, make.names(new)))
  uniq[(length(context)+1):(length(context)+length(new))]
}

#' Evaluate the first argument; if null, evaluate and return the
#' second argument.
#'
#' @param a The first argument to evaluate.
#' @param b The second argument to evaluate. Only evaluated if
#' \code{a} is null.
#' @return the value of \code{a} if not null, else \code{b}
#' @author Peter Meilstrup
`%||%` <- function(a, b) if(is.null(a)) b else a

#' Turn an expression-substituting function into a
#' nonstandard-evaluating function.
#'
#' This just places a wrapper around the function so that you do not
#' have to remember how to use substitute, and you are not tempted to
#' mix nonstandard with standard evaluation(*).
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
#' the result in the caller. In other words, to behave as a
#' macro. Functions which evaluate different arguments in different
#' scopes (e.g. \link{\link{transform}} cause problems; consider
#' writing these as functors, or using formula objects or
#' \code{\link[plyr]{.}()} to capture environments explicitly.
#'
#' As a future enhancement, we will cache the values of macro
#' substitutions.
#'
#' @author Peter Meilstrup
#' @seealso template
#' @export

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
# set the source to look reasonable?
#  attr(f, "srcref") <-
#    paste("macro(", paste(attr(fn, "srcref") %||% deparse(fn), collapse="\n"), ")")

  class(f) <- c("macro", class(f))
  attr(f, "orig") <- fn
  f
}

#' Perform template substitutions on a quoted R expressions.
#'
#' This is an extended version of the \code{\link{backquote}}
#' utility.  'template' quotes its first argument, then scans for
#' terms erapped in \code{.()}, \code{...()}, or names that match
#' \code{`.()`} The wrapped expressions or names are evaluated in the
#' given environment.  Expressions wrapped in \code{...()} will be
#' interpolated into the argument list in which they occur. Names
#' wrapped in `.()` will be substituted and coerced to name.
#'
#' The contents of a name matching `.(  )` will be parsed and
#'
#' This can used recursively to build interesting code transformations.
#'
#' @title
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
#' function(...(list(x=1, y=missing.value()))) x+y
#'
#' # Building a function with an arbitrary list of arguments:
#' argnames <- letters[1:4]
#' template(function(.=...(setNames(missing.value(length(argnames)), argnames))) {
#'   list(...(lapply(argnames, as.name)))
#' })
#' #function(a, b, c, d) list(a, b, c, d)
#'
#' #' The poor .() function is overloaded. You can escape it thus:
#' dfname <- "baseball"
#' template(ddply(.(as.name(dfname)), .(quote(.(id, team))), identity))
#' #ddply(baseball, .(id.team), identity)
template <- function(expr, .envir=parent.frame()) {
  require(stringr)

  unquote <- function(e) {
    if (is.pairlist(e)) {
      as.pairlist(unquote.list(e))
    } else if (length(e) <= 1L) {
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
    } else if (e[[1L]] == as.name("...")) {
      x <- eval(e[[2L]], .envir)
      attr(x, "...") <- TRUE #flag to be picked up by unquote.list
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
      as.character(eval(parse(text = match[2]), .envir))
    } else {
      ch
    }
  }

  unquote(substitute(expr))
}


#' Return an empty symbol.
#'
#' The empty symbol is used to represent missing values in the R
#' language; for instance in the value slot of a function argument
#' when there is no default; and in the expression slot of a promise
#' when a missing argument is given.
#'
#' @param n Optional; a number. If provided, will return a list of
#' missing values with this many elements.
#' @return A symbol with empty name, or a list of such.
#' @examples
#' # These statements are equivalent:
#' quote(function(x, y=1) x+y)
#' call("function", as.pairlist(x=missing.value(), y=1), quote(x+y))
#'
#' # These statements are also equivalent:
#' quote(df[,1])
#' substitute(df[row,col], list(row = missing.value(), col = 1))
#'
#' # These statements are equivalent
#' quote(function(a, b, c, d, e) print("hello"))
#' call("function", as.pairlist(setNames(missing.value(5), letters[1:5])), quote(print("hello")))
#' @export
missing.value <- function(n) {
  if (missing(n)) {
    quote(expr=)
  } else {
    rep(list(quote(expr=)), n)
  }
}

# below is speculative and should probably rather be saved into a branch.

0 && {
#' Expand any macros in the quoted expression.
#'
#' This searches for macro functions referred to in the quoted
#' expression and substitutes their equivalent expansions.  Not
#' guaranteed to give exact results.
#'
#' @param expr An expression. This is quoted and not evaluated.
#' @param .envir An environment in which to look for macro functions,
#' or a named list of macro functions. Defaults to the calling
#' environment.
#' @return The expansion of the given expression.
#' @author Peter Meilstrup
#'
#' This is intended for interactive/debugging use; in general, its
#' results are not correct. For example, expressions appearing inside
#' of \code{link{quote}()} will get expanded anyway.
expand_macros_q <- function(expr, .envir=parent.frame()) {
  eval(substitute(expand_macros_q(expr, .envir)))
  "not written"
}

#' Given a quoted expression and a list of macro functions, perform
#' one level of macro expansion.
#'
#' @param expr A language object.
#' @param macros A named list of macro functions. These are expected
#' to have an "orig" attribute (as functions built using \link{macro}
#' do.)
#' @return The expression with macros expanded.
#' @author Peter Meilstrup
expand_macros <- function(expr, macros) {
  "not written"
}

}
