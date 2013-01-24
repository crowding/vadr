#' Show information about a dot-dot-dot object.
#'
#' This unpacks the contents of a dot-dot-dot (or \code{<...>})
#' object, returning the results in a data frame. A \code{<...>}
#' object is a pairlist of promises, usually bound to the special name
#' \code{"..."} and, when bound to that name, given special
#' dispensation by the R interpreter when appearing in the argument
#' list of a call. Dots objects are normally opaque to R code, but you
#' can obtain a \code{<...>} inside a function by using
#' \code{get("...")}.
#'
#' @param ... Any number of arguments. Usually, you will pass in the
#' ... from the body of a function,
#' e.g. \code{dots_info(...)}. Techinically this creates a copy of the
#' dots list, but it should be identical.
#'
#' @return A data frame, with one row for each element of \code{...},
#' and columns:
#' \describe{
#' \item{"name"}{The name of each argument, or "" if present.}
#' \item{"envir"}{The enviroment the promise came from.}
#' \item{"expr"}{The expression attached to the promise.}
#' \item{"eval"}{TRUE if the promise has been evaluated.}
#' \item{"value"}{The value attached to the promise, or NULL if not evaluated.}
#' \item{"seen"}{TRUE if the promise has already been "seen" during promise
#' evaluation. This flag is set by R during evaluation to detect cyclic
#' references.}
#' }
#' @note The "envir" column is a list of lists, because print.data.frame
#' still can't handle a column that contains environments (even using
#' \link{AsIs}).
#' @seealso dots_names dots_missing
#' @author Peter Meilstrup
#' @export
dots_info <- function(...) .Call("dots_info", get("..."))

#' Functions to work with dot-dot-dot (\dots) arguments.
#'
#' These are useful for writing functions that accept any number of
#' arguments but some may be missing. For example, arrays in R can
#' have any number of dimensions, indexed by the \code{\link{"["}}
#' function, where a missing argument means to take all indexes on that
#' dimension. However there is not a good way to replicate
#' \code{\link{"["}}'s behavior in base R; using \code{list(...)} to
#' collect all positional arguments will throw errors on missing
#' arguments. These functions help extract names and values of
#' positional arguments without triggering missing-value errors.
#'
#' Parallel functions exist as methods of the \{\dots} class of
#' objects returned by \code{\link{dots}} that allows you to manipulate
#' ... argument lists as explicit objects.
#' @param ... Any arguments. Usually you will pass \code{...} from the
#' body of a function.
#' @return \itemize{
#' \item For \code{\link{dots_names}}, the names of all arguments. Names are
#' also attached to results from the other functions listed here.
#' \item For \code{\link{dots_missing}}, a logical vector with TRUE for each
#' missing argument.
#' \item For \code{\link{list_missing}}, a named list of all evaluated
#' arguments, where any missing arguments are set to NULL.
#' \item For \code{\link{list_quote}}, a list of quoted arguments. This extracts
#' the original expressions from a dotlist without forcing evaluation.
#' }
#' @author Peter Meilstrup
#' @aliases dots_missing list_missing list_quote alist
#' @seealso is.missing dots curr
#' @useDynLib ptools
#' @export
dots_names <- function(...) .Call("dots_names", get("..."))

#' @export
dots_missing <- function(...) {
  result = logical(nargs())
  sym = paste("..", seq_len(nargs()), sep="")
  for (i in seq_len(nargs()))
    result[[i]] <- do.call("missing", list(as.name(sym[[i]])))
  structure(result, names=dots_names(...))
}

#' @export
list_missing <- function(...) {
  out <- logical(nargs())
  sym = paste("..", seq_len(nargs()), sep="")
  for (i in seq_len(nargs())) {
    x <- as.name(sym[[i]])
    out[i] <- eval(substitute(missing(x)))
  }
  n <- dots_names(...)
  if (!is.null(n)) names(out) <- n
  out
}

#' @export
list_quote <- function(...) eval(substitute(alist(...)))

#' Capture the list of "dot-dot-dot" arguments as an object.
#'
#' \code{dots} and methods of class \code{...} provide a more
#' convenient interface to capturing lists of unevaluated arguments
#' and applying them to functions.
#'
#' @param ... Any number of arguments.
#' @return A dots object. This is currently just the raw DOTSXP with
#' the object bit set and the class set to "..." so that method dispatch works.
#' @author Peter Meilstrup
#' @seealso "%<<%" "%>>%" "%()%" "[...." "[[....", "names...."
#' @examples
#' reverse.list <- function(...) {
#'  d <- dots(...)
#'  list %()% rev(d)
#' }
#' reverse.list("a", b="bee", c="see")
#'
#' named.list <- function(...) {
#'  d <- dots(...)
#'  list %()% d[names(d) != ""]
#'  }
#' named.list(a=1, b=2*2, stop("this is not evaluated"))
#' @export
dots <- function(...) structure(get("..."), class="...")

#' @S3method "print" "..."
`print....` <- function(x) cat("<...[", length(x), "]>\n")

#' Partially and fully apply arguments to functions.
#'
#' These operators help in passing arbitrary lists of arguments to
#' functions, with a more convenient interface than
#' \code{\link{do.call}}. The partial application operator allows
#' saving some arguments with a reference to a function so the
#' resulting function can be passed elsewhere.
#'
#' These objects have methods for objects of class \code{...} produced
#' by \code{\link{dots}}, so that you may partially apply argument
#' lists without arguments as yet unevaluated.
#' @param x a vector, optionally with names, or an object of class
#' \code{...} as produced by \code{\link{dots}}.
#' @param f a function, to be called, or to to have arguments attached to.
#' @aliases %()% %<<% %>>% %__% curr curl
#' @name grapes-open-paren-close-paren-grapes
#' @return \itemize{
#' \item For \code{\%()\%}, the result of calling the function with the
#' arguments provided. THis has slightly different semantics from
#' \code{\link{do.call}(f, as.list(x), quote=TRUE)}, in that arguments
#' are passed through already-evaluated promises, rather than wrapped
#' in "quote" and passed in new promises. This makes a difference if
#' \code{f} performs nonstandard evaluation.
#' \item For \code{\%<<\%} and \code{\%>>\%}, a new function with the
#' arguments partially applied. For \code{arglist \%>>\% f}, the
#' arguments will be placed in the argument list before any further
#' arguments; for \code{f \%<<\% arglist} the arguments will be placed
#' afterwards.
#' \item \code{curr} and \code{curl} are standalone functions that partially
#' apply arguments to functions; \code{curr(f, a=1, b=2)} is equivalent to
#' \code{f %<<% dots(a=1, b=2)}, and
#' \code{curl} is the "left curry" corresponding to \code{%>>%}
#' \item For \code{\%__\%}, the two operands pasted together. The result
#' will be a list, or a \code{dots} object if any of the operands are
#' \code{dots} objects.
#' }
#' @note "Curry" is a slight misnomer for partial function application.
#' @author Peter Meilstrup
#' @export
`%()%` <- function(f, arglist) UseMethod("%()%", arglist)

#' @S3method "%()%" "..."
`%()%....` <- function(f, arglist, ...) {
  # this method elegant but doesn't work on some
  # nonstandard-eval functions (e.g. alist $()$ dots(...) just returns
  # quote(...))?
  assign("...", arglist)
  f(...)
}

#' @S3method "%()%" default
#' @useDynLib ptools
`%()%.default`  <- function(f, arglist, .envir=parent.frame()) {
#  do.call(f, as.list(arglist), quote=TRUE)
  seq <- do.call(dots, as.list(arglist), FALSE, .envir)
  .Call("call_function_from_dots", f, seq, .envir, TRUE)
}

#' @export
`%<<%` <- function(f, x) UseMethod("%<<%", x)

#' @export
`%>>%` <- function(x, f) UseMethod("%>>%", x)

#' @S3method "%<<%" "..."
`%<<%....` <- function(f, x) {
  if (is.null(x)) return(f)
  dotslist <- list(NULL, x)
  function(...) {
    if (missing(...)) {
      assign("...", x)
      f(...)
    } else {
      dotslist[1] <<- list(get("..."))
      rm("...")
      count <- 0
      makeActiveBinding("...", function(x) {
        count <<- count+1
        dotslist[[count]]
      }, environment())
      #a DOTSXP is only expanded into a function's arguments when the
      #evaluator encounters the special symbol "...". We use an active
      #binding to get R to expand two different DOTSXPS from the same
      #symbol.
      f(..., ...)
    }
  }
}

#' @S3method "%>>%" "..."
`%>>%....` <- function(x, f) {
  if (is.null(x)) return(f)
  dotslist <- list(x, NULL)
  function(...) {
    if (missing(...)) {
      assign("...", x)
      f(...)
    } else {
      dotslist[2] <<- list(get("..."))
      rm("...")
      count <- 0
      makeActiveBinding("...", function(x) {
        count <<- count+1
        dotslist[[count]]
      }, environment())
      f(..., ...)
    }
  }
}

#also a standalone right-curry and left-curry; does not use S3-dispacthed dots objects

#' @export
curr <- function(f, ...) {
  if(missing(...)) {
    f
  } else {
    stored_dots <- list(NULL, get("..."))
    function(...) {
      if (missing(...)) {
        assign("...", stored_dots[[2]])
        f(...)
      } else {
        stored_dots[1] <- list(get("..."))
        rm("...")
        selector <- 0
        makeActiveBinding("...",
                          function() stored_dots[[selector <<- selector+1]],
                          environment())
        # this depends in a deep way on how ... are represented and evaluated
        # see eval.c
        f(..., ...)
      }
    }
  }
}

#' @export
curl <- function(f, ...) {
  if(missing(...)) {
    f
  } else {
    stored_dots <- list(get("..."), NULL)
    function(...) {
      if (missing(...)) {
        assign("...", stored_dots[[1]])
        f(...)
      } else {
        stored_dots[2] <- list(get("..."))
        rm("...")
        selector <- 0
        makeActiveBinding("...",
                          function() stored_dots[[selector <<- selector+1]],
                          environment())
        # this depends in a deep way on how ... are represented and evaluated
        # see eval.c, "R Internals"
        f(..., ...)
      }
    }
  }
}

#Curry methods for plain values.
#Here we reuse %()% since we had a time getting it to follow the desired
#semantics.

#' @S3method "%<<%" default
`%<<%.default` <- function(f, x) function(...)
  if(missing(...)) f %()% x else dots(...) %>>% f %()% x

#' @S3method "%>>%" default
`%>>%.default` <- function(x, f) function(...)
  if(missing(...)) f %()% x else f %<<% dots(...) %()% x

cdots <- function(x, y) UseMethod("cdots", x)

#' @export
`%__%` <- cdots

#' @S3method "cdots" "..."
`cdots....` <- function(x, y) UseMethod("cdots....", y)

cdots........ <- function(x, y, ...) {
  dotslists <- list(x, y)
  count <- 0
  rm("...")
  makeActiveBinding("...", function(x) {
    count <<- count+1
    dotslists[[count]]
  }, environment())
  dots(..., ...)
}

cdots.....default <- function(x, y) stop()

#' @S3method "cdots" default
cdots.default <- function(f, x) UseMethod("cdots.default")

#' @S3method cdots.default "..."
cdots.default.... <- function (f,x) stop()

#' @S3method cdots.default default
cdots.default.default <- c

#' Convert a list of expressions into a \code{\dots} object (a list of
#' promises.)
#'
#' @param x a vector of expressions.
#' @param .envir The environment within which each promise will be evaluated.
#' @return An object of class \code{\dots}.
#' @seealso dots "%<<%" "%>>%" "%()%" "[...." "[[....", "names...."
#' @author Peter Meilstrup
as.dots <- function(x, .envir=parent.frame()) UseMethod("as.dots")

#' @S3method as.dots "..."
as.dots.... <- function(x, ...) x

#' @S3method as.dots default
as.dots.default <- function(x, .envir=parent.frame())
  do.call(dots, as.list(x), FALSE, .envir)

#' Check if list members are equal to the "missing value."
#'
#' For \code{\dots} objects as made by \code{\link{dots}}, performs
#' this check without forcing evaluation.
#' @param x An object
#' @return A vector of boolean values.
#' @author Peter Meilstrup
#' @seealso dots dots_missing missing.value
#' @export
is.missing <- function(x) if (missing(x)) TRUE else UseMethod("is.missing")

#' @S3method is.missing "..."
is.missing.... <- function(x, ...) {
  out <- logical(length(x))
  if (nargs() > 2) stop("more than one argument supplied to is.missing")
  assign("...", x)
  sym = paste("..", seq_len(length(x)), sep="")
  for (i in seq_len(length(x))) {
    n <- as.name(sym[[i]])
    out[i] <- eval(substitute(missing(n)))
  }
  n <- dots_names(...)
  if (!is.null(n)) names(out) <- n
  out
}


#' @S3method is.missing default
is.missing.default <- function(f) {
  if (is.list(f))
    vapply(f, identical, FALSE, quote(expr=))
  else
    rep(FALSE, length(f))
}
