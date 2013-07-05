#' Show information about a dot-dot-dot object.
#'
#' This unpacks the contents of a dot-dot-dot (or \code{<...>})
#' object, returning the results in a data frame. A \code{<...>}
#' object is a pairlist of promises, usually bound to the special name
#' \code{"..."} and, when bound to that name, given special
#' dispensation by the R interpreter when appearing in the argument
#' list of a call. Dots objects are normally opaque to R code, and
#' usually don't explicitly appear in user code, but you can obtain a
#' \code{\dots} inside of R by using \code{get("...")}.
#'
#' @param ... Any number of arguments. Usually, you will pass in the
#' ... from the body of a function,
#' e.g. \code{dots_unpack(...)}. Technically this creates a copy of the
#' dots list, but it should be identical.
#'
#' @return A data frame, with one row for each element of
#' \code{\dots}, and columns: \describe{ \item{"name"}{The name of
#' each argument, or "" if present.}  \item{"envir"}{The enviroment
#' the promise came from.}  \item{"expr"}{The expression attached to
#' the promise. If the promise has been evaluated, this will be NULL.}
#' \item{"value"}{The value attached to the promise. If the promise
#' has not been evaluated, this will be NULL. (in reality is it
#' usually the "missing value," but it would cause too much
#' strangeness to return missing values from a function.}}
#' @note There are some problems with R printing data frames
#' containing lists of language objects (and more problems when
#' working with "missing value" objects.) Therefore this sets the
#' class on the columns to one with a special as.character method.
#' @seealso dots_names dots_missing dots_expressions dots
#' @aliases unpack
#' @author Peter Meilstrup
#' @useDynLib ptools _dots_unpack
#' @export
dots_unpack <- function(...) {
  du <- .Call(`_dots_unpack`, get("..."))
  data.frame(du, row.names=make.names(du$name, unique=TRUE), check.names=TRUE)
}

#' @export
unpack <- function(x) UseMethod("unpack")

#' @S3method unpack ...
#' @useDynLib ptools _dots_unpack
unpack.... <- function (x) {
  du <- .Call(`_dots_unpack`, x)
  data.frame(du, row.names=make.names(du$name, unique=TRUE), check.names=TRUE)
}

#' @export
expressions <- function(...) UseMethod("expressions")

#' Extract the expressions from a dots object.
#'
#' @param x A dots object (see \code{\link{dots}}
#' @return A named list of expressions.
#' @seealso dots unpack
#' @S3method expressions ...
expressions.... <- function(x) {
  y <- .Call(`_dots_unpack`, get("x"))
  unclass(structure(y$expr, names=y$name))
}

#' @S3method format deparse
format.deparse <- function(x, ...) {
  format(vapply(x, deparse, "", nlines=1, width.cutoff=100), ... )
}

#' Functions to work with dot-dot-dot (\dots) arguments.
#'
#' These are useful for writing functions that accept any number of
#' arguments but some may be missing. For example, arrays in R can
#' have any number of dimensions, indexed by the \code{\link{"["}}
#' function, where a missing argument means to take all indexes on that
#' dimension. However there is not a good way to replicate
#' \code{\link{"["}}'s behavior in base R; using \code{list(\dots)} to
#' collect all positional arguments will throw errors on missing
#' arguments. These functions help extract names and values of
#' positional arguments without triggering missing-value errors.
#'
#' Parallel functions exist as methods of the \code{\dots} class of
#' objects returned by \code{\link{dots}} that allows you to manipulate
#' \dots argument lists as explicit objects.
#' @param ... Any arguments. Usually you will pass \code{\dots} from the
#' body of a function.
#' @return \itemize{
#' \item For \code{\link{dots_names}}, the names of all arguments. Names are
#' also attached to results from the other functions listed here.
#' \item For \code{\link{dots_missing}}, a logical vector with TRUE for each
#' missing argument.
#' \item For \code{\link{list_missing}}, a named list of all evaluated
#' arguments, where any missing arguments are set to missing.value.
#' \item For \code{\link{list_quote}}, a list of quoted arguments. This extracts
#' the original expressions from a dotlist without forcing evaluation.
#' }
#' @author Peter Meilstrup
#' @aliases dots_missing list_missing list_quote alist
#' @seealso is.missing dots curr
#' @useDynLib ptools _dots_names
#' @export
dots_names <- function(...) .Call(`_dots_names`, get("..."))

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
  out <- vector("list", nargs())
  sym = paste("..", seq_len(nargs()), sep="")
  for (i in seq_len(nargs())) {
    x <- as.name(sym[[i]])
    if (eval(call("missing", x))) {
      out[[i]] <- missing_value()
    } else {
      out[[i]] <- eval(x)
    }
  }
  n <- dots_names(...)
  if (!is.null(n)) names(out) <- n
  out
}

#' @export
list_quote <- function(...) substitute(alist(...))[-1]

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
#' @seealso "%<<%" "%<<<%" "%()%" "[...." "[[....", "names...."
#' @examples
#' reverse.list <- function(...) {
#'  d <- dots(...)
#'  list \%()\% rev(d)
#' }
#' reverse.list("a", b="bee", c="see")
#'
#' named.list <- function(...) {
#'  d <- dots(...)
#'  list \%()\% d[names(d) != ""]
#'  }
#' named.list(a=1, b=2*2, stop("this is not evaluated"))
#' @export
dots <- function(...) structure(if (nargs() > 0) get("...") else NULL,
                                class="...")

#' Return an empty symbol.
#'
#' The empty symbol is used to represent missing values in the R
#' language; for instance in the value of formal function function
#' arguments when there is no default; in the expression slot of a
#' promise when a missing argument is given; bound to the value of a
#' variable when it is called with a missing value;
#'
#' @param n Optional; a number. If provided, will return a list of
#' missing values with this many elements.
#' @return A symbol with empty name, or a list of such.
#' @seealso list_missing is.missing
#' @examples
#' # These statements are equivalent:
#' quote(function(x, y=1) x+y)
#' call("function", as.pairlist(x=missing_value(), y=1), quote(x+y))
#'
#' # These statements are also equivalent:
#' quote(df[,1])
#' substitute(df[row,col], list(row = missing_value(), col = 1))
#'
#' # These statements are also equivalent:
#' quote(function(a, b, c, d, e) print("hello"))
#' call("function", as.pairlist(setNames(missing_value(5), letters[1:5])), quote(print("hello")))
#' @export
missing_value <- function(n) {
  if (missing(n)) {
    quote(expr=)
  } else {
    rep(list(quote(expr=)), n)
  }
}

#' @S3method "print" "..."
`print....` <- function(x, ...) invisible(cat("<...[", length(x), "]>\n"))

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
#' @aliases %()% %<<% %<<<% %__% curr curl
#' @name grapes-open-paren-close-paren-grapes
#' @return \itemize{ \item For \code{\%()\%}, the result of calling
#' the function with the arguments provided. When \code{x} is a
#' \code{\dots} object, its contents are passed inithout
#' evaluating. When \code{x} is another type of sequence its elements
#' are put in the value slots of already-evaluated promises. This is
#' slightly different behavior from \code{\link{do.call}(f,
#' as.list(x), quote=TRUE)}, which passes unevaluated promises with
#' expressions wrapped in \code{link{quote}}. This makes a difference
#' if \code{f} performs nonstandard evaluation.  \item For
#' \code{\%<<\%} and \code{\%<<<\%}, a new function with the arguments
#' partially applied. For \code{f \%<<<\% arglist}, the arguments will
#' be placed in the argument list before any further arguments; for
#' \code{f \%<<\% arglist} the arguments will be placed afterwards.
#' \item \code{curr} and \code{curl} are standalone functions that
#' partially apply arguments to functions; \code{curr(f, a=1, b=2)} is
#' equivalent to \code{f \%<<\% dots(a=1, b=2)}, and \code{curl} is
#' the "left curry" corresponding to \code{\%>>\%}. \item For
#' \code{\%__\%}, the two operands concatenated together. The result will be
#' a list, or a \code{dots} object if any of the operands are
#' \code{dots} objects.  }
#' @note "Curry" is a slight misnomer for partial function application.
#' @author Peter Meilstrup
#' @export "%()%"
`%()%` <- function(f, arglist) UseMethod("%()%", arglist)

#' @S3method "%()%" "..."
`%()%....` <- function(f, arglist) {
  # this method elegant but doesn't work on some
  # nonstandard-eval functions (e.g. alist $()$ dots(...) just returns
  # quote(...))?
  if (length(arglist) == 0) return(f())
  assign("...", arglist)
  f(...)
}

#' @S3method "%()%" default
`%()%.default`  <- function(f, arglist, .envir=parent.frame()) {
  if (length(arglist) == 0) return(f())
  assign("...", as.dots.literal(as.list(arglist)))
  f(...)
}

#' @export
`%<<%` <- function(f, x) UseMethod("%<<%", x)

#' @export
`%<<<%` <- function(f, x) UseMethod("%<<<%", x)

#' @S3method "%<<%" "..."
`%<<%....` <- function(f, x) {
  if (length(x) == 0) return(f)
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

#' @S3method "%<<<%" "..."
`%<<<%....` <- function(f, x) {
  if (length(x) == 0) return(f)
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

#also a standalone right-curry and left-curry; does not use
#S3-dispatched dots objects.

#' @export
curr <- function(f, ...) {
  if (missing(...)) {
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
        f(..., ...)
      }
    }
  }
}

#' @export
curl <- function(f, ...) {
  if (missing(...)) {
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
        f(..., ...)
      }
    }
  }
}

#Curry methods for plain values.
#Here we reuse %()% since we had a time getting it to follow the desired
#semantics.

#' @S3method "%<<%" default
`%<<%.default` <- function(f, x) `%<<%....`(f, as.dots.literal(x))

#' @S3method "%<<<%" default
`%<<<%.default` <- function(f, x) `%<<<%....`(f, as.dots.literal(x))

#' @export
`%__%` <- function(x, y) UseMethod("%__%", x)

#' @S3method "%__%" "..."
#' @export
`%__%....` <- function(x, y) UseMethod("%__%....", y)

#' @S3method "%__%...." "..."
`%__%........` <- function(x, y, ...) {
  if (length(x) == 0) return(y)
  if (length(y) == 0) return(x)
  dotslists <- list(x, y)
  count <- 0
  rm("...")
  makeActiveBinding("...", function(x) {
    count <<- count+1
    dotslists[[count]]
  }, environment())
  dots(..., ...)
}

#' @S3method "%__%...." default
`%__%.....default` <- function (x, y) `%__%........`(x, as.dots.literal(y))

#' @S3method "%__%" default
#' @export
`%__%.default` <- function(x, y) UseMethod("%__%.default", y)

#' @S3method "%__%.default" "..."
`%__%.default....` <- function (x, y) `%__%........`(as.dots.literal(x), y)

#' @S3method "%__%.default" default
`%__%.default.default` <- c

#' Convert a list of expressions into a \code{\dots} object (a list of
#' promises.)
#'
#' @param x a vector or list.
#' @param .envir The environment within which each promise will be evaluated.
#' @return An object of class \code{\dots}. For \code{as.dots}, the
#' list items are treated as expressions to be evaluated. For
#' \code{as.dots.literal}, the items are treated as literal values.
#' @seealso dots "%<<%" "%<<<%" "%()%" "[...." "[[....", "names...."
#' @author Peter Meilstrup
#' @aliases as.dots.literal
#' @export
as.dots <- function(x, .envir=parent.frame()) UseMethod("as.dots")

#' @S3method as.dots "..."
as.dots.... <- function(x, ...) x

#' @S3method as.list "..."
as.list.... <- function(x) list %()% x

#' @S3method as.dots default
as.dots.default <- function(x, .envir=parent.frame())
  do.call(dots, as.list(x), FALSE, .envir)

#' @useDynLib ptools _as_dots_literal
#' @export
as.dots.literal <- function(x)
  .Call(`_as_dots_literal`, as.list(x), do.call(dots, vector("list", length(x))))

#' Check if list members are equal to the "missing value."
#'
#' For \code{\dots} objects as made by \code{\link{dots}}, performs
#' this check without forcing evaluation.
#' @param x An object
#' @return A vector of boolean values.
#' @author Peter Meilstrup
#' @seealso dots dots_missing missing_value
#' @export
is.missing <- function(x) if (missing(x)) TRUE else UseMethod("is.missing")

#' @S3method is.missing "..."
is.missing.... <- function(x) {
  out <- logical(length(x))
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

#' @S3method "[" "..."
#' @useDynLib ptools _list_to_dotslist
`[....` <- function(x, ...) {
  temp <- .Call(`_dotslist_to_list`, x)
  temp <- temp[...]
  .Call(`_list_to_dotslist`, temp)
}

#' @S3method "[[" "..."
#' @useDynLib ptools _dotslist_to_list
`[[....` <- function(x, ...) {
  temp <- .Call(`_dotslist_to_list`, x)
  do.call(force.first.arg, list(temp[[...]]))
}

#' @S3method "[<-" "..."
`[<-....` <- function(x, ix, value, ...) UseMethod("[<-....", value)

#' @S3method "[<-...." "..."
#' @useDynLib ptools _dotslist_to_list _list_to_dotslist
`[<-........` <- function(x, ix, value, ...) {
  into <- .Call(`_dotslist_to_list`, x)
  from <- .Call(`_dotslist_to_list`, value)
  into[ix, ...] <- from
  .Call(`_list_to_dotslist`, into)
}

#' @S3method "[<-...." "default"
#' @useDynLib ptools _list_to_dotslist
#' @useDynLib ptools _dotslist_to_list
`[<-.....default` <- function(x, ix, value, ...) {
  into <- .Call(`_dotslist_to_list`, x)
  from <- .Call(`_dotslist_to_list`, as.dots.literal(value))
  into[ix, ...] <- from
  .Call(`_list_to_dotslist`, into)
}

#' @S3method "[[<-" "..."
#' @useDynLib ptools _list_to_dotslist
#' @useDynLib ptools _dotslist_to_list
`[[<-....` <- function(x, ...) {
  into <- .Call(`_dotslist_to_list`, x)
  from <- .Call(`_dotslist_to_list`, dots(...))
  #value comes last in an array assignment
  aargs <- c(.Call(`_dotslist_to_list`, dots(into)), from)
  aargs[length(aargs)] <- .Call(`_dotslist_to_list`, dots(from[[length(from)]]))
  into <- `[[<-` %()% .Call(`_list_to_dotslist`, aargs)
  .Call(`_list_to_dotslist`, into)
}


#' @S3method "$" "..."
#' @useDynLib ptools _dotslist_to_list
`$....` <- function(x, name) {
  temp <- .Call(`_dotslist_to_list`, x)
  do.call(force.first.arg, list(do.call(`$`, list(temp, name))))
}

#' @S3method "$<-" "..."
#' @useDynLib ptools _dotslist_to_list
#' @useDynLib ptools _list_to_dotslist
`$<-....` <- function(x, name, ...) {
  into <- .Call(`_dotslist_to_list`, x)
  from <- .Call(`_dotslist_to_list`, dots(...))
  eval(call("$<-", quote(into), name, quote(from[[length(from)]])))
   .Call(`_list_to_dotslist`, into)
}

#' @S3method "names" "..."
#' @useDynLib ptools _dots_names
names.... <- function(x) .Call(`_dots_names`, x)

#' @useDynLib ptools _dotslist_to_list _list_to_dotslist
`names<-....` <- function(x, value) {
  temp <- .Call(`_dotslist_to_list`, x)
  names(temp) <- value
  .Call(`_list_to_dotslist`, temp)
}

#force() forces "the argument named x", while force.first.arg is
#agnostic to the name.
force.first.arg <- function(...) ..1
