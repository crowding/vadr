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

#' Functions to work with dot-dot-dot arguments.
#'
#' These are useful for writing functions that accept any number of
#' arguments but some may be missing. For example, arrays in R can
#' have any number of dimensions, indexed by the \code{\link{"["}}
#' function, where a missing argument means to take all indexes on that
#' dimension. However there is not a good to replicate
#' \code{\link{"["}}'s behavoior in pure R; using \code{list(...)} to
#' collect all positional arguments will throw errors on missing
#' arguments. These functions help extract names and values of
#' positional arguments without triggering missing-value errors.
#'
#' @param ... Any arguments. Usually you will pass \code{...} from the
#' body of a function.
#' @return For \code{\link{dots_names}}, a character vector giving the names of
#' all arguments.
#'
#' For \code{\link{dots_names}}, the names of all arguments.
#'
#' For \code{\link{dots_missing}}, a logical vector with TRUE for each
#' missing argument.
#'
#' For \code{\link{list_missing}}, a named list of all evaluated
#' arguments, where any missing arguments are set to NULL.
#'
#' For \code{\link{dots_names}}, a list of quoted arguments.
#' @author Peter Meilstrup
#' @aliases dots_missing list_missing list_quote
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
  result = list(nargs())
  sym = paste("..", seq_len(nargs()), sep="")
  for (i in seq_len(nargs()))
    result[[i]] <- if (do.call("missing", list(as.name(sym[[i]]))))
      NULL else evall(as.name(sym[[i]]))
  names(result) <- dots_names(...)
  result
}

#' @export
list_quote <- function(...) eval(substitute(alist(...)))

#' Partially apply arguments to a function.
#'
#' The arguments given in brackets are saved and a new function is
#' constructed.
#'
#' The difference between
#' \code{'[.function'} and \code{'[.function'} is that the single
#' bracket applies arguments to the right, while the double bracket
#' applies arguments to the left.
#'
#' This curry captures the arguments you give as promises; they will
#' only be evaluated once. if the curried function chooses to do
#' so. [For another type of curry see addDefaults?]

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
#' @aliases %()% %<<% %>>% %__%
#' @name grapes-open-paren-close-paren-grapes
#' @return \itemize{
#' \item For \code{\%()\%}, the result of calling the function with
#' the arguments provided. This is effectively the same as
#' \code{\link{do.call}(f, as.list(x), quote=TRUE)}.
#' \item For \code{\%<<\%} and \code{\%>>\%}, a new function with the
#' arguments partially applied. For \code{arglist \%>>\% f}, the
#' arguments will be placed in the argument list before any further
#' arguments; for \code{f \%<<\% arglist} the arguments will be placed
#' afterwards.
#' \item For \code{\%__\%}, the two operands pasted together. The result
#' will be a list, or a \code{dots} object if any of the operands are
#' \code{dots} objects.
#' }
#'
#' @author Peter Meilstrup
#' @export
`%()%` <- function(f, arglist) UseMethod("%()%", arglist)

#' @S3method "%()%" ...
`%()%....` <- function(f, arglist, ...) {
  # this method elegant but doesn't work on some
  # nonstandard-eval functions (e.g. alist $()$ dots(...) just returns
  # quote(...))?
  assign("...", arglist)
  f(...)
}

#' @S3method "%()%" default
#' @useDynLib ptools
`%()%.default`  <- function(f, arglist, .envir=parent.frame())
  do.call(f, as.list(arglist), TRUE, .envir)

#' @export
`%<<%` <- function(f, x) UseMethod("%<<%", x)

#' @S3method "%<<%" "..."
`%<<%....` <- function(f, x) stop()

#' @S3method "%<<%" default
`%<<%.default` <- function(f, x) stop()

#' @export
`%>>%` <- function(f, x) UseMethod("%>>%", x)

#' @S3method "%>>%" "..."
`%>>%....` <- function(f, x) stop()

#' @S3method "%>>%" default
`%>>%.default` <- function(f, x) stop()

cdots <- function(x, y) UseMethod("cdots", x)

#' @export
`%__%` <- cdots

#' @S3method "cdots" "..."
`cdots....` <- function(x, y) UseMethod("cdots....", y)

cdots........ <- function(x, y, ...) {
  #we can trick R's eval system into concatenating together multiple
  #dotslists like this. Why? Basically, when the evaluator finds the
  #special symbol "..."  in a call it knows to start unpacking a
  #DOTSXP (otherwise opaque to R code) into multiple arguments. But it
  #looks up whatever "..." is in the frame, so we can intercede with
  #an active binding to swap out different dotlists.
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
