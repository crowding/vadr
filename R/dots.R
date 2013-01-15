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
#' \itemize{
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
  result
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
