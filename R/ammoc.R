#' Evaluate all arguments in order, but return the first.
#'
#' Useful in macros where you want to clean up a temporary variable
#' but return it; as in \code{ammoc(x, rm(x))}. It's the reverse of
#' the C comma operator. There is an infix alias, \code{\%\'\%}, (i.e.,
#' inverted comma; use whichever you feel a worse pun.)
#'
#' @note In most Lisps, this is called \code{prog1}.
#' @param ... any number of arguments.
#' @aliases %'%
#' @rdname ammoc
#' @return The first argument.
#' @author Peter Meilstrup
#' @export
ammoc <- function (...) list(...)[[1]]


#' @export "%'%"
#' @rdname ammoc
`%'%` <- ammoc
