#' Augmented assignment.
#'
#' Assigns to a variable the result of a transformation applied to that
#' variable.
#' @param x A variable to assign to,
#' @param y An expression, interpreted in the manner of \code{\link{chain}}.
#' @return The rvalue of the assignment.
#' @author Peter Meilstrup
#' @seealso chain
#' @export
#' @examples
#' library(stringr)
#' x <- 'a'
#' x %<~% toupper
#' x %<~% str_dup(3)
#' x %<~% rep(3)
#' x %<~% .[2:3]
`%<~%` <- macro(function(x, y) {
  if ('.' %in% all.names(y)) {
    qq(.(x) <- (function(.) .(y))(.(x)))
  } else if(is.call(y)) {
    qq(.(x) <- .(y[[1]])(.(x), ..(as.list(y)[-1])))
  } else {
    qq(.(x) <- .(y)(.(x)))
  }
})
