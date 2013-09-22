#' Alternative to mapply with a cleaner calling convention.
#'
#' @usage mply(fn, ...)(...)
#' @param fn A function to apply.
#' @param ... (in the first set of parens) Extra arguments to be given to
#' each call.
#' @param ... (in the second set of parens) Arguments to apply; they are
#' expected to be all of the same length.
#' @return A list.
#'
#' @note After more than a hundred or so elements, this
#' has less overhead per loop than \code{\link{mapply}}.
#' @author Peter Meilstrup
#' @export
mply <- function(...){
  fn <- curr(...)
  function(...) {
    argnames <- names(dots(...)) %||% rep("", nargs())
    output <- vector("list", length(..1))
    do.call(mply_loop, as.list(argnames))
    output
  }
}

mply_loop <- macro(function(...) {
  argnames <- list(...)
  qq(for (i in seq_along(..1)) {
    output[[i]] <- fn( ..( qqply(
        `.(name)` = `.(paste0("..", x))`[[i]] )(
        name = argnames, x = seq_along(argnames)) ) )
  })
})
