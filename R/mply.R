#' Alternative to \code{\link{mapply}} with a cleaner calling convention.
#'
#' \code{mply} applies a function to corresponding elements from any number
#' of vector arguments.
#'
#' @usage mply(...)(...)
#' @param ... In the first set of parens, a function to apply, and extra
#' arguments to be applied to each call. In the second set of parens, arguments
#' to iterate over. Multiple arguments will be recycled up to the length of the
#' longest argument.
#' @return A list.
#' @author Peter Meilstrup
#' @export
mply <- function(...) {
  fn <- curr(...)
  function(...) {
    loop <- do.call(mply_loop, dots_expressions(...)) #i.e. in this environment
    args <- list(...)
    if (length(args) == 0) return(list())
    lengths <- vapply(args, length, 0)
    L <- max(lengths)
    if (L != 0 && (any(lengths==0) || any(L %% lengths != 0))) {
      warning("Longer object length is not a multiple of shorter object length")
    }
    args <- list(...)
    loop(L)
}}

mply_loop <- macro(function(...) {
  args <- list(...)
  N <- names(args)
  syms <- lapply(paste0("..", seq_along(args)), as.symbol)
  names(syms) <- N
  qq(function(L) {
    output <- vector("list", L)
    for(i in seq_len(L))
      output[[i]] <-
        fn(..(
          mapply(function(sym, j) substitute(sym[[(i-1) %% lengths[[j]] + 1]],
                                             list(sym=sym, j=j)),
                 syms, seq_along(syms))))
    output
  })
})
