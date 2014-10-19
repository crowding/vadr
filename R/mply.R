#' Alternative to mapply with a cleaner calling convention.
#'
#' @usage mply(fn, ...)(...)
#' @param fn A function to apply.
#' @param ... (in the first set of parens) Extra arguments to be given to
#' each call.
#' @param ... (in the second set of parens) Arguments to apply. Arguments will
#' be recycled up to the length of the longest argument.
#' @return A list.
#'
#' @note After more than a hundred or so elements, this
#' has less overhead per loop than \code{\link{mapply}}.
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
