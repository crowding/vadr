#' @include lru.R
NULL

#Cache for macro expansions
cache <- lru_cache()

#Macro expansions may employ "...", and there's no way for the
#compiler to tell here. This stops the "... may be used in an
#incorrect context" warning.
cacheenv <- (function(...) environment())()

#Memoize a function based on the raw pointers of the _expression_
#representation
#of its arguments. (this technique doesn't seem to work too well on
#_evaluated_ objects since there is so much copying of them.
macro_cache <- function(fn, JIT=FALSE) {
  force(fn)
  delayedAssign("fn_pointer", object_pointers(list(fn)))

  function(...) {
    digest <- expressions_and_pointers(...)
    key <- paste(c(fn_pointer, names(digest)), collapse=".")
    cache(
      key,
      if(JIT) {
        do.call(fn, list_quote(...), quote=TRUE)
      } else  {
        compile(do.call(fn, list_quote(...), quote=TRUE),
                cacheenv,
                options=list(suppressUndefined=TRUE))
      })
  }
}

#' Report on macro cache contents.
#'
#' @return list with entries "hits", "misses", and "size"
#' @author Peter Meilstrup
#' @export
macro_cache_report <- function() {
  hitdata <- mget(c("hits", "misses", "expired", "entries"), environment(cache))
  as.list(hitdata)
}
