#Cache for macro expansions
cache <- new.env(hash=TRUE, parent=emptyenv())
hitdata <- as.environment(list(
    hits = 0,
    misses = 0,
    expired = 0))

#Macro expansions may employ "...", and there's no way for the
#compiler to tell here. This stops the "... may be used in an
#incorrect context" warning.
cacheenv <- (function(...) environment())()

#Memoize a function based on the _expression_ representation
#of its arguments. (this technique doesn't seem to work too well on
#_evaluated_ objects since there is so much copying of them --
#play around with object_pointers.
macro_cache <- function(fn, JIT=FALSE) {
  force(fn)
  delayedAssign("fn_pointer", object_pointers(list(fn)))

  function(...) {
    digest <- expressions_and_pointers(...)
    key <- paste(c(fn_pointer, names(digest)), collapse=".")
    if (exists(key, envir=cache)) {
      assign("hits", hitdata$hits + 1, envir = hitdata)
      result <- cache[[key]][[1]]
    } else {
      assign("misses", hitdata$misses + 1, envir = hitdata)
      result <- do.call(fn, list_quote(...), quote=TRUE)
      if (JIT)
          result <- compile(result, cacheenv,
                            options=list(suppressUndefined=TRUE))
      #Hold on to the list of expression objects to keep them from
      #getting stale or updating.
      cache[[key]] <- list(result, digest)
      #TODO make a method for expiring old objects from the cache.
      #LinkedHashMap or equivalent.
    }
    result
  }
}

#' Report on macro cache contents.
#'
#' @return list with entries "hits", "misses", and "size"
#' @author Peter Meilstrup
#' @export
macro_cache_report <- function() {
  c(as.list(hitdata), size=length(cache))
}
