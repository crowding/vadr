#Lightweight in-memory caching

#Memoize a function based on the _expression_ representation
#of its arguments. (this technique doesn't seem to work too well on
#_evaluated_ objects since there is so much copying of them --
#play around with object_pointers.
macro_cache <- function(fn, JIT=FALSE) {
  force(fn)
  expansionCache <- new.env(hash=TRUE, parent=emptyenv())

  function(...) {
    digest <- expressions_and_pointers(...)
    key <- paste(names(digest), collapse=".")
    if (exists(key, envir=expansionCache)) {
      result <- expansionCache[[key]][[1]]
    } else {
      result <- do.call(fn, list_quote(...), quote=TRUE)
      if (JIT)
          result <- compile(result, cacheenv,
                            options=list(suppressUndefined=TRUE))
      #Hold on to the list of expression objects to keep them from
      #getting stale or updating.
      expansionCache[[key]] <- list(result, digest)
      #TODO make a method for expiring old objects from the cache.
      #Perhaps a doubly linked list. whise head is moved up.
    }
    result
  }
}

#get compiler to shut up about "... may be used in an incorrect context"
#which it seems to be wrong about
cacheenv <- (function(...) environment())()
