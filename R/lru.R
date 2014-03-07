library(microbenchmark)

lru_cache <- function(cache.size = 1000) {
  lru <- new.env(hash=TRUE, parent=emptyenv(), size=cache.size)
  pred <- new.env(hash=TRUE, parent=emptyenv(), size=cache.size)
  succ <- new.env(hash=TRUE, parent=emptyenv(), size=cache.size)

  hits <- 0
  misses <- 0
  expired <- 0
  entries <- 0

  pred$TAIL <- "HEAD"
  succ$HEAD <- "TAIL"

  function(key, value) {
    #value lazily forced if not found
    if (exists(key, lru)) {
      hits <<- hits+1
      #move accessed value to front
      new_succ <- succ[[key]]
      new_pred <- pred[[key]]
      succ[[new_pred]] <<- new_succ
      pred[[new_succ]] <<- new_pred
      pred[[succ$HEAD]] <<- key
      pred[[key]] <<- "HEAD"
      succ[[key]] <<- succ$HEAD
      succ$HEAD <<- key
      lru[[key]]
    } else {
      misses <<- misses+1
      lru[[key]] <<- value
      #drop if entries exceeded
      if (entries >= cache.size) {
        last <- pred$TAIL
        succ[[pred[[last]]]] <<- "TAIL"
        pred$TAIL <<- pred[[last]]
        del(last, lru)
        del(last, pred)
        del(last, succ)
        expired <- expired
      } else {
        entries <<- entries + 1
      }
      succ[[key]] <<- succ$HEAD
      pred[[succ$HEAD]] <<- key
      succ$HEAD <<- key
      pred[[key]] <<- "HEAD"
      value
    }
  }
}
