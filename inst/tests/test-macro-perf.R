context("macro performance")

## test via performance that creation of macros is effectively memoized.

## define a simple macro: unrolled_loop. should be slow to cons up
## every time, but fast if memoized

test_that("caching makes macros faster", {
  library(microbenchmark)
  library(plyr)

  #you would never expect unrolling a loop to make things faster in R
  #but it's an example of a macro. (more importantly, I think unrolling
  #an expression might be a better microbenchmark than microbenchmark...)
  #(actually, with JIT enabled, the unrolled loop does become slightly
  #faster than the straight loop.)
  .unrolled_loop_body <- function(index_var, repetitions, body) template({
    ...( lapply(seq_len(as.numeric(repetitions)),
                function(x)
                substitute.nq(body,
                              structure(list(x),
                                        names=as.character(index_var)))))
  })
  unrolled_loop <- macro(cache=FALSE, .unrolled_loop_body)
  unrolled_loop_memo <- macro(cache=TRUE, .unrolled_loop_body)
  unrolled_loop_fun <- function() {
    x <- numeric(100)
    unrolled_loop(i, 100, x[i] <- runif(1, max=i))
  }
  unrolled_loop_fun_memo <- function(x) {
    x <- numeric(100)
    unrolled_loop_memo(i, 100, x[i] <- runif(1, max=i))
  }
  bare_loop_fun <- function() {
    x <- numeric(100)
    for(i in 1:100) x[i] <- runif(1, max=i)
  }

  results <- microbenchmark(
    no_cache = {
      x <- numeric(100)
      unrolled_loop(i, 100, x[i] <- runif(1, max=i))
    },
    cache = {
      x <- numeric(100)
      unrolled_loop_memo(i, 100, x[i] <- runif(1, max=i))
    },
    bare_loop = {
      x <- numeric(100);
      for(i in 1:100) x[i] <- runif(1, max=i)
    },
    cache_fun = {
      "microbenchmark workaround"
      unrolled_loop_fun_memo()
    },
    no_cache_fun = {
      "microbenchmark workaround"
      unrolled_loop_fun()
    },
    bare_loop_fun = {
      "microbenchmark workaround"
      bare_loop_fun()
    }
               )

  summary <- dlply(results, "expr", function(x) median(x$time))

  #we expect that not caching is twice as bad as caching (>5x here)
  #and that it's not twice as bad as a hand-unrolled loop (1.16x here)
  #[but it's a pretty large macro expansion; further improvements may
  #be available.]
  with(summary, {
    expect_true(no_cache > cache*2)
    expect_true(cache < bare_loop*2)
    expect_true(cache_fun < bare_loop_fun*2)
    expect_true(cache_fun < bare_loop_fun*2)
  })

  with(summary, cache/bare_loop)
})
