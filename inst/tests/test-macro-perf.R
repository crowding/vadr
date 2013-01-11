context("macro performance")

## test via performance that creation of macros is effectively memoized.

## define a simple macro: unrolled_loop. should be slow to cons up
## every time, but fast if memoized

test_that("caching makes macros faster", {
  library(microbenchmark)
  library(plyr)
  .unrolled_loop_body <- function(index_var, repetitions, body) template({
    ...( lapply(seq_len(as.numeric(repetitions)),
                function(x)
                substitute.nq(body,
                              structure(list(x),
                                        names=as.character(index_var)))))
  })

  unrolled_loop <- macro(cache=FALSE, .unrolled_loop_body)
  unrolled_loop_memo <- macro(cache=TRUE, .unrolled_loop_body)

  results <- microbenchmark(
    no_cache = {x <- numeric(100)
                unrolled_loop(i, 100, x[i] <- runif(1, max=i))
              },
    cache = {x <- numeric(100)
             unrolled_loop_memo(i, 100, x[i] <- runif(1, max=i))
           },
    bare_loop = {x <- numeric(100); for(i in 1:100) x[i] <- runif(1, max=i)}
    )

  results <- dlply(results, "expr", colwise(median))
  expect_true(results$no_cache > results$cache*2)
  expect_true(results$cache < results$bare_loop*2)
})
