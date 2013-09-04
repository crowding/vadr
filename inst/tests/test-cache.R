context("cache")

`%is%` <- expect_equal

with_connection <- function(conn, what) {
  force(conn)
  on.exit(close(conn))
  what
}

with_sink <- function(connection, code) {
  sink(connection)
  on.exit(sink())
  code
}

expect_no_print <- function(code, pattern=".+") {
  what <- character(0)
  with_connection(conn <- textConnection("what", "a", TRUE), {
    with_sink(conn, code)
    out <- what
  })
  expect_that(paste(out, collapse=""), not(matches(pattern)))
}

expect_print <- function(code, pattern=".+") {
  what <- character(0)
  with_connection(conn <- textConnection("what", "a", TRUE), {
    with_sink(conn, code)
  })
  expect_that(paste(what, collapse=""), matches(pattern))
}

test_that("internal cache() memoizes a function,", {
  f <- macro_cache(function(x) {print("yay!"); x*2})
  a <- 1:5
  b <- a
  #expect_print(f(a) %is% seq(2, 10, 2), "yay")
  #expect_no_print(f(b) %is% seq(2, 10, 2))
})
