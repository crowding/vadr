context("Promise extraction")

test_that("can register arguments to actual environments", {
  f1 <- function(a, ...) { #(one@parent
    b <- 1
    where = "f1"
    f2(b, where, ...)
  }
  f2 <- function(a, ...) { # 1@f1, where@f1
    b <- 2
    where <- "f2"
    f3(b, where, ...)
  }
  f3 <- function(a, b, c, ...) { #b@f2, where@f2, where@f1
    stop("not written")
  }
  f1(one)
})

test_that("arg_env error on evaluated promise", {
  stop("not written")
})

test_that("get dotslists of args direct or by name", {
  stop("not written")
})

test_that("get all arguments, named or no, of present function", {
  stop("not written")
})
