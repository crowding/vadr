context("Promise extraction")

`%is%` <- expect_equal

test_that("can register arguments to actual environments", {

  f1 <- function(a, ...) { #a=one@top, two@top
    b <- 1
    where = "f1"
    f2(b, where, ...) #b, where, two
  }
  f2 <- function(a, ...) { # a=b@f1, where@f1, two@top
    b <- 2
    where <- "f2"
    f3(b, where, ...) #b@f2, where@f2, where@f1, two@top
  }
  f3 <- function(a, b, c, ...) { #a=b@f2, b=where@f2, c=where@f1, two@top
    arg_expr(a) %is% quote(b)
    arg_expr(b) %is% quote(where)
    arg_expr(c) %is% quote(where)
    dots_expressions(...) %is% alist(two)
    arg_env(a)$where %is% "f2"
    arg_env(b)$where %is% "f2"
    arg_env(c)$where %is% "f1"
    dots_environments(...)[[1]]$where %is% "top"
  }
  where <- "top"
  f1(one, two)
})

test_that("arg_env error on evaluated promise", {
  f1 <- function(x) arg_env(x)
  f2 <- function(...) {
    list(...)
    f1(...)
  }
  expect_error(f2(124), "evaluated")
})

test_that("empty arguments return missing value and empty environment", {
  f1 <- function(x) arg_env(x)
  f2 <- function(x) arg_expr(x)
  expect_identical(f1(), emptyenv())
  expect_identical(f2(), missing_value())
})

test_that("get dotslists of args direct", {
  f1 <- function(x, y) arg_dots(x, b=y)
  d <- f1(x=one.arg, two.arg)
  names(d) %is% c("", "b")
  expressions(d) %is% alist(one.arg, b=two.arg)
  expect_identical(environments(d), list(environment(), b=environment()))
})

test_that("get dotslist of args by name", {
  f1 <- function(x, y) get_dots(c("x", b="y"))
  d <- f1(x=one.arg, two.arg)
  names(d) %is% c("", "b")
  expressions(d) %is% alist(one.arg, b=two.arg)
  expect_identical(environments(d), list(environment(), b=environment()))
})

test_that("get dotslists handles missing arguments", {
  f1 <- function(x, y) arg_dots(x, b=y)
  d <- f1(, two.arg)
  is.missing(expressions(d)) %is% c(TRUE, b=FALSE)
  expect_identical(environments(d), list(emptyenv(), b=environment()))
})

test_that("error on missing thing", {
  f <- function(x) arg_env(y)
  expect_error(f(), "not")
  f <- function(x) arg_expr(y)
  expect_error(f(), "not")
  f <- function(x) arg_dots(y)
  expect_error(f(), "not")
})

test_that("getting promises handles DDVAL (..1 etc)", {
  brace <- function(...) {
    e <- arg_env(..1)
    f <- arg_expr(..2)
    do.call(`{`, list(...), envir=e)
  }

  x <- 1
  y <- quote(x+3)
  brace(x, y) %is% 4
})

all.identical <- function(list) {
  falsefalse <- environment() #unique for this invocation
  ident <- function(x, y) if (identical(x, y)) x else falsefalse
  answer <- Reduce(ident, list)
  !identical(answer, falsefalse)
}

test_that("environment to dots", {
  capture <- function(a=plan, ..., z=thingy) {
    environment()
  }

  captured <- capture(one + two, f=four, five)
  d <- env2dots(captured)

  sort(names(d)) %is% c("", "a", "f", "z")
  names(d)[[order(names(d))[[1]]]] <- "anewname"
  (expressions(d)[sort(names(d))]
   %is% alist(a=one + two, anewname=five, f=four, z=thingy))
  expect_true(all.identical(environments(d)[c("anewname", "a", "f")]))
  expect_false(identical(environments(d)[["z"]], environments(d)[["a"]]))
})

test_that("dotlist to environment", {
  got <- FALSE
  id <- function(x) {
    got <<- TRUE;
    x
  }
  a <- dots(a=one, b=two, c=three, four, five, d=id(4))
  e <- dots2env(a)
  sort(ls(e)) %is% c("a", "b", "c", "d")
  got %is% FALSE
  e$d %is% 4
  got %is% TRUE
  substitute(b+c, e) %is% quote(two + three)
  substitute(list(...), e) %is% quote(list(four, five))

  # use existing, env, appending to ...
  test <- function(a, b, ...) {
    dots2env(dots(c=five, d=six, seven, eight), environment())
  }
  e2 <- test(one, two, three, four)
  substitute(list(a, b, c, d), e2) %is% quote(list(one, two, five, six))
  substitute(list(...), e2) %is% quote(list(three, four, seven, eight))
})

test_that("formals replication", {
  test <- function(a, b, ...) {
    list(nargs(), missing(a), missing(b), missing(...))
  }

  wrapped <- wrap_formals(function(...) test(...), test)

  test() %is% list(0, TRUE, TRUE, TRUE)
  wrapped() %is% list(0, TRUE, TRUE, TRUE)

  test(1) %is% list(1, FALSE, TRUE, TRUE)
  wrapped(1) %is% list(1, FALSE, TRUE, TRUE)

  test(1, 2) %is% list(2, FALSE, FALSE, TRUE)
  wrapped(1, 2) %is% list(2, FALSE, FALSE, TRUE)

  test(1, 2, 3) %is% list(3, FALSE, FALSE, FALSE)
  wrapped(1, 2, 3) %is% list(3, FALSE, FALSE, FALSE)

  test(z=3) %is% list(1, TRUE, TRUE, FALSE)
  wrapped(z=3) %is% list(1, TRUE, TRUE, FALSE)
})
