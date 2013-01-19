context("safe functions")

`%is a%` <- function(x, y) y %in% class(x)
`%is%` <- expect_equal

test_that("syntax for safe functions", {
  fun(x)(0) %is a% "function"
  fun[x](x+1) %is a% "function"
  fun[x][[y]](if(runif(1) <- 0.5) {y <- 1; missing(y)}) %is a% "function"
  f <- fun[x][[y]]:{
    y <- 2*x
    y + 1
  } %is a% "function"
})

test_that("[[]] explicitly declares local", {
  y <- 2
  f1 <- fun[x][[]]:{
    x+y
  }
  f1(2) %is% 4

  f2 <- fun[x][[]]:{
    assign("y", 2)
    x+y
  }
  expect_error(f2(2))
})

test_that("variables appearing left of <- are guessed local", {
  y <- 2
  f1 <- fun[x]:{
    y <- x + 1
    y + x
  }
  f1(2) %is% 4

  f3 <- fun[x]:{
    y <- y + 1
    y + x
  }
  expect_error(f(3))

  f4 <- fun[x]:{
    y <- x + 1
    y + x
  }
  f4(10) %is% 22
})

test_that("[] handles defaults", {
  y <- 100
  x <- 200

  f1 <- fun[x=1, y]:{
    expect_true(missing(y))
    x
  }
  f1(1) %is% 1
  f2(x) %is% 2

  f2 <- fun[x=y+1, y=2*x]:{
    x*y
  }
  f2(x=2) %is% 8
  f2(y=2) %is% 12
  expect_error(f2())
})

test_that("local vars are initialized (to promises)", {

  f1[x][[y=x*2]]:{
    x <- x + 1
    y
  }

  f(2) %is% 6;
})

test_that("guessing local variables works on subassignments", {
  y <- list(1, 2, 3)

  f1 <- fun[]:{
    names(y) <- c("a", "b", "c")
  }

  expect_error(f1())

  `sub<-` <- function(tmp=rep(list(NULL), length(y)), y) {
    names(tmp) <- y
  }

  f2 <- fun():{
    sub(y) <- c("a", "b", "c")
  }

  expect_equal(f2(), list(a=NULL, b=NULL, c=NULL))
})

test_that("safe function knows about bind[]", {
  #and via a mechanism that is semi-available to teach it about other
  #nonstandard-bindings
  butlast <- fun[x]:{
    bind[...=rest, last] <- x
    rest
  }

  getname <- fun[x]:{
    bind[name=bind[first=first, last=last], ...=] <- x
    paste(first, last)
  }

  butfirst(c(1, 2, 3)) %is% c(1,2)
  getname(list(first="Harry", middle="S", last="Truman")) %is% "Harry Truman"
})
