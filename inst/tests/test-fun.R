context("functions")

test_that("func captures expressions", {
  f <- fun(x + y)
  expect_equal(f(x=2,y=3), 5)
})

test_that("func captures lexical scope", {
   xinc <- NULL
   if (exists("y")) rm("y", inherits=TRUE)
   f <- local({
     x <- 4
     xinc <<- function() {x <<- x + 1}
     fun(y+x)
   })

   expect_equal(f(y=5), 9)
   expect_equal(f(y=6), 10)
   expect_equal(f(y=6), 10)
   xinc() #x is now 5
   expect_equal(f(y=6), 11)
   expect_error(f()) #there is no y in scope
   y <- 4 #now there is y in a parent scope of f
   expect_equal(f(), 9) #4 + 5
})

test_that("fun arguments are only names that don't look like calls", {
  f <- fun(y/x)
  expect_equal(f(10, 2), 5)
  expect_equal(f(x=4,y=32), 8)

  expect_equal(names(formals(f)), c("y", "x"))
})

test_that("fun captures all names in order of appearance on request", {
  f1 <- fun(x + y, .all.names=TRUE)
  expect_equal(f1(x=4,y=5), 9)
  expect_equal(f1(x=4,y=5,`+`=`*`), 20)
  expect_equal(f1(`/`,10,2), 5)

  expect_equal(names(formals(f1)), c('+', 'x', 'y'));

  f2 <- fun(x*(1-x), .all.names=TRUE)

  expect_equal(f2(x=0.25), 0.1875)
  expect_equal(names(formals(f2)), c('*', 'x', '(', '-'))
})

test_that("fun works with ...", {
  ##You can use "..." in a func expression.
  ##Mixing ... and all.names=TRUE is not really recommended though.
  f <- fun(list(...))
  expect_equal(f(1,2,y=4), list(1,2,y=4))
})

test_that("summarizer", {
  f <- summarizer(z=x/y,zz=2*z)
  expect_equal(f(x=10,y=2), data.frame(z=5,zz=10))
})

test_that("summarizer with missing names", {
   f <- summarizer(x/y, 2*z)
   expect_equal(f(10, 2, 3), plyr:::quickdf(list(`x/y`=5, `2 * z`=6)))
})

test_that("fun equivalent of mutate", {
})
