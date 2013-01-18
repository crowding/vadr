(context "Dots")

test_that("x <- dots() captures dots and %()% calls with dots", {
  x <- 1;
  y <- 3;

  f <- `/`

  d <- dots(y=x, 4)
  expectEqual( f %()% d, 0.5 )
})

test_that("Curried dots evaluate like promises", {
  do_tests <- function() {
    for (t in tests()) {
      w <- 2
      x <- 3
      y <- 4
      z <- 5
      d <- dots(w+x, y+z)
      f <- `*`
      eval(t)
    }
  }

  tests() <-
    alist(
      {
        expect_equal((f %<.% d), 120)
        expect_equal(f %()% d)
      }, {
        
      }, {

      },

      )
  do_tests()
})

test_that("Curry operators concatenate (unevaluated) dots", {

})

test_that("dots [] operator subsets without evaluating []", {

})

test_that("dots [[]] operator goes ahead and evaluates", {

})

test_that("dots names() method extracts tags", {

})

test_that("dots unpack() method extracts dots information", {

})

test_that("unpack(dots(...)) unpacks a dotslist and exposes promise behavior", {

  evalX <- function(x, ...) {
    force(x)
    do_describe(yy=x, ...)
  }

  do_dots <- function(...) {
    dots_info(...)
  }

  test <- do_dots(x=1+2, y=z, z=2)

})

test_that("unpack(dots(...)) descends through promise chains if necessary", {
  
})

test_that("dots_missing", {
  expect_equal(c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE),
               dots_missing(a, b, c=, 4, d=x+y, ) )

  checkMyDots <- function(...) {
    missing <- dots_missing(...)
  }

  expect_equal(c(FALSE, FALSE, c=TRUE, FALSE, d=FALSE, TRUE),
               checkMyDots(a, b, c=, 4, d=x+y, ) )

  #but it doesn't eval
  expect_equal(c(FALSE, TRUE, FALSE)
               CheckMyDots(stop("no"), c=, stop("no")))
})

test_that("dots_names", {
  expect_equal(c("", "", "c", "", "d", ""),
               dots_names(a, b, c=, 4, d=x+y, ) )

  #and dots_names does not eval dots
  expect_equal(c("", "a")
               dots_names(stop("no"), a=stop("no")))
})
