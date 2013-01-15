(context "Dots")

test_that("dots_info unpacks a dotslist and exposes promise behavior", {

  evalX <- function(x, ...) {
    force(x)
    do_describe(yy=x, ...)
  }

  do_dots <- function(...) {
    dots_info(...)
  }

  test <- do_dots(x=1+2, y=z, z=2)

})

test_that("dots_missing", {
  expect_equal(c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE),
               dots_missing(a, b, c=, 4, d=x+y, ) )

  checkMyDots <- function(...) {
    missing <- dots_missing(...)
  }

  expect_equal(c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE),
               checkMyDots(a, b, c=, 4, d=x+y, ) )
})

test_that("dots_names", {
  expect_equal(c("", "", "c", "", "d", ""),
               dots_names(a, b, c=, 4, d=x+y, ) )
})
