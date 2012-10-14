context("ammoc")

test_that("returns first arg, discards rest", {
  y <- "hello"
  res <- ammoc(x <- y, rm(y))
  expect_false(exists("y"))
  expect_equal(x, "hello")
  expect_equal(res, "hello")
})
