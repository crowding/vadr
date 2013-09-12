context("Quasiquote performance")

library(microbenchmark)
library(reshape2)

test_that("qq should be faster than bquote", {
  x <- 3;
  y <- quote({x;y;z})
  t <- summary(microbenchmark(
      times=250,
      bquote = bquote(function(a, b=.(x)) {foo; bar; .(y)}),
      qq = qq(function(a, b=.(x)) {foo; bar; .(y)})))
  expect_that(t[2, "median"] < t[1, "median"],
              is_true())
})

test_that("qq within acceptible distance of substitute", {
  y <- quote({x;y;z})
  t <- summary(microbenchmark(
      times=500,
      bquote = bquote(function(a, b=3) {foo; bar; .(y)}),
      qq = qq(function(a, b=3) {foo; bar; .(y)}),
      substitute = substitute(function(a, b=3) {foo; bar; y}, list(y=y))))
  expect_that(t[2, "median"] < t[3, "median"]*8, is_true())
 })
