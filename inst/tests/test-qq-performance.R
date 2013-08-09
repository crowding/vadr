context("Quasiquote performance")

library(microbenchmark)

test_that("qq should be faster than bquote", {
  x <- 3;
  y <- quote({x;y;z})
  microbenchmark(
      bquote = bquote(function(a, b=.(x)) {foo; bar; .(y)}),
      qq = qq(function(a, b=.(x)) {foo; bar; .(y)}))
})

test_that("qq might even be faster than substitute", {
  y <- quote({x;y;z})
  x <- microbenchmark(
      bquote = bquote(function(a, b=3) {foo; bar; .(y)}),
      qq = qq(function(a, b=3) {foo; bar; .(y)}),
      substitute = substitute(function(a, b=3) {foo; bar; y}, list(y=y)))
})
