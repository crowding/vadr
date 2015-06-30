context("macros")

`%is%` <- expect_equal

test_that("quoting.env", {
  en <- quoting.env(c('+', '(', 'a', 'b', '*'), environment())

  expect_equal(evalq(a+b, en), quote(a+b))
  expect_equal(evalq(a+(b*a), en), quote(a+(b*a)))
  z <- 100
  expect_equal(evalq(a+(b*a)*z, en), quote(a+(b*a)*100))
})

test_that("quoting.env and '...'", {
  #some special casing is needed to make "..." eval to itself.
  en <- quoting.env(c("a", "b", "c", "list", "..."))
  expect_equal(evalq(c(a, list(sdf = b, ...)), en),
               quote(c(a, list(sdf = b, ...))))
})

test_that("quoting.env and missings", {
  en <- quoting.env(c('[', '<-', 'a', 'b', 'c'))
  expect_equal(evalq(a[1, ] <- b[, 2], en),
               quote(a[1, ] <- b[, 2]))
})

test_that("macro() turns a lexical substitutor function into a macro", {
  d <- function(expr_b) {
    call("+", expr_b, expr_b)
  }

  double <- macro(d)
  expect_equal(double(5), 10)

  x <- 5
  side_effect <- function(){
    x <<- x+1
  }
  expect_equal(double(side_effect()), 13)

  expect_true("macro" %in% class(double))

  expect_equal(attr(double, "orig"), d)
})

test_that("macro cache pays attention to tags", {
  divmacro <- macro(function(a,b) qq(.(a)/.(b)))

  expect_equal(divmacro(10, 5), 2)
  expect_equal(divmacro(5, 10), 0.5)
  expect_equal(divmacro(a=10, b=5), 2)
  expect_equal(divmacro(b=10, a=5), 0.5)
  expect_equal(divmacro(b=5, a=10), 2)
})

test_that("expand_macro expands all visible macros (by one step)", {
  local({
    addmacro <- macro(function(x, y) qq(.(x) + .(y)))
    doublemacro <- macro(function(x, y) qq(.(x) * addmacro(.(y), .(y))))
    #
    expect_equal(expand_macros(quote(addmacro(a, b))), quote(a+b))
    expect_equal(expand_macros_q(addmacro(a, b*y)), quote(a+b*y))
    expect_equal(expand_macros_q(doublemacro(a, b)), quote(a * addmacro(b, b)))
    #macros are expanded from the top down.
    expect_equal(expand_macros_q(addmacro(a, addmacro(b,c))),
                 quote(a+addmacro(b,c)))
    expect_equal(expand_macros_q(addmacro(a, addmacro(b,c)), recursive=TRUE),
                 quote(a+(b+c)))
  })
})

test_that("quote_args", {
  # Function that quotes arguments "like an argument list", returning
  # a pairlist.

  quote_args(a=1, b=y, c=x+y) %is% as.pairlist(alist(a=1, b=y, c=x+y))
  quote_args(a=1, b, c) %is% as.pairlist(alist(a=1, b=, c=))
  expect_error(quote_args(a, b, x+y))
  expect_error(quote_args(a, b, 1))
  expect_error(quote_args(a, , c))
})

test_that("with_arg", {
  (with_arg(a=2, b=3, list(4), list(5))
   %is% list(list(4, a=2, b=3), list(5, a=2, b=3)))

  x <- 1; y <- 2
  (with_arg(a=x+y, list(1), alist(1))
   %is% list(list(1, a=3), alist(1, a=x+y)))

  (with_arg(.collect=c, a=1, b=2, c(1, 2), c(1))
   %is% c(1, 2, a=1, b=2,1, a=1, b=2))
})

## This would be a nice-to-have
## test_that("Formals", {
##   addmacro <- macro(function(x, y) qq(.(x) + .(y)))
##   expect_equal(formals(addmacro), quote_args(x, y))
## })
