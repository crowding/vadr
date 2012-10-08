context("macros")

#test_that("registering a macro applies a code transformation on the fly,")

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

test_that("list_with_missing", {
  expect_equal(list_with_missing(1, 2, 3),
               list(1,2,3))

  expect_equal(list_with_missing(1, 2, , "three"),
               alist(1, 2, , "three"))

  expect_equal(list_with_missing(a="one", b=, "three"),
               alist(a="one", b=, "three"))
})

test_that("list_with_missing evaluates arguments in the original scopes", {
  fOne <- function(...) {
    fThree <- function(...) {
      x <- "three"
      list_with_missing(..., three=x)
    }
    fTwo <- function(...) {
      x <- "two"
      fThree(..., two=x)
    }
    x <- "one"
    fTwo(..., one=x)
  }

  x <- "four"
  expect_equal(fOne(four=x),
               list(four="four", one="one", two="two", three="three"))
})

test_that("quoting.env and missings", {
  en <- quoting.env(c('[', '<-', 'a', 'b', 'c'))
  expect_equal(evalq(a[1, ] <- b[, 2], en),
               quote(a[1, ] <- b[, 2]))
})

#test_that("substitutor makes a nice substitution-macro")

