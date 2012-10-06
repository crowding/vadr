context("macros")

#test_that("registering a macro applies a code transformation on the fly,")

test_that("recapitulating environment", {
  en <- recapitulating.environment(c('+', '(', 'a', 'b', '*'), environment())

  expect_equal(evalq(a+b, en), quote(a+b))
  expect_equal(evalq(a+(b*a), en), quote(a+(b*a)))
  z <- 100
  expect_equal(evalq(a+(b*a)*z, en), quote(a+(b*a)*100))
})

test_that("recapitulating environment and '...'"), {
  #some special casing is needed to make "..." eval to itself.
  en <- recapitulating.env(c("a","b","c","list","..."))
  expect_equal(evalq(c(a, list(sdf=b,...)), en), quote(c(a, list(sdf=b,...))))
})

test_that("recapitulating environment and missings"), {

})

test_that("recapitulating environment and missings", {
  #this doesn't work yet. Some other special handling needed...
  en <- recapitulating.env(
})


#test_that("substitutor makes a nice substitution-macro")

