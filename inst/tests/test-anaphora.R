#anaphora -- functions that do "something" with a (complex expression)
#input without repeating hte conplex expression

library(testthat)

testthat("put() changes a subelement of a structure, returning modified structure", {
  x <- 1:10
  y <- put(x[1], 4)
  expect_equal(x, 1:10)
  expect_equal(y, c(4, 2:10))
  withNames(1:10, letters(1:10))
})

testthat("alter() runs subelements of s structure through a chain", {
  x <- structure(1:5, letters[1:5])
  y <- alter(names(x[5]), toupper)
  y <- names(x(5))
})

testthat("on() specifies target put/alter target (if computed value)", {
  x <- put(names(on(1:10)), letters(1:10))
  expect_equal(x, structure(1:10, names=letters(1:10)))

  y <- alter(names(on(rev(x))), toupper)
  expect_equal(y, structure(10:1, names=letters(10:1)))
})

testthat("via<- allows values to be modified, in the manner of chain()", {
  x <- 1:5
  via(names[x[5]], toupper) <- letters[5]
  expect_equal(x, c(1,2,3,4,E=5))
})

testthat("default<- lets you provide a default value from bind[]", {
  local({
    bind[a = default(a, 5), default(b, 5)] <- list(a=4, b=3)
    expect_equal(a, 4)
    expect_equal(b, 3)
  })
  local({
    bind[a = default(a, 5), default(b, 5)] <-
        list(a=missing_value(), b=missing_value())
    expect_equal(a, 5)
    expect_equal(b, 5)
  })
  local({
    bind[default(a, 5), b] <- list(missing_value(), 4)
    expect_equal(a, 5)
    expect_equal(b, 4)
  })
})

testthat("default<- allows there to be missing matches as well", {
  local({
    expect_error({
      bind(a, b) <- list(a=4)
    })
  })
  local({
    bind(a, b=2) <- list(a=4)
  })
  local({
    bind[a = default(a, 5), default(b, 2)] <- list(b=3)
    expect_equal(a, 5)
    expect_equal(b, 3)
  })
})

testthat("<- asserts that the provided value passes a chain with TRUE", {
  check(x, names, !any(.="")) <- structure(1:5, names=letters(1:5))
  expect_equal(x, structure(1:5, names=letters(1:5)))
  expect_error( check(x, .>0, all) <- c(1, -1) )
})
