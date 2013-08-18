#anaphora -- functions that do "something" with a (complex expression)
#input without repeating the complex expression
context("anaphora")

test_that("put() changes a subset to a value", {
  x <- 1:10
  y <- put(x, on[1], 4)
  expect_equal(x, 1:10)
  expect_equal(y, c(4, 2:10))
  withNames(1:10, letters(1:10))
})

test_that("alter() sets a sebset to the chain of the subset", {
  x <- structure(1:5, letters[1:5])
  y <- alter(x, names(on[5]), toupper)
  y <- names(x(5))
})

test_that("inject() sets a subelement of a value to the chain of the value", {
  x <- inject(1:10, names, letters[], rev)
})

test_that("with_attrs is a list of a thing and its attributes", {
  
})

test_that(paste("rebind() is a functional equiv. of bind(), "
                "outputting a list/data frame of the same class?"), {
  
})

test_that("`on` used as placeholder target put/alter/inject target (if computed value)", {
  x <- put(1:10, names(on(1:10)), letters(1:10))
  expect_equal(x, structure(1:10, names=letters(1:10)))

  y <- alter(names(on(rev(x))), toupper)
  expect_equal(y, structure(10:1, names=letters(10:1)))
})



test_that("via<-(x), sets x to chain of its arguments", {
  x <- 1:5
  via(names[x[5]], toupper) <- letters[5]
  expect_equal(x, c(1,2,3,4,E=5))
})

test_that("check() chains a value through conditions and throws errors", {
  structure(1:5, names=letters(1:5))
})

test_that("checked<-() asserts that the provided value passes a chain with TRUE", {
  check(x, names, !any(.="")) <- structure(1:5, names=letters(1:5))
  expect_equal(x, structure(1:5, names=letters(1:5)))
  expect_error( check(x, .>0, all) <- c(1, -1) )
})

test_that("default<- lets you provide a default value from bind[]", {
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

test_that("default<- allows there to be missing matches as well", {
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
