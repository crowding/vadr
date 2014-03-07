context("LRU cache")

test_that("cache stores values", {
  store <- lru_cache()
  store("foo", 1)
  store("bar", 2)
  store("baz", 3)
  expect_equal(1, store("foo", stop("should not be evaluated")))
  expect_equal(2, store("bar", stop("should not be evaluated")))
  expect_equal(3, store("baz", stop("should not be evaluated")))
  expect_equal(2, store("bar", 4))
})

test_that("cache incrementally expires old values", {
  store <- lru_cache(3)
  store("foo", 1)
  store("bar", 2)
  store("baz", 3)
  store("qux", 4)
  expect_equal(4, store("qux", stop("should not be evaluated")))
  expect_equal(3, store("baz", stop("should not be evaluated")))
  expect_equal(2, store("bar", stop("should not be evaluated")))
  expect_equal(100, store("foo", 100))
})

test_that("cache expires least recently accessed values", {
  store <- lru_cache(3)
  store("foo", 1)
  store("bar", 2)
  store("baz", 3)

  #from end of list
  expect_equal(1, store("foo", stop("should not be evaluated")))
  expect_equal(4, store("qux", 4))
  expect_equal(100, store("bar", 100))

  #from middle of list
  expect_equal(4, store("qux", stop("should not be evaluated")))
  expect_equal(200, store("baz", 200))
})
