test_that("bind of a list", {
  #Assign to parallel variables!"
  bind[a,b] <- list(1, 2)
  expect_equal(a, 1)
  expect_equal(b, 2)
})

test_that("bind of a vector", {
  bind[x, y] <- c("foo", "bar")
  expect_equal(x, "foo")
  expect_equal(y, "bar")
})

test_that("bind named list", {
  #Yes, it is somewhat awkward to have the assigned-to variables on the
  #right sides of the equals, but that's how it best parallels
  #R's argument-binding syntax
  bind[a=x, b=y] <- list(a="foo", b="bar")
  expect_equal(x, "foo")
  expect_equal(y, "bar")
})

test_that("bind complicated lvalues", {
  a <- c(1,2)
  bind[a[1], a[2]] <- list(20, 30)
  expect_equal(a, c(20, 30))
})

test_that("bind happens left to right in bind arguments", {
  a <- c(1,2,3)
  bind[y=a[1:2], z=a[2:3], x=a[c(1,3)]] <-
    list(z=c(10, 20), y=c(40, 60), x=c(80, 100))
  expect_equal(a, c(80, 10, 100))
})

test_that("bind ignores ellipses", local({
  bind[a=x, ..., c=y] <- list(a="foo", b="bar", c="baz", d="qux")
  expect_equal(x, "foo")
  expect_equal(y, "baz")
  expect_false(exists("..."))
}))

test_that("bind captures ellipses", {
  bind[a=x, ...=foo, b=y] <- list(a="foo", b="bar", c="baz", d="qux")
  expect_equal(x, "foo")
  expect_equal(y, "bar")
  expect_equal(foo, list(c="baz", d="qux"))
})

test_that("bind ellipsis capture preserves vector type", {
  bind[a=x, ...=foo, b=y] <- list(a="foo", b="bar", c="baz", d="qux")
  expect_equal(x, "foo")
  expect_equal(y, "bar")
  expect_equal(foo, list(c="baz", d="qux"))
  bind[a=x, ...=foo, b=y] <- c(a="foo", b="bar", c="baz", d="qux")
  expect_equal(foo, c(c="baz", d="qux"))
})

test_that("bind ignores a variable", local({
  bind[a=x, b= , c] <- list(a="foo", b="bar", c="baz")
  expect_false(exists("b"))
  bind[x, ,baz] <- list(a="foo", b="bar", c="baz")
  expect_equal(x, "foo")
  expect_equal(x, "bar")
  expect_equal(sort(ls()), c("baz", "c", "x"))
}))

test_that("bind works recursively", {
  bind[a=x, b=bind[xx, yy]] <- list(a="foo", b=c("bar", "baz"))
  expect_equal(x, "foo")
  expect_equal(xx, "bar")
  expect_equal(yy, "baz")
})

test_that("bind works recursively with ellipses", {
  bind[a=x, ...=bind[aa=xx, bb=yy], b=y] <-
    list(a=1, b="two", aa="eleven", bb=22)
  expect_equal(x, 1)
  expect_equal(y, "two")
  expect_equal(xx, "eleven")
  expect_equal(yy, 22)
})

# bind[names=colnames, colnames=row.names, ...] <- attributes(data.frame(a=1))
# bind[a, names(a)] <- list(names(a), names(a))
