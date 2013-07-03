context("bind")

`%is%` <- expect_equal

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

test_that("bind a data frame with ...", {
  x <- data.frame(a=c(1,2), b=c("one", "two"), c=c("uno", "dos"))
  bind[c=spanish, b=english, ...=rest] <- x
})

test_that("bind happens left to right in bind arguments", {
  a <- c(1,2,3)
  bind[y=a[1:2], z=a[2:3], x=a[c(1,3)]] <-
    list(z=c(10, 20), y=c(40, 60), x=c(80, 100))
  expect_equal(a, c(80, 10, 100))
})

test_that("bind can ignore a 'rest' argument", local({
  bind[a=x, ...=, c=y] <- list(a="foo", b="bar", c="baz", d="qux")
  expect_equal(x, "foo")
  expect_equal(y, "baz")
  expect_false(exists("..."))
}))

test_that("bind captures rest", {
  bind[a=x, ...=foo, b=y] <- list(a="foo", b="bar", c="baz", d="qux")
  expect_equal(x, "foo")
  expect_equal(y, "bar")
  expect_equal(foo, list(c="baz", d="qux"))
})

test_that("bind rest capture is positional", {
  bind[first, ...=rest] <- 1:10
  first %is% 1; rest %is% 2:10

  bind[...=rest, last] <- 1:10
  rest %is% 1:9; last %is% 10

  bind[first, ...=, last] <- 2:20
  first %is% 2; last %is% 20

  bind[, ...=middle,] <- 1:10
  middle %is% 2:9
})

test_that("bind works with =", {
  #ah, yes, '=' is a distinct operator in R
  bind[a,b] = c(1,2)
  a %is% 1; b %is% 2
})

## This doesn't seem to have a possibility of working. It dispatches to [<-.
## test_that("bind works with <<-", {
##   # (for god-knows-what reason you might want to do that)
##   x <- 1
##   y <- 2
##   local({
##     bind[x, y] <<- c(3, 4)
##     ls() %is% character(0)
##   })
##   x %is% 3
##   y %is% 4
## })

##I'd like to make it pass this but how?
## test_that("using bind does not create a var named bind", local({
##   bind[x] <- 1
##   print(ls())
##   ls() %is% "x"
## }))

test_that("bind ellipsis capture preserves vector type", {
  bind[a=x, ...=foo, b=y] <- list(a="foo", b="bar", c="baz", d="qux")
  expect_equal(x, "foo")
  expect_equal(y, "bar")
  expect_equal(foo, list(c="baz", d="qux"))
  bind[a=x, ...=foo, b=y] <- c(a="foo", b="bar", c="baz", d="qux")
  expect_equal(foo, c(c="baz", d="qux"))
})

test_that("bind complains of unmatchable or unmatched arguments", {
  expect_error( bind[a=x, b=y] <- c(a=1,b=2,c=3) )
  expect_error( bind[w,x,y,z] <- c(1,2,3,4,5) )
  expect_error( bind[a=x, b=y] <- c(a=1, r=2) )
  expect_error( bind[a=x, b=y] <- c(a=1, r=2) )
  expect_error( bind[a=x, b=y, ...=rest] <- c(a=1, c=2, d=3, e=4) )
})

test_that("bind name matching behavior", {
  `%is%` <- expect_equal

  bind[a, b] <- c(a=100, b=200)
  a %is% 100; b %is% 200

  bind[a, b] <- c(b=34, a=56)
  a %is% 34; b %is% 56

  bind[a, b=b] <- c(b=78, a=90)
  a %is% 90; b %is% 78

  bind[b=a, b] <- c(a=123, b=456)
  a %is% 456; b %is% 123

  #this is interesting, the dots argument remembers names.  I wonder
  #if it's possible for other arguments to remember names, could do
  #that for binding a vector, but seem not possible for unbinding a
  #list.
  bind[...=a, b=b] <- c(a=98, b=76)
  a %is% c(a=98); b %is% 76
})

test_that("bind ignores a variable", {
  local({
    bind[a=x, b= , c=c] <- list(a="foo", b="bar", c="baz")
    x %is% "foo"; c %is% "baz"
    expect_false(exists("b"))

    ##check that bind does not assign to the empty name!!!
    expect_false("" %in% ls())
  })
  local({
    bind[x, , baz] <- list(a="foo", b="bar", c="baz")
    x %is% "foo"; baz %is% "baz"
    expect_false("c" %in% ls())
  })
})

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

test_that("bind works with data frames", {
    bind[lat=lat, long=long, ...=df] <- quakes
    expect_equal(quakes$lat, lat)
    expect_equal(quakes$long, long)
})

test_that('bind works with lists of language objects', {
    l <- alist(`_data`, 5, 6, 7)
    bind[arg, ...=args] <- l
    expect_equal(arg, quote(`_data`))
    expect_equal(args, list(5, 6, 7))
})

test_that('bind works with pairlists and missing values', {
    l <- quote(function(a=foo, b, c) {foo})
    bind[ , bind[a=a_default, ...=], ...=] <- l
    expect_equal(a_default, quote(foo))
    #should this error? I claim not because
    #x <- alist(foo=, bar, baz)
    #tf <- x$foo
    #does not error.
    #
    #On the other hand, it would be nice to be able to treat defaults
    #like they ought to be treated. This would require treatment of missings...
    bind[ , bind[b=b_default, ...=], ...=] <- l
    expect_identical(list_missing(b_default), list(quote(e=)))
})

FALSE && test_that("bind works with dots objects", {
    #no guarantees on what order they are extracted in though.
    x <- 1
    y <- 2
    ex <- dots(a = x[2] <- x+y, b = y[2] <- y+x[2], aa=1, bb=2)
    bind[a=a, b=b, ...=q] <- ex
    b <- ex$b
    expect_equal(a, c(3))
    expect_equal(b, c(5))
    expect_equal(x, c(1,3))
    expect_equal(y, c(2,5))
    expect_equal(c %()% q, c(aa=1, bb=2))
})

# some interesting test cases to think about / play with.
# bind[names=colnames, colnames=row.names, ...] <- attributes(data.frame(a=1))
# bind[a, names(a)] <- list(names(a), names(a))
# bind[a=x, aa, ...=foo, b=y, zz] <- c(a="foo", b="bar", c="baz", d="qux", "quux", "quux", "quuux", "grauply")

