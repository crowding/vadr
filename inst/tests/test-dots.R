context("Dots")

`%is%` <- expect_equal

##Quickie macro to help with setup and teardown.
##this is also an example of problematic autoindent in the Emacs mode...
with_setup <- macro(JIT=FALSE, function(setup=NULL, ..., teardown=NULL) {
  template({
    ...( lapply(list(...), function(x) template({
      .(setup)
      .(x)
      .(teardown)
    }))
        )
  })
})

## DOTSXP UNPACKING --------------------------------------------------

test_that("unpack(dots(...)) descends through promise chains if necessary", {

})

test_that("dots unpack() method extracts dots information", {

})

test_that("unpack(dots(...)) unpacks a dotslist and exposes promise behavior", {

  evalX <- function(x, ...) {
    force(x)
    do_describe(yy=x, ...)
  }

  do_dots <- function(...) {
    dots_info(...)
  }

  test <- do_dots(x=1+2, y=z, z=2)

})

## these should also be in reference to dots objects

test_that("dots_missing", {
  expect_equal(c(FALSE, FALSE, c=TRUE, FALSE, d=FALSE, TRUE),
               dots_missing(a, b, c=, 4, d=x+y, ) )

  checkMyDots <- function(...) {
    missing <- dots_missing(...)
  }

  expect_equal(c(FALSE, FALSE, c=TRUE, FALSE, d=FALSE, TRUE),
               checkMyDots(a, b, c=, 4, d=x+y, ) )

  #but it doesn't eval
  expect_equal(c(FALSE, c=TRUE, FALSE),
               checkMyDots(stop("no"), c=, stop("no")))
})

test_that("dots_names", {
  expect_equal(c("", "", "c", "", "d", ""),
               dots_names(a, b, c=, 4, d=x+y, ) )

  #and dots_names does not eval dots
  expect_equal(c("", "a"),
               dots_names(stop("no"), a=stop("no")))
})

## DOTS OBJECT, CALLING AND CURRYING -------------------------------------

test_that("%()% has literal-value-passing semantics, unlike do.call", {
  ## notwithstanding any nonstandard evaluation a thing might do.
  x = 2
  y = 5

  list %()% c(x, y) %is% list(2,5)
  list %()% list(x, y) %is% list(2,5)
  list %()% alist(x, y) %is% alist(x, y)
  alist %()% alist(x, y)
  alist %()% alist(x, y) %is% alist(x, y) #really?
  alist %()% list(x,y) %is% alist(2, 5)
})

test_that("x <- dots() captures dots and %()% calls with dots", {
  x <- 1;
  y <- 3;

  f <- `/`

  d <- dots(y=x, 4)
  expectEqual( f %()% d, 0.5 )
})

test_that("as.dots() converts expressions to dotslists w.r.t. a given env", {
  x <- 3

  f1 <- function(l) {
    x <- 1
    as.dots(l)
  }

  f2 <- function(l) {
    x
    as.dots(l)
  }

  c %()% f1(alist(x)) %is% 1
  c %()% f2(alist(x)) %is% 2
  c %()% as.dots(alist(x)) %is% 3
})

test_that("as.dots() is idempotent on dots objects", {
  x <- 3
  l <- as.dots(alist(x))
  f <- function(l) {
    x <- 4
    as.dots(l)
  }
  l <- f(l)
  x <- 5
  c %()% l %is% 5
})

test_that("Curried dots evaluate like promises", {
  with_setup(
    setup={ bind[w, x, y, z] <- c(2, 3, 4, 5)
            d <- dots(w+x, y+z)
            f <- `*` },
    { f2 <- f %<<% d
      f2() %is% 45
      x <- 4
      f2() %is% 45
      (f %()% d) %is% 54 },
    { f <- f %<<% d
      x <- 4
      f2() %is% 54
      x <- 3
      (f %()% d) %is% 54 },
    { #left-curry applies to the left of the arglist
      f2 <- dots(w+x) %.>% `/`
      x <- 2
      f2(2) %is% 2
      x <- 3
      f2(2) %is% 2 }
    )
})

test_that(paste("Curry operators concatenate dots, dots stay attached to envs"), {
  with_setup(
    setup={ envl <- list2env(structure(as.list(letters), names=letters))
            envu <- list2env(structure(as.list(letters), names=LETTERS))
            envn <- list2env(structure(as.list(1:10)), names=letters[1:10])
            l <- evalq(dots(a, b, c), envl)
            u <- evalq(dots(a, b, c), envu)
            n <- evalq(dots(a, b, c), envn)
            P <- paste %<<% list(sep="") },
    P  %()%  l  %is%  "abcdef",
    P  %()%  u  %is%  "ABCDEF",
    #these two cases are bothersome.
    P  %.<%  l  %.<%  u  %()%  n  %is%  "123ABCabc", #this is not intuitive
    l  %>>% (u  %>>%  P) %()%  n  %is%  "ABCabc123", #this also counterintuitive...
    u  %++%  l  %>>%  P  %()%  n  %is%  "ABCabc123",
    l  %>>%  P  %.<%  u  %()%  n  %is%  "abc123ABC",
    u  %++%  l  %>>%  P  %()%  n  %is%  "ABCabc123"
    )
})


test_that("dots [] operator subsets without evaluating []", {
  with_setup(
    setup= {
      a <- dots(x, r=y, x+y)
      x <- 3
      y <- 4},
    { c %()% a[1:2] %is% c(3,r=4)
      x <- 8
      c %()% a[3] %is% 8
      y <- 2
      c %()% a %is% c(3,r=4,8)},
    { c %()% a[2:3] %is% c(4, 7)
      x <- 2
      c %()% a %is% c(2,r=4,7) },
    { c %()% a["r"] %is% c(r=4)
      x <- 4
      c %()% "r" %is% c(r=4)
    }
    )
})

test_that("[<-.... replacement operator, even", {
  #should be able to replace items of a dotslist with items from
  #another dotslist (or ordinary sequence.)
  with_setup(
    setup={
      x <- 2; y<-3;
      d <- dots(a=x, b=y, c=x+y) },
    {
      d[2] <- 10
      y <- 4
      c %()% d %is% c(a=2, b=10, c=7) },
    {
      d["a"] <- dots(x*y)
      x <- 5
      c %()% d %is% c(a=15, b=3, c=8)
    })
})

test_that("dots [[]] and $ operators go ahead and evaluate. (?)", {
  stop("test not written")
})

test_that("dots names method extracts tags", {
  names(dots(a, b, c=, 4, d=x+y, )) %is% c("", "", "c", "", "d", "")
  names(dots(stop("no"), a=stop("no"))) %is%  c("", "a")
})

test_that("dots names<- method can set tags w/o disturbing promise eval", {
  with_setup(
    setup={
      x <- 2; y<-3;
      d <- dots(a=x, b=y, c=x+y) },
    {
      names(d) <- c("foo", "bar", "baz")
      y <- 4
      c %()% d %is% c(foo=2, bar=10, baz=7) }
    )
})

