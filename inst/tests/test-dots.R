context("dots")

`%is%` <- expect_equal

##Quickie macro to help with setup and teardown.
with_setup <- macro(JIT=FALSE, function(setup=NULL, ..., teardown=NULL) {
  template({
    ...( lapply(list(...), function(x) template({
      .(setup)
      .(x)
      .(teardown)
    })))
  })
})

## DOTSXP UNPACKING --------------------------------------------------

test_that("dots_unpack() method extracts dots information into a data frame", {
  f <- function(...) {
    list(dots_unpack(...), environment())
  }
  x <- 2
  y <- 3
  bind[di, env] <- f(x, y=3, z=x+y)
  env <- environment()

  expect_identical(di$expr[[1]], quote(x))
  expect_identical(di$expr[[2]], quote(3))
  expect_identical(di$expr[[3]], quote(x+y))
  expect_identical(di$env[[3]], env)
  expect_identical(di$env[[3]], env)
  expect_identical(di$env[[3]], env)
  expect_identical(di$value[[1]], NULL)
  expect_identical(di$value[[2]], NULL)
  expect_identical(di$value[[3]], NULL)
  expect_identical(di$name[[1]], "")
  expect_identical(di$name[[2]], "y")
  expect_identical(di$name[[3]], "z")
})

test_that("dots_unpack(...) exposes promise behavior", {
  a <- 12
  b <- a+2
  unpack_fns <- function(...) {
    #get functions that to things to the same dotslist
    list(
      function() dots_unpack(...),
      function() (function(x, ...) x)(...),
      function() list(...),
      environment()
      )}
  outer_env <- environment()
  bind[reunpack, eval_x, eval_all, inner_env] <- unpack_fns(x=a, y=a+2)

  du <- reunpack()
  expect_identical(du$value[[1]], NULL)
  expect_identical(du$env[[1]], outer_env)
  eval_x()
  du2 <- reunpack()
  expect_identical(du2$value[[1]], 12)
  expect_identical(du2$envir[[1]], NULL)
  expect_identical(du2$envir[[2]], outer_env)
  expect_identical(du2$value[[2]], NULL)
})

test_that("dots_unpack has a print method that works", {
  capture.output(dots_unpack(a, b, c, d, 4, e)) #should go without error
})

test_that("dots_unpack(...) descends through promise chains if necessary", {
  y <- 1
  f1 <- function(...) {
    x <- 1
    list(getdots(y=x+1, ...), environment())
  }
  getdots <- function(...) dots_unpack(...)

  bind[du, f1_env] <- f1(a=y+z)

  expect_identical(du[["a", "envir"]], environment())
  expect_identical(du[["y", "envir"]], f1_env)
  expect_identical(du[["a", "expr"]], quote(y+z))
  expect_identical(du[["y", "expr"]], quote(x+1))
})

## these should also be in reference to dots objects
test_that("dots_missing", {
  with_setup(
    setup={
      if (exists("a")) rm(a)
      unmissing <- 1
      b <- missing_value()
    },
    #test both the dots_missing form and the is.missing.... form
    thunk <- dots_missing,
    thunk <- function(...) is.missing....(dots(...)),
    #actual testing in the teardown
    teardown={
      expect_equal(c(   FALSE, FALSE,     c=TRUE, FALSE, d=FALSE, TRUE),
                   thunk(   a, unmissing, c=,     4,     d=x+y,       ))

      #this currently (R 2.15.2) answers "b" differently in some cases.
      #My opionion is this is a bug in R, so don't check right now.
      ## wrap <- function(...) {
      ##   thunk(...)
      ## }
      ## #                                   *WHAT*
      ## expect_equal(c(   FALSE, FALSE,     FALSE, c=TRUE, FALSE, d=FALSE, TRUE),
      ##              wrap(    a, unmissing, b,     c=,     4,     d=x+y,       ))

      #And this check for missingness does not eval
      expect_equal(c(FALSE, c=TRUE, FALSE),
                   thunk(stop("no"), c=, stop("no")))
      rm(unmissing)
      rm(b)
    })})

test_that("dots_names", {
  expect_equal(c("", "", "c", "", "d", ""),
               dots_names(a, b, c=, 4, d=x+y, ) )

  #and dots_names does not eval dots
  expect_equal(c("", "a"),
               dots_names(stop("no"), a=stop("no")))
})

test_that("is.missing on non-dotlists", {
  a <- alist(1, 2, adsf, , b=, )
  is.missing(a) %is% c(FALSE, FALSE, FALSE, TRUE, b=TRUE, TRUE)
  b <- c(1, 2, NA, NaN)
  is.missing(b) %is% c(FALSE, FALSE, FALSE, FALSE)
  is.missing() %is% TRUE
  is.missing(function(x) y) %is% FALSE
})

## DOTS OBJECT, CALLING AND CURRYING -------------------------------------

test_that("%()% is like do.call(quote=TRUE) but doesn't overquote", {
  x = 2
  y = 5

  ff <- function(x, y) list(substitute(x), substitute(y))

  list %()% list(x, y) %is% list(2,5)
  list %()% alist(x, y) %is% ff(x, y)
  list %()% ff(x, y+z) %is% ff(x, y+z)
  ff %()% ff(x, y) %is% ff(x, y)
  ff %()% list(x,y) %is% ff(2, 5)
 })

test_that("x <- dots() captures dots and %()% calls with dots", {
  x <- 1;
  y <- 3;
  f <- `/`
  d <- dots(y=x, 4)
  f %()% d %is% 0.25
})

test_that("%()% and %<<% on vectors respects tags", {
  paste %()% c(sep="monkey", 1, 2, 3) %is% "1monkey2monkey3"
  c %<<% c(a=1) %()% c(b=2) %is% c(b=2, a=1)
  c(a=1) %>>% c %()% c(b=2) %is% c(a=1, b=2)
})

test_that("curr and curl", {
  #these are versions that don't
  f = curr(`/`, x)
  expect_error(f(5))
})

test_that("curry DTRT with original scope of its arguments", {
  with_setup(
    setup={
      g <- function(...) {
        x <- "this is not in f"
        thunk(c, ...)
      }

      f <- function(...) {
        x <- "this is in f"
        g(this_x_should_be_scoped_in_f = x, ...)
      }
    },
    #for each variant of curry
    thunk <- curl,
    thunk <- curr,
    thunk <- function(f, ...) f %<<% dots(...),
    thunk <- function(f, ...) dots(...) %>>% f,
    #the actual test is in the teardown...
    teardown={
      f()() %is% c(this_x_should_be_scoped_in_f = "this is in f")
      length(f(a=1)()) %is% 2
    }
)})

test_that("as.dots() converts expressions to dotslists w.r.t. a given env", {
  x <- 3
  f1 <- function(l) {
    x <- 1
    as.dots(l)
  }
  f2 <- function(l) {
    x <- 2
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
    setup={
      bind[w, x, y, z] <- c(2, 3, 4, 5)
      d <- dots(w+x)
      dd <- dots(w+x, y+z)
      f <- `*` %<<% d
      f2 <- `*` %<<% dd
    },
    {
      f2() %is% 45
      x <- 4
      f(y+z) %is% 54
    },
    {
      x <- 4
      f2() %is% 54
      f(2) %is% 12
      x <- 3
      (f %()% d) %is% 36
    },
    {
      #left-curry applies to the left of the arglist
      f2 <- dots(w+x) %>>% `/`
      x <- 2
      f2(2) %is% 2
      x <- 3
      f2(2) %is% 2
    })
})

test_that("Curry operators concatenate dots, dots stay attached to envs", {
  with_setup(
    setup={
      envl <- list2env(structure(as.list(letters), names=letters))
      envu <- list2env(structure(as.list(LETTERS), names=letters))
      envn <- list2env(structure(as.list(1:10), names=letters[1:10]))
      l <- evalq(dots(a, b, c), envl)
      u <- evalq(dots(a, b, c), envu)
      n <- evalq(dots(a, b, c), envn)
      P <- paste %<<% list(sep="")
    },
    P  %()%  l  %is%  "abc",
    P  %()%  u  %is%  "ABC",
    P  %()%  n  %is%  "123",
    #these two cases are bothersome.
    P  %<<%  l  %<<%  u  %()%  n  %is%  "123ABCabc", #this is not intuitive
    l  %>>% (u  %>>%  P) %()%  n  %is%  "ABCabc123", #this also counterintuitive...
    u  %__%  l  %>>%  P  %()%  n  %is%  "ABCabc123",
    l  %>>%  P  %<<%  u  %()%  n  %is%  "abc123ABC",
    u  %__%  l  %>>%  P  %()%  n  %is%  "ABCabc123"
    )
})

test_that("%__% with mixed sequence types", {
  with_setup(
    setup={
      x <- "a"; y <- "b"; z <- "c"
      a <- dots(x, y, z)
      b <- LETTERS[4:6]
    },
    paste0 %()% (a %__% b) %is% "abcDEF",
    paste0 %()% (b %__% a) %is% "DEFabc",
    {x <- "_"; paste0 %()% (b %__% a) %is% "DEF_bc"},
    {x <- "_"; paste0 %()% (a %__% b) %is% "_bcDEF"}
    )
})

test_that("dots has some kind of print method", {
  d <- dots(a, b, c)
   capture.output(print(d))
})

test_that("dots() et al with empty inputs", {
  #note that there isn't such a thing as an empty dotslist, and this
  #(a) complicates evaluating "..." etc, and (b) complicates making a
  #dotslist the basis of the class (as it will have to be something
  #else to match a zero value.
  #So test variants of dots apply, curry, and cdots, with empty dotslists.
  f <- function(x=4, y=2) x * y
  a <- dots()
  b <- as.dots(c())
  c <- list(1);
  d <- dots(2);

  f %()% a %is% 8
  f %()% b %is% 8
  (a %>>% f)() %is% 8
  (f %<<% b)() %is% 8
  f %()% (b %__% a) %is% 8
  (f %<<% list())() %is% 8
  (list() %>>% f)() %is% 8
  f %()% (c %__% list()) %is% 2
  f %()% (list() %__% d) %is% 4
  f %()% (a %__% c) %is% 2
  f %()% (c %__% a) %is% 2
  f %()% (a %__% d) %is% 4
  f %()% (d %__% a) %is% 4
})


test_that("dots [] operator subsets without forcing promises", {
  with_setup(
    setup= {
      a <- dots(x, r=y, x+y)
      x <- 3
      y <- 4
    }, {
      c %()% a[1:2] %is% c(3,r=4)
      x <- 4
      c %()% a[3] %is% 8
      y <- 2
      c %()% a %is% c(3,r=4,8)
    }, {
      c %()% a[2:3] %is% c(r=4, 7)
      x <- 2
      c %()% a %is% c(2,r=4,7)
    }, {
      c %()% a["r"] %is% c(r=4)
    }
    )
})

test_that("[<-.... replacement operator can take values from another dotsxp", {
  #should be able to replace items of a dotslist with items from
  #another dotslist. Non-dotslists should error.
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

test_that("dots [[]] and $ operators force ONE promise and return the value.", {
  with_setup(
    setup={
      x <- 2; y <-3
      d <- dots(a=x, b=y, c=x+y)
    },
    {
      d[[2]] %is% 3
      x <- 1
      d[[1]] %is% 1
    },
    {
      x <- 4
      d$c %is% 6
      x <- 3
      d[["a"]] %is% 3
    }
    )
})

test_that("dots names method extracts tags without forcing", {
  names(dots(a, b, c=, 4, d=x+y, )) %is% c("", "", "c", "", "d", "")
  names(dots(stop("no"), a=stop("no"))) %is%  c("", "a")
})

test_that("dots names<- method can set tags w/o forcing", {
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

