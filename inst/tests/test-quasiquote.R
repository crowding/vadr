context("quasiquotation")

`%is%` <- expect_equal

test_that("quasiquote", {
  #"qq" is intended to be a stronger version of backquote.

  a <- quote(a+b)
  b <- quote(b+c)
  c <- quote(c+d)
  default = quote(a+z)

  expect_equal(qq( .(a) + .(b)    + .(c)),
               quote(   a + b + (b + c) + (c + d)))

  expect_equal(qq( .(a) + b + .(qq( .(a) + b ) ) ),
               quote(   a + b + b + (a + b + b) ) )

  expect_equal( qq( function(a, b = .(default)) force(b) ),
                quote(    function(a, b = a + z     ) force(b) ))
})

test_that("qq interpolation in names", {
  achar <- "a"
  aname <- quote(a)
  bchar <- "b"

  expect_equal(qq( list(".(achar)"=foobar) ),
                  quote( list(         a=foobar) ))
  expect_equal(qq( list(`.(aname)`=baz   ) ),
                  quote( list(         a=baz   ) ))
  expect_equal(qq( list(`.(paste(achar, bchar, sep=""))` = foobar ) ),
                  quote( list(                              ab = foobar ) ))
  #also in formal argument lists.
  expect_equal(qq( function( `.(achar)`, `.(bchar)` = default) body ),
                  quote( function(          a,          b = default) body ))

  #thius also covers some situations where nothing other than a name
  #is allowed by the parser
  expect_equal(qq(a$`.(bchar)`),
                  quote( a$b ))
  expect_equal(qq( for(`.(aname)` in seq_len(10)) NULL),
               quote(    for(         a in seq_len(10)) NULL));
})

test_that("multiple element qq interpolation", {
  arglist <- alist(aa, bb, cc)
  namedlist <- alist(aa=a, bb=b, ccc=)

  expect_equal(qq( list(z, b, ...(arglist)) ),
                  quote( list(z, b, aa, bb, cc) ) )
  expect_equal(qq( list(z, ...(namedlist), q) ),
                  quote( list(z, aa=a, bb=b, ccc=, q) ))
  #and in argument lists. Note how ccc is on the name on the right side.
  #note that the name of the argument ... appears in is ignored.
  expect_equal(qq( function(q, .=...(namedlist), x) body ),
                  quote( function(q, aa=a, bb=b, ccc, x) body ))
})

test_that("qq interpolation in first argument", {
  #qq() wasn't hitting the first element of function arg lists?
  #turns out bquote() itself had this bug too.
  argument.name <- "x"
  expect_equal(
    qq(function(`.(argument.name)`) {
      cat(.(argument.name), " is ", `.(argument.name)`, "\n")
    })
    ,
    quote( function(x) {cat("x", " is ", x, "\n")})
    )

  argnames <- letters[1:4]
  expect_equal(
    qq(function(
                .=...(setNames(missing_value(length(argnames)), argnames)))
             {
               list(.=...(lapply(argnames, as.name)))
             } )
    ,
    quote( function(a, b, c, d) { list(a, b, c, d) } )
    )
})

test_that("qqs with ...(NULL)", {
  qq(list(1, 2, ...(NULL), 4)) %is% quote(list(1, 2, 4))
})

test_that("quasiquote descends into heads of calls,", local({
  qq( (.(as.name("list")))(1, 2, 3) ) %is% quote( (list)(1, 2, 3) )

  tempfun <- function() {
    qq((function() {...(list(1, 2, 3))})())
  }

  #the real problem here was call objects of length 1
  expect_equal(
    tempfun(),
    quote( (function() {1; 2; 3})() ))

}))

test_that("quasiquote non-call, non-primitive lists", {
  expect_equal(
    do.call(qq, list(alist(a, b, .(paste("foo", "bar")), d))),
      alist(a, b, "foo bar", d))

  expect_equal(
      do.call(qq, list(alist(function(){test(quote(.(2+2)))}))),
      list(quote(function() {test(quote(4))})))
})

test_that("qe(x) is a shortcut for eval(qq(x))", {
  x <- 1; y <- 2; z <- 3;
  foo <- c("x", "y", "z")
  qe(sum(...(lapply(foo, as.name)))) %is% 6
  qe(function(.=...(list(a=1))) `.("a")`)() %is% 1
})

test_that("unquote strips srcrefs off 'function' ", {
  x <- qe(function(.=...(list(a=1))) `.("a")`)
  attr(x, "srcref") %is% NULL
})

test_that("qqply(expr)(args) substitutes args into expr", {
  f <- qe(function(
      .=...( qqply(`.(..1)`=.(..2))(letters, 1:26))) {
    ...(qqply(.(as.name(x)) <- .(as.name(y)))(y=letters[2:26], x=letters[1:25]))
    e
  })
  f() %is% 6
})

test_that("qeply() like qqply but evaluates each one", {
  local({
    xxx <- qeply(`.(x)`<-.(y))(x=letters, y=1:26)
    e %is% 5
    xxx[[6]] %is% 6
  })
})

test_that("qqply computes names for its output", {
  x <- qqply(`.(x)` = .(y))(x=letters[1:3], y=1:3)
  x %is% list(a =1, b=2, c=3)
})
