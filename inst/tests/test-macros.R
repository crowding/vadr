context("macros")

`%is%` <- expect_equal

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

test_that("quoting.env and missings", {
  en <- quoting.env(c('[', '<-', 'a', 'b', 'c'))
  expect_equal(evalq(a[1, ] <- b[, 2], en),
               quote(a[1, ] <- b[, 2]))
})

test_that("macro() turns a lexical substitutor function into a macro", {
  d <- function(expr_b) {
    call("+", expr_b, expr_b)
  }

  double <- macro(d)
  expect_equal(double(5), 10)

  x <- 5
  side_effect <- function(){
    x <<- x+1
  }
  expect_equal(double(side_effect()), 13)

  expect_true("macro" %in% class(double))

  expect_equal(attr(double, "orig"), d)
})

test_that("macro cache pays attention to tags", {
  divmacro <- macro(function(a,b) template(.(a)/.(b)))

  expect_equal(divmacro(10, 5), 2)
  expect_equal(divmacro(5, 10), 0.5)
  expect_equal(divmacro(a=10, b=5), 2)
  expect_equal(divmacro(b=10, a=5), 0.5)
  expect_equal(divmacro(b=5, a=10), 2)
})

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

test_that("expand_macro expands all visible macros (by one step)", {
  local({
    addmacro <- macro(function(x, y) qq(.(x) + .(y)))
    doublemacro <- macro(function(x, y) qq(.(x) * addmacro(.(y), .(y))))
    #
    expect_equal(expand_macros(quote(addmacro(a, b))), quote(a+b))
    expect_equal(expand_macros_q(addmacro(a, b*y)), quote(a+b*y))
    expect_equal(expand_macros_q(doublemacro(a, b)), quote(a * addmacro(b, b)))
    #macros are expanded from the top down.
    expect_equal(expand_macros_q(addmacro(a, addmacro(b,c))),
                 quote(a+addmacro(b,c)))
    expect_equal(expand_macros_q(addmacro(a, addmacro(b,c)), recursive=TRUE),
                 quote(a+(b+c)))
  })
})

test_that("quote_args", {
  # Function that quotes arguments "like an argument list", returning
  # a pairlist.

  quote_args(a=1, b=y, c=x+y) %is% as.pairlist(alist(a=1, b=y, c=x+y))
  quote_args(a=1, b, c) %is% as.pairlist(alist(a=1, b=, c=))
  expect_error(quote_args(a, b, x+y))
  expect_error(quote_args(a, b, 1))
  expect_error(quote_args(a, , c))
})

test_that("with_arg", {
  (with_arg(a=2, b=3, list(4), list(5))
   %is% list(list(4, a=2, b=3), list(5, a=2, b=3)))

  x <- 1; y <- 2
  (with_arg(a=x+y, list(1), alist(1))
   %is% list(list(1, a=3), alist(1, a=x+y)))

  (with_arg(.collect=c, a=1, b=2, c(1, 2), c(1))
   %is% c(1, 2, a=1, b=2,1, a=1, b=2))
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

