context("macros")

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

test_that("list_with_missing", {
  expect_equal(list_with_missing(1, 2, 3),
               list(1,2,3))

  expect_equal(list_with_missing(1, 2, , "three"),
               alist(1, 2, , "three"))

  expect_equal(list_with_missing(a="one", b=, "three"),
               alist(a="one", b=, "three"))
})

test_that("list_with_missing evaluates arguments in the original scopes", {
  fOne <- function(...) {
    fThree <- function(...) {
      x <- "three"
      list_with_missing(..., three=x)
    }
    fTwo <- function(...) {
      x <- "two"
      fThree(..., two=x)
    }
    x <- "one"
    fTwo(..., one=x)
  }

  x <- "four"
  expect_equal(fOne(four=x),
               list(four="four", one="one", two="two", three="three"))
})

test_that("quoting.env and missings", {
  en <- quoting.env(c('[', '<-', 'a', 'b', 'c'))
  expect_equal(evalq(a[1, ] <- b[, 2], en),
               quote(a[1, ] <- b[, 2]))
})

test_that("macro() turns a lexical substitutor function into a macro", {
  d <- function(expr_a, expr_b, expr_c) {
    as.call(as.name("+"), b, b)
  }

  double <- macro(d)
  expect_equal(double(5), 10)
  x <- 5

  side_effect <- function(){
    x <<- x+1
  }
  expect_equal(double(side_effect(), 13))

  #
  expect_that("macro" %in% class(double))

  expect_equal(getAttribute(macro, "orig"), double)
})

test_that("template", {
  #"template" is intended to be a stronger version of backquote.

  a <- quote(a+b)
  b <- quote(b+c)
  c <- quote(c+d)
  default = quote(a+z)

  expect_equal(template( .(a) + .(b)    + .(c)),
               quote(   a + b + (b + c) + (c + d)))

  expect_equal(template( .(a) + b + .(template( .(a) + b ) ) ),
               quote(   a + b + b + (a + b + b) ) )

  expect_equal( template( function (a, b=.(default)) force(b) ),
                 function(a, b = a + z) force(b) )
})

test_that("template interpolation in names", {
  achar <- "a"
  aname <- quote(a)
  bchar <- "b"

  expect_equal(template( list(".(achar)"=foobar) ),
                  quote( list(         a=foobar) ))
  expect_equal(template( list(`.(aname)`=baz   ) ),
                  quote( list(         a=baz   ) ))
  expect_equal(template( list(`.(paste(achar, bchar, sep=""))` = foobar ) ),
                  quote( list(                              ab = foobar ) ))
  #also in formal argument lists.
  expect_equal(template( function( `.(achar)`, `.(bchar)` = default) body ),
                  quote( function(          a,          b = default) body ))

  #thius also covers some situations where nothing other than a name
  #is allowed by the parser
  expect_equal(template(a$`.(bchar)`),
                  quote( a$b ))
  expect_equal(template( for(`.(aname)` in seq_len(10)) NULL),
               quote(    for(         a in seq_len(10)) NULL));
})

test_that("multiple element template interpolation", {
  arglist <- alist(aa, bb, cc)
  namedlist <- alist(aa=a, bb=b, ccc=)

  expect_equal(template( list(z, b, ...(arglist)) ),
                  quote( list(z, b, aa, bb, cc) ) )
  expect_equal(template( list(z, ...(namedlist), q) ),
                  quote( list(a, aa=a, bb=b, ccc=, q) ))
  #and in argument lists. Note how ccc is on the name on the right side.
  #note that the name of the argument ... appears in is ignored at best.
  expect_equal(template( function(q, .=...(namedlist), x) body ),
                  quote( function(q, aa=b, bb=b, ccc, x) body ))
})

