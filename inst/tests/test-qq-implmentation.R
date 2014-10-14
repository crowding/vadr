context("Quasiquote low level")

#these tests are for getting the guts of fast macro-ized quasiquote
#implementation in place. They test internals (the particular
#format of a quasiquote macro expansion) rather than behavior
#(the effects of quasiquoting) so are not binding.

`%is%` <- expect_equal

expect_registers <- function(argument, expect, register=list()) {
  argument_label <- testthat:::find_expr("argument")
  expect_label <- testthat:::find_expr("expect")
  register_label <- testthat:::find_expr("register")
  unquote_call_label <- paste0("uq",
                               "(", argument_label, ")")

  registry <- new_registry()
  result <- uq(argument, registry)
  registered <- registry(op="expressions")
  named <- registry(op="argnames")

  expect_that(result, label=unquote_call_label,
              is_equivalent_to(expect, label=expect_label))

  expect_that(registered,
              label=paste0("registered arguments of ",
                           unquote_call_label),
              is_equivalent_to(register, label=register_label))
}

expect_uq <- function(unquotable, expected) {
  envir = arg_env(unquotable)
  r <- new_registry()
  evalable <- uq(unquotable, r)
  args <- r(op="expressions")
  fn <- eval(call("function", as.pairlist(alist(...=)), evalable))

  unquote_call_label <- paste0("uq", "(",
                               testthat:::find_expr("unquotable"),
                               ")")

  expect_label <- testthat:::find_expr("expected")
  expect_that(
      do.call(fn, args, envir=envir)[[1]],
      label=unquote_call_label,
      equals(expected,
             label=expect_label)
      )
}

with_registered <- function(argument) {
  registry <- new_registry()
  result <- uq(argument, registry)
  list(unquoted=result, delayed=registry(op="expressions"))
}

uq_makes <- function(unquotable) {
  envir <- arg_env(unquotable)
  r <- new_registry()
  evalable <- uq(unquotable, r)
  args <- r(op="expressions")
  fn <- eval(call("function", as.pairlist(alist(...=)), evalable))
  #print(fn)
  do.call(fn, args, envir=unquotable)[[1]]
}

test_that("registry", {
  r <- new_registry()
  r(quote(a+b)) %is% quote(..1)
  r(quote(c+d)) %is% quote(..2)
  r(quote(e/f)) %is% quote(..3)
  r(op="expressions") %is% alist(..1=a+b, ..2=c+d, ..3=e/f)
})

## God and damn. I think the rule is that each piece returns a list of
## evaluabels that each evaluate to lists.

test_that("register intercept", {
  x <- new_registry()
  r <- register_intercept(x)
  r(op="eval_needed") %is% FALSE
  r(quote(a+b)) %is% quote(..1)
  r(op="eval_needed") %is% TRUE
  r(op="expressions") %is% alist(..1=a+b)
})

test_that("unquote char", {
  expect_registers("hello", alist("hello"))
  expect_registers(".(4)", alist("4"))
  expect_registers(".(foo)",
                   quote(list(as.character(..1))), alist(..1=foo))
  expect_registers(".(`+`(a,b))",
                   quote(list(as.character(..1))),
                   alist(..1=a+b))
  expect_registers(".(as.name('foo'))",
                   quote(list(as.character(..1))),
                   alist(..1=as.name('foo')))
})

## test_that("complicated unquote char", {
##   expect_registers("testing.(this)",
##                    quote(list(paste0("testing", as.character(..1)))),
##                    alist(..1=as.name('this')))
##   expect_registers(".(this) is a test",
##                    quote(list(paste0(as.character(..1), " is a test"))),
##                    alist(..1=as.name('this')))
##   expect_registers("and .(this) is a .(harder) test",
##                    quote(list(paste0("and ",
##                                      as.character(..1), " is a ",
##                                      as.character(..2), " test"))),
##                    alist(..1=as.name('this'), ..2=as.name('harder')))
##   expect_registers("this .( c('(', '')[p] )is.( c(\")\", 'n\\'t')[p] ) in parens",
##                    quote(list(paste0("this ", as.character(..1),
##                                      "is", as.character(..2),
##                                      " in parens"))),
##                    alist(..1=c('(', '')[p],
##                          ..2=c(')', "n't")[p]))
## })

test_that("unquote name", {
  expect_registers(quote(hello), alist(hello))
  expect_registers(quote(`.(4)`), alist(`4`))
  expect_registers(quote(`.(foo)`),
                   quote(list(uq_as_name(as.character(..1)))), alist(..1=foo))
  expect_registers(quote(`.(foo+bar+baz)`),
                   quote(list(uq_as_name(as.character(..1)))), alist(..1=foo+bar+baz))
})

#This part is trickier.

#Here's how unquotes appearing in arg lists are expanded into
#intermediate form. The intermediate form should eval to the evaluated
#form. The {{}} represents non-syntactic appearance of data objects in
#an expresion.

# A single dot
# quasiquote foem       list(a, b, .(1:3), d)
# intermediate form  -> as.call(c( {{alist(list, a, b)}}, list(..1), {{alist(d)}}))
# captured arguments -> ..1=1:3
# evaluated form     -> list(a, b, c, {{1:3}}, d)

# A triple dot
# quasiquote form       list(a, b, c, .(1:3), d)
# intermediate form  -> as.call(c( {{alist(list, a, b, c)}}, ..1, {{alist(d)}}))
# captured arguments -> ..1=1:3
# evaluated form     -> list(a, b, 1, 2, 3, d)

# A single dot with multiple arguments
# quasiquote            list(a, b, .(1:3, 4:6), d)
# intermediate form  -> as.call(c( {{alist(list, a, b)}}, list(..1, ..2), {{alist(d)}}))
# captured arguments -> ..1=1:3, ..2=4:6
# evaluated form     -> list(a, b, c, {{1:3}}, d)

# A triple dot with multiple arguments
# quasiquote            list(a, b, .(1:3, 4:6), d)
# intermediate form  -> as.call(c( {{alist(list, a, b)}}, ..1, ..2, {{alist(d)}}))
# captured arguments -> ..1=1:3, ..2=4:6
# evaluated form     -> list(a, b, c, {{1:3}}, d)

test_that("unquote dots", {
  expect_registers(quote(.(bar)),     quote(list(..1)),  alist(..1=bar))
  expect_registers(quote(.("bar")),   list("bar"))
  expect_registers(quote(...(bar)),   quote(..1),     alist(..1=bar))
  expect_registers(quote(...("bar")), "bar")
  expect_registers(quote(...("bar")), "bar")
})

test_that("unquote call", {
  expect_uq(quote(foo(bar)), quote(foo(bar)))
  expect_uq(quote(foo(.(1+1))), quote(foo(2)))
  expect_uq(quote(.(as.name("foo"))(1+1)), quote(foo(1+1)))
  expect_uq(quote(`.(paste0("f", "oo"))`(1+1)), quote(foo(1+1)))
  expect_uq(quote(.(list)(a, b, c)), as.call(c(list(list), alist(a, b, c))))
  uqable <- as.call(c(list(list), alist(a, b, c)))
  expect_uq(uqable, as.call(c(list(list), alist(a, b, c))))
})

test_that("Unquote function arguments", {
  expect_uq( quote(function(x, y=4) 4),
             quote(function(x, y=4) 4))

  expect_uq(quote(function(x, y=1) .(2+2)),
            quote(function(x, y=1) 4))

  expect_uq(quote(function(x, y=.(3+4)) 5),
            quote(function(x, y=7) 5) )

  expect_uq( quote(function(x, y=.(3+4)) .(2+2)),
             quote(function(x, y=7) 4) )

  expect_uq(quote(function(`.(letters[1:3])`=...(letters[1:3]))
                  list(...(lapply(letters[1:3], as.name)))),
            quote(function(a="a", b="b", c="c")
                  list(a, b, c)))

  expect_uq(quote(function() 2), quote(function() 2))
})

test_that("unquote splice empty", {
  expect_uq(quote(list(1, 2, ...(NULL))), quote(list(1, 2)))

  expect_uq(quote(function(x=...(NULL)) 2),
            quote(function() 2))
})

test_that("unquote in for", {
  expect_uq(quote(for (`.(letters[4])` in .(1:10)) {print(.(2+2)+d)}),
            quote(for (d in 1:10) { print(4+d) }))
})

named_makes <- function(item) {
  reg <- new_registry()
  uq_named(item, reg)
}

test_that("unquoted named elements", {
  expect_uq(quote(list(a=1)), quote(list(a=1)))
  expect_uq(quote(list(`.("foo")`=1)), quote(list(foo=1)))
  expect_uq(quote(list(`.(1+2)`=1)), quote(list(`3`=1)))
  expect_uq(quote(list(a=.("foo"))), quote(list(a="foo")))
  expect_uq(quote(list(a=.(paste0("foo", "bar")))), quote(list(a="foobar")))
  expect_uq(quote(list(`.(1+2)`=.(paste0("foo", "bar")))),
            quote(list(`3`="foobar")))
})

test_that("Unquote name substitution in argument lists", {
  expect_uq(quote(alist(a, b=.(1+2), `.(1+1)`=c, d)),
            quote(alist(a, b=3, `2`=c, d)))

  expect_uq(quote(alist(a, b, `.(1+1)`=.(1+2), d)),
            quote(alist(a, b, `2`=3, d)))

  #when using ., inner name is obscured...
  expect_uq(quote(alist(a, b, `x`=.(c(y=1+2)), d)),
            bquote(alist(a, b, x=.(c(y=3)), d)))

  expect_uq(quote(alist(a, b, `.(c('x'))`=.(c(y=1+2)), d)),
            bquote(alist(a, b, x=.(c(y=3)), d)))

  #But when using ..., the inner names dominate.
  #(this asymmetry is required for function arg lists.)
  expect_uq(quote(alist(a, b, `x`=...(c(y=1+2)), d)),
            quote(alist(a, b, y=3, d)))

  expect_uq(quote(alist(a, b, `.(c('x'))`=...(c(y=1+2)), d)),
            quote(alist(a, b, y=3, d)))

  #but we still allows this cuteness
  expect_uq(quote(c(`.(letters)`=...(LETTERS))),
            as.call(c(list(quote(c)), structure(LETTERS, names=letters))))
})
