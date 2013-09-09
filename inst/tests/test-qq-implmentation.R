context("Quasiquote implementation")

#these tests are for gettting the guts of fast macro-ized quasiquote
#implementation in place. They test internal behavior (the particular
#format of a quasiquote expansion) rather than behavior
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
  r <- new_registry()
  fn <- eval(call("function", as.pairlist(alist(...=)),
                  as.call(c(list(quote(c)), uq(unquotable, r)))))

  unquote_call_label <- paste0("uq", "(",
                               testthat:::find_expr("unquotable"),
                               ")")

  expect_label <- testthat:::find_expr("expected")
  expect_that(
      do.call(fn, unattr(r(op="expressions")), envir=parent.frame()),
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
  r <- new_registry()
  fn <- eval(call("function", as.pairlist(alist(...=)),
                  as.call(c(list(quote(c)), uq(unquotable, r)))))
  print(fn)
  do.call(fn, unattr(r(op="expressions")), envir=parent.frame())
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
  expect_registers("hello", list(alist("hello")))
  expect_registers(".(4)", list(alist("4")))
  expect_registers(".(foo)",
                   alist(list(as.character(..1))), alist(..1=foo))
  expect_registers(".(`+`(a,b))",
                   alist(list(as.character(..1))),
                   alist(..1=a+b))
  expect_registers(".(as.name('foo'))",
                   alist(list(as.character(..1))),
                   alist(..1=as.name('foo')))
})

test_that("unquote name", {
  expect_registers(quote(hello), list(alist(hello)))
  expect_registers(quote(`.(4)`), list(alist(`4`)))
  expect_registers(quote(`.(foo)`),
                   alist(list(as.name(..1))), alist(..1=foo))
  expect_registers(quote(`.(foo+bar+baz)`),
                   alist(list(as.name(..1))), alist(..1=foo+bar+baz))
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

test_that("unquote call", {
  expect_registers(quote(.(bar)),     alist(list(..1)),  alist(..1=bar))
  expect_registers(quote(.("bar")),   list(alist("bar")))
  expect_registers(quote(...(bar)),   alist(c(..1)),     alist(..1=bar))
  expect_registers(quote(...("bar")), list(alist("bar")))
  expect_registers(quote(foo(bar)),   list(alist(foo(bar))))
  expect_registers(quote(...("bar")), alist("bar"))
})

test_that("unquote multiple args call", {
  expect_registers(quote(...("bar", "baz")), list(c("bar", "baz")))
  expect_registers(quote(.("bar", "baz")), list(list("bar", "baz")))
  expect_uq(quote(.("bar", c("baz", "qux"))),
            list("bar", c("baz", "qux")))
  expect_uq(quote(...("bar", c("baz", "qux"))),
            c(c("bar", "baz", "qux")))
})

test_that("unquote named .call", {
  expect_registers(quote(.(a="bar")), list(list(a="bar")))
  expect_registers(quote(...(a="bar")), list(a="bar"))
  expect_registers(quote(.(a=1+1)), alist(list(a=..1)), alist(..1=1+1))
  expect_registers(quote(...(a=1+1)), alist(c(a=..1)), alist(..1=1+1))
  expect_registers(quote(.(a=1+1, b=2+2)), alist(list(a=..1, b=..2)),
                   alist(..1=1+1, ..2=2+2))
  expect_registers(quote(...(a=1+1, b=2+2)), alist(c(a=..1, b=..2)),
                   alist(..1=1+1, ..2=2+2))
})

test_that("unquote list", {
  expect_registers(alist(a, b, c, d), list(alist(a, b, c, d)))

  expect_uq(alist(.(1+1)), alist(2))
  expect_registers(alist(.(1+1)), alist(c(list(..1))),
                   alist(..1=1+1))

  expect_uq(alist(.(1+1), b), alist(2, b))
  expect_registers(alist(.(1+1), b),
                   list(bquote(c(list(..1), .(alist(b))))),
                   alist(..1=1+1))

  expect_uq(alist(a, ...(1+1, 2+2), c), alist(a, 2, 4, c))
  expect_registers(alist(a, ...(1+1, 2+2), c),
                   list(bquote(c(.(alist(a)), c(..1, ..2), .(alist(c))))),
                   alist(..1=1+1, ..2=2+2))
})

test_that("unquote named multiple argument call", {
  expect_uq(alist(a=...(b=1+1, c=2), q=1) ,
            c(a.b=2, a.c=2, q=1))
  expect_uq(alist(a=.(b=1+1, c=2), q=1),
            list(a.b=2, a.c=2, q=1))
  expect_uq(alist(a=1, b=.(1, 2)),
            alist(a=1, b1=1, b2=2))
  expect_uq(alist(a=1, b=...(1, 2)),
            c(a=1, b1=1, b2=2))

  expect_uq(quote(list(a=1, b=.(1, "foo"))),
            alist(list(a=1, b1=1, b2="foo")))

  expect_uq(quote(list(a=1, b=.(1, "foo"))),
            alist(list(a=1, b1=1, b2="foo")))

  expect_uq(quote(list(a=1, b=.(1, "foo"))),
            alist(list(a=1, b1=1, b2="foo")))

  expect_uq(alist(a=1, b=...()), c(a=1))
  expect_uq(alist(c=...(a=1:3, b=2:4)),
            c(c.a1=1, c.a2=2, c.a3=3, c.b1=2, c.b2=3, c.b3=4))
})

#I HAVE_GOT_THIS_FAR

if(FALSE) {

test_that("unquote named list", {
  expect_uq(alist(a, b, `c`=c, d),
            alist(a, b, c=c, d))

  expect_uq(alist(a, b=.(1+2), `.(1+1)`=c, d),
            alist(a, b=3, `2`=c, d))

  expect_uq(alist(a, b, `.(1+1)`=.(1+2), d),
            alist(a, b, `2`=3, d))

  expect_uq(alist(a, b, .(a=1), c, d),
            alist(a, b, a=1, d))

  expect_uq(alist(a, b, .(a=1), c, d),
            alist(a, b, a=1, d))
})

test_that("Unquote name substitution in argument lists tho", {
  ## expect_uq(quote( `.(letters)`=...(LETTERS)),
  ##           list(structure(LETTERS, names=letters)))
})

}
