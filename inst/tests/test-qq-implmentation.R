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
  evalable <- uq(unquotable, r)
  fn <- eval(call("function", as.pairlist(alist(...=)), evalable))

  unquote_call_label <- paste0("uq", "(",
                               testthat:::find_expr("unquotable"),
                               ")")

  expect_label <- testthat:::find_expr("expected")
  expect_that(
      do.call(fn, unattr(r(op="expressions")), envir=parent.frame())[[1]],
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
  evalable <- uq(unquotable, r)
  fn <- eval(call("function", as.pairlist(alist(...=)), evalable))
  #print(fn)
  do.call(fn, unattr(r(op="expressions")), envir=parent.frame())[[1]]
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
})

test_that("Unquote function arguments", {
  testfn <- eval(uq_makes(quote(
      function(x, y=.(3+4)) .(2+2)) ))

  expect_equal(body(testfn), 4)
  expect_equal(args(testfn), args(function(x, y=7) NULL))

  testfn <- eval(uq_makes(quote(
      function(x=...(letters[1:3])) {
        list(...(lapply(letters[1:3], as.name)))
      }  )))

  expect_equal(body(testfn), quote({list(a, b, c)}))
  expect_equal(args(testfn), args(function(x1="a", x2="b", x3="c") NULL))
})

test_that("unquote in for", {
  expect_uq(quote(for (`.(letters[4])` in .(1:10)) {print(.(2+2)+d)}),
            quote(for (d in 1:10) { print(4+d) }))
})

if(FALSE) {

  test_that("unquoted named elements", {
    uq_named(list(a=1))
  })

  named_makes <- function(item) {
    reg <- new_registry()
    result <- uq_named(item, reg)
    list(unquoted=result, delayed=reg(op="expressions"))
  }

  named_makes(alist(a=1))
  named_makes(alist(`.("foo")`=1))
  named_makes(alist(`.(1+2)`=1))
  named_makes(alist(a=.("foo")))
  named_makes(alist(a=.(foo+bar)))
  named_makes(alist(`.(1+2)`=.(foo+bar)))

  test_that("Unquote name substitution in argument lists tho", {
    expect_uq(alist(a, b=.(1+2), `.(1+1)`=c, d),
              alist(a, b=3, `2`=c, d))

    expect_uq(alist(a, b, `.(1+1)`=.(1+2), d),
              alist(a, b, `2`=3, d))

    expect_uq(quote( `.(letters)`=...(LETTERS)),
              list(structure(LETTERS, names=letters)))
})}
