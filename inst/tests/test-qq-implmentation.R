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
  fn <- eval(bquote(function(...) .(uq(unquotable, r))))

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
  fn <- eval(bquote(function(...) .(uq(unquotable, r))))
  do.call(fn, unattr(r(op="expressions")), envir=parent.frame())
}

test_that("registry", {
  r <- new_registry()
  r(quote(a+b)) %is% quote(..1)
  r(quote(c+d)) %is% quote(..2)
  r(quote(e/f)) %is% quote(..3)
  r(op="expressions") %is% alist(..1=a+b, ..2=c+d, ..3=e/f)
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

test_that("register intercept", {
  x <- new_registry()
  r <- register_intercept(x)
  r(op="eval_needed") %is% FALSE
  r(quote(a+b)) %is% quote(..1)
  r(op="eval_needed") %is% TRUE
  r(op="expressions") %is% alist(..1=a+b)
})

test_that("unquote name", {
  expect_registers(quote(hello), alist(hello))
  expect_registers(quote(`.(4)`), alist(`4`))
  expect_registers(quote(`.(foo)`),
                   quote(list(as.name(..1))), alist(..1=foo))
  expect_registers(quote(`.(foo+bar+baz)`),
                   quote(list(as.name(..1))), alist(..1=foo+bar+baz))
})

test_that("unquote call", {
  expect_registers(quote(.(bar)), quote(list(..1)), alist(..1=bar))
  expect_registers(quote(.("bar")), list("bar"))
  expect_registers(quote(...(bar)), quote(..1), alist(..1=bar))
  expect_registers(quote(...("bar")), as.list("bar"))
  expect_uq(quote(foo(bar)), list(quote(foo(bar))))
  expect_registers(quote(...("bar")), as.list("bar"))
})

test_that("unquote list", {
  expect_registers(alist(a, b, c, d), alist(a, b, c, d))
  expect_uq(alist(.(1+1)), alist(2))
  expect_uq(alist(.(1+1), b), alist(2, b))
  expect_uq(alist(a, .(1+1)), alist(a, 2))
})

test_that("unquote named list", {
  if(FALSE) {
  expect_uq(alist(a, b, `.(c)`=c, d),
                 bquote(structure(.(list(a, b, c, d)),
                                  names=c("", "", as.character(..1), ""))),
                 alist(..1=quote(c)))

  #this formatting may change with information about what is fast to eval
  #but let's start with the simplest way that supports unquote-splicing,
  #which catenates literal lists.
  expect_registers(list(a, b, .(c), d),
                   bquote(
                       as.call(c(.(alist(list, a, b)),
                                 list(..1),
                                 .(alist(d))))))

  #and this format has the right effect...
  expect_uq(quote(list(a, b, .(1+1), d)),
                 alist(a, b, 2, d))

  #and this format has the right effect...
  #todo: unquoting names in lists...
}})
