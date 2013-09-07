context("Quasiquote implementation pieces")

`%is%` <- expect_equal

expect_registers <- function(unquote, argument, expect, register=list()) {
  unquote_label <- testthat:::find_expr("unquote")
  argument_label <- testthat:::find_expr("argument")
  expect_label <- testthat:::find_expr("expect")
  register_label <- testthat:::find_expr("register")
  unquote_call_label <- paste0(unquote_label,
                               "(", argument_label, ")")

  registry <- new_registry()
  result <- unquote(argument, registry)
  registered <- registry(op="expressions")
  named <- registry(op="argnames")

  expect_that(result, label=unquote_call_label,
              is_equivalent_to(expect, label=expect_label))

  expect_that(registered,
              label=paste0("registered arguments of ",
                           unquote_call_label),
              is_equivalent_to(register, label=register_label))
}

with_registered <- function(unquoter, argument) {
  registry <- new_registry()
  result <- unquoter(argument, registry)
  list(result, registry(op="expressions"))
}

test_that("registry", {
  r <- new_registry()
  r(quote(a+b)) %is% quote(..1)
  r(quote(c+d)) %is% quote(..2)
  r(quote(e/f)) %is% quote(..3)
  r(op="expressions") %is% alist(..1=a+b, ..2=c+d, ..3=e/f)
})

test_that("unquote char", {
  expect_registers(uq_char, "hello", "hello")
  expect_registers(uq_char, ".(4)", "4", list())
  expect_registers(uq_char, ".(`+`(a,b))",
                   quote(as.character(..1)),
                   alist(..1=a+b))
  expect_registers(uq_char, ".(as.name('foo'))",
                   quote(as.character(..1)),
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
  expect_registers(uq_name, quote(hello), quote(hello))
  expect_registers(uq_name, quote(`.(4)`), quote(`4`))
  expect_registers(uq_name, quote(`.(foo+bar+baz)`),
                   quote(as.name(..1)), alist(..1=foo+bar+baz))
})

with_registered(uq_name, quote(`.(foo+bar+baz)`))
