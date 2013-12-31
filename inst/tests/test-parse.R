context("String substitution parsing")
`%is%` <- expect_equal

test_that("Matching expressions", {
  cat("\n")
  find_subst_expressions("hey") %is% list(c("hey"))
  find_subst_expressions(".(hey)") %is% list(c("", "hey", ""))
  find_subst_expressions(c(".(hey)", "bye")) %is% list(c("", "hey", ""), "bye")
  find_subst_expressions(".(hey)bye") %is% list(c("", "hey", "bye"))
  find_subst_expressions("nana.(hey)") %is% list(c("nana", "hey", ""))
  (find_subst_expressions("nana{{hey}}", "{{", "}}")
     %is% list(c("nana", "hey", "")))
  (find_subst_expressions("nana___hey___bye", "___", "___")
   %is% list(c("nana", "hey", "bye")))

  find_subst_expressions("na\u0180na") %is% list(c("na\u0180na"))
  find_subst_expressions("na\u0180na") %is% list(c("na\u0180na"))

  find_subst_expressions("nana.(hey.") %is% list("nana.(hey.")
  find_subst_expressions("nana.(h'ey)") %is% list("nana.(h'ey)")
  find_subst_expressions("nana.(h'ey)')") %is% list(c("nana", "h'ey)'", ""))
  find_subst_expressions("nana.(h%ey)%)") %is% list(c("nana", "h%ey)%", ""))
  find_subst_expressions('nana.(h"ey")o') %is% list(c("nana", 'h"ey"', "o"))
  (find_subst_expressions("nana.(hey%\\%)bye") %is%
   list(c("nana", "hey%\\%", "bye")))
  (find_subst_expressions("nana.(('b'))bye") %is%
   list(c("nana", "('b')", "bye")))
  (find_subst_expressions("nana.(`{`(`}`))bye") %is%
   list(c("nana", "`{`(`}`)", "bye")))
  find_subst_expressions("nana.([hey)]") %is% list(c("nana.([hey)]"))
  find_subst_expressions("nana{.(hey})") %is% list(c("nana{.(hey})"))

  (find_subst_expressions("this .(is) a (.(test( '(' )))!") %is%
   list(c("this ", "is", " a (", "test( '(' )", ")!")))

  find_subst_expressions("foo .(bar #baz))") %is% list(c("foo .(bar #baz))"))
  find_subst_expressions("foo .(bar #baz)\n)") %is% list(c("foo ", "bar #baz)\n", ""))
})

test_that("interpolation", {
  expect_equal(interpolate("foo, {{2+2}}, baz", begin="{{", end="}}"),
               "foo, 4, baz")
  expect_error(interpolate("foo, .(rep(1,2)), baz"))
  env <- new.env(parent=emptyenv())
  env$`*` <- `+`
  expect_equal(interpolate("foo, .(3*2), baz", envir=env),
               "foo, 5, baz")
  expect_error(interpolate("foo, .(3+2), baz", envir=env))
  expect_equal(interpolate("foo .(5+4), .(6+5)"), "foo 9, 11")
})

test_that("interply", {
  expect_equal(interply("hello")(1:10), rep("hello", 10))
  expect_equal(interply("hello .(..1)")(c("world", "nurse", "kitty")),
               c("hello world", "hello nurse", "hello kitty"))
  expect_equal(interply("hello {{..1}}", begin="{{", end="}}"
                        )(c("world", "nurse", "kitty")),
               c("hello world", "hello nurse", "hello kitty"))
  expect_equal(interply(".(q) .(r)")(q="hello", r=c("world", "nurse", "kitty")),
               c("hello world", "hello nurse", "hello kitty"))
  expect_equal(interply(".(5*x)", envir=list2env(list(`*`=`+`),
                                                 new.env(parent=emptyenv()))
                        )(x=1:5),
               c("6", "7", "8", "9", "10"))
  expect_error(interply(".(5+x)", envir=list2env(list(`*`=`+`),
                                                 new.env(parent=emptyenv()))
                        )(x=1:5))
  expect_equivalent(interply("hello")(), character(0))
  expect_equivalent(interply("hello")(c()), character(0))
  expect_error(interply())
  expect_error(interply(c("foo", "bar")))
})

test_that("interply shortcut", {
  expect_equal("hello" %#% list(1:10), rep("hello", 10))
  expect_equal("hello .(..1)" %#% list(c("world", "nurse", "kitty")),
               c("hello world", "hello nurse", "hello kitty"))
  expect_equal("hello .(x)" %#% c(x="world"), "hello world")
})
