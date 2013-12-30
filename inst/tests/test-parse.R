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
})
