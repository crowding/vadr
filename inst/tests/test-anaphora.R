`%is%` <- expect_equal

test_that("Modification assignment", {
  x <- 'a'
  x %<~% toupper
  x %is% 'A'
  x %<~% str_dup(3)
  x %is% 'AAA'
  x %<~% rep(3)
  x %is% c('AAA', 'AAA', 'AAA')
  x %<~% .[2:3]
  x %is% c('AAA', 'AAA')
})
