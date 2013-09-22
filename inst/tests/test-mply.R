context("mply")

`%is%` <- expect_equal

test_that("mply", {
   mply(c)(a=1:3, b=letters[1:3]) %is% list(
       c(a=1, b="a"), c(a=2, b="b"), c(a=3, b="c"))

   mply(c)(1, b=2, 3) %is% list(c(1, b=2, 3))

   mply(c, 1)(a=1:3, b=letters[1:3]) %is% list(
       c(a=1, b="a", 1), c(a=2, b="b", 1), c(a=3, b="c", 1))

   mply(c)(c()) %is% list()

   expect_error(mply(c)())

   expect_error(mply(c) %()% NULL)

   (mply(function(y, z) list(z, substitute(y)), y=x+1)(z=list(1, 2))
    %is% list(alist(1, x+1), alist(2, x+1)))
})
