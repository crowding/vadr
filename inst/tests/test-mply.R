context("mply")

`%is%` <- expect_equal

test_that("mply", {
   mply(c)(a=1:3, b=letters[1:3]) %is% list(
       c(a=1, b="a"), c(a=2, b="b"), c(a=3, b="c"))

   mply(c)(1, b=2, 3) %is% list(c(1, b=2, 3))

   mply(c, 1)(a=1:3, b=letters[1:3]) %is% list(
       c(a=1, b="a", 1), c(a=2, b="b", 1), c(a=3, b="c", 1))

   mply(c)(c()) %is% list()

   # mply returns a list whose length is the max* of the lengths of its
   # arguments. Since "length" is always non-negative, the max* of an empty
   # list is zero.  Therefore mply with no arguments should return a
   # length-0 list.
   #
   # (The other possibility would be to throw an error on encountering
   # no args. I think that vector recycling is more consistent with the
   # first option.)
   mply(c)() %is% list()

   (mply(c) %()% NULL) %is% list()

   # mply should recycle arguments.
   (mply(prod)(1:2, 1:10, 1:5) %is%
    list(1, 8, 9, 32, 25, 12, 14, 48, 36, 100))

   (mply(function(y, z) list(z, substitute(y)), y=x+1)(z=list(1, 2))
    %is% list(alist(1, x+1), alist(2, x+1)))
})
