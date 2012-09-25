context("chain")

## what are the pythagorean quadruples under 20?
##quads <- subset(expand.grid(x=1:20, y=1:20, z=1:20, w=1:20), x^2+y^2+z^2 == w^2)

path <- matrix(c(0, 0, 0,
                 0, 3, 4,
                 1, 1, 2,
                 0,-3,-6,
                 2, 3, 3,
                 0, 0,-3,
                 0, 0, 0), ncol=3, byrow=TRUE)

check <- cumsum(sqrt(rowSums(apply(path, 2, diff)^2)))

test_that('chain without DWIM', {
   ch <- mkchain(apply(.,2,diff), .^2, rowSums(.), sqrt(.), cumsum(.),
                 .dwim=FALSE)
   dist <- ch(path)
   expect_equal(dist, check)
})

test_that('chain with DWIM', {
  ch <- mkchain(apply(2,diff), .^2, rowSums, sqrt, cumsum)
  dist <- ch(path)
  expect_equal(dist, check)
})

test_that('chain immediate', {
  dist <- chain(path, apply(2,diff), .^2, rowSums, sqrt, cumsum)
  expect_equal(dist, check)
})

test_that('chain lexical scope', {
  pow <- 3
  dist <- chain(path, apply(2,diff), .^2, rowSums, sqrt, cumsum)
})

test_that('chain intermediate result', {
  #nothing here yet. Think about the syntax.
})

test_that('chain placeholder', {
#  dist <- chain(foo=P, apply(2,diff), foo^2, rowSums, sqrt, cumsum)
})

test_that('mkchain placeholder', {
  #mkchain(foo=., apply(2, diff), foo^2, rowSums, sqrt, sum)
})
