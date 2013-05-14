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
  pow <- 2
  dist <- chain(path, apply(2,diff), .^pow, rowSums, sqrt, cumsum)
  expect_equal(dist, check)
})

test_that('mkchain placeholder', {
  f <- mkchain[foo](apply(2, diff), foo^2, rowSums, sqrt, cumsum)
  expect_equal(f(path), check)
})

test_that('chain placeholder', {
  dist <- chain[foo](path, apply(2,diff), foo^2, rowSums, sqrt, cumsum)
  expect_equal(dist, check)
})

test_that('chain/mkchain arguments', {
  data <- c(13, 32, 54, 68, 12, 31, 14, 31,  5,  9)
  f <- mkchain[x, threshold=20](x>threshold, sum)
  expect_equal(f(data), sum(data > 20))
  expect_equal(f(data, 60), sum(data > 60))

  #and for mkchain
  expect_equal(chain[x, threshold=20](data, x>threshold, sum))
  #no way to change "threshold" in the chain form but that's ok
})

test_that('chain remembers intermediate result', {
  test <- (function(.) {
    . <- foo <- .
    . <- apply(., 2, diff)
    . <- foo^2
    . <- rowSums(.)
    . <- sqrt(.)
    . <- cumsum(.)
    . <- foo/.
  })(path)
  #should be equiv.
  dist <- chain(foo=path, apply(2,diff), foo^2, rowSums, sqrt, cumsum, foo/.)
  expectEqual(foo, test)
})

