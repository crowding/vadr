require(testthat)
require(ptools)

## what are the pythagorean quadruples under 20?
test.pipe <- function() {
  ##quads <- subset(expand.grid(x=1:20, y=1:20, z=1:20, w=1:20), x^2+y^2+z^2 == w^2)

  ##each step's length is 5, 3, 9, 11, 7, 3
  path <- matrix(c(0, 0, 0,
                   0, 3, 4,
                   1, 1, 2,
                   0,-3,-6,
                   2, 3, 3,
                   0, 0,-3,
                   0, 0, 0), ncol=3, byrow=TRUE)
  basic.distance <- cumsum(sqrt(rowSums(apply(path, 2, diff)^2)))
  pipe.macro.distance <- pipe.macro(path, apply(.,2,diff), .^2, rowSums(.), sqrt(.), cumsum(.))
  pipe.macro.dwim.distance <- pipe.macro(path, apply(2,diff), .^2, rowSums, sqrt, cumsum)
  pipe.distance <- pipe(path, apply(.,2,diff), .^2, rowSums(.), sqrt(.), cumsum(.))
  pipe.dwim.distance <- pipe(path, apply(2,diff), .^2, rowSums, sqrt, cumsum)
}
