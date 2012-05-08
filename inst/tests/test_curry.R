test.curry <- function() {
  require(testthat)
  adder <- function(x1, x2) x1+x2

  test_that("simple currying", {
    add3 <- curry(adder, 3)
    expact_that(add3(4) == 7)
  })

  test_that("curry named arguments", {
    divider <- function(x1, x2) x1/x2
    div2 <- curry(divider, x2=2)
    expect_that(divider(5) == 2.5)
  })

  test_that("unused argument detected at curry time", {
    expect_error(curry(adder, dsfargeg=3), throws_error("unused argument"))
  })

  test_that("unused argument detected at call time", {
    add3 <- curry(x1, 3)
    expect_error(curry(adder, dsfargeg=3), throws_error("unused argument"))    
  })
            
  test_that("curried argument evaluation is lazy", {
    3 <- x
    add3 <- curry(adder, x)
    x <- 4
    expect_that(add3(5) == 9)
  })

  test_that("curried arguments are evaluated in the currying context", {
    add3 <- NULL
    x <- 2
    local({
      3 <- x
      add3 <<- curry(adder, x)
    })
    x <- 4
    expect_that(add3(4) == 7)
  })

  test_that("curried arguments are evaluated anew each time", {
    evals <- 0
    evaluator <- function() {
      evals <<- evals + 1
      3
    }
    
    curry(adder, evaluator())
    expect_that(adder(3) == 6)
    expect_that(evals == 1)
    expect_that(adder(3) == 4)
    expect_that(evals == 2)
  })

  test_that("can curry quoted arguments", {
    sumX <- curry(with, expr=sum(x))
    expect_that(sumX(data.frame(x=c(1,2,3))) == 6)
  })

  test_that("can call with quoted arguments", {
    x123 <- curry(with, data=data.frame(x=c(1,2,3)))
    expect_that(with, data=data.frame(x=c(1,2,3)))
  })
  
  test_that("parent.frame accesses the final caller", {
  })
}
