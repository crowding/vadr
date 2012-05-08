require(testthat)
#require(ptools)

context("data frame indexing")

df <- data.frame(  A = c(1,4,2,6,7,3,6)
                 , B = c(3,7,2,7,3,5,4)
                 , C = c(2,7,5,2,7,4,5)
                 , index = c("A","B","A","C","B","B","C")
                 , letter = I(letters[7:13])
                 , lletter = I(list("a",1,"b",3,NULL,5,"d"))
                 , row.names = c("foo", "bar", "baz", "qux", "quux", "quuux", "quuuux")
                 )

test_that("can index data columns by number", {
  expect_that( index(df, col=df$index) , equals( c(1, 7, 2, 2, 3, 5, 5) ))
})

test_that("can index a single row and col, by number or by name", {
  expect_that( index(df, 3, 2) , equals( 2 ))
  expect_that( index(df, 7, 1) , equals( 6 ))
})

test_that("can index a row by number and col by name", {
  expect_that( index(df, 3, "B") , equals( 2 ))
  expect_that( index(df, 7, "A") , equals( 6 ))
})


test_that("can index both by name", {
  expect_that( index(df, "baz", "B"), equals( 2 ) )
  expect_that( index(df, "quuuux", "A"), equals(6) )
})

test_that("can index by a 1x2 num array", {
  expect_that( index(df, array(c(3, 2), dim=c(1,2))) , equals( 2 ))
  expect_that( index(df, array(c(7, 1), dim=c(1,2))) , equals( 6 ))
})

test_that("can index by a char array", {
  expect_that( index(df, array(c("baz", "B"), dim=c(1,2))), equals(2) )
  expect_that( index(df, array(c("quuuux", "A"), dim=c(1,2))), equals(6) )
})

test_that("can index by a mixed data frame", {
  expect_that( index(df, data.frame(a=3, b="B")), equals(2) )
  expect_that( index(df, data.frame(a="quuuux", b=1)), equals(6) )
})

test_that("can vectorized index", {
  expect_that( index(df, col=df$index), equals(c(1, 7, 2, 2, 3, 5, 5)))
})

test_that("can vectorized index with out of order vectors", {
  expect_that( index(df, c(1, 3, 2, 4), c("A", "B", "C", "A")), equals(c(1,2,7,6)))
})

test_that("unknown indices throw exception", {
  expect_that( index(df, "grauply", 1), throws_error() )
  expect_that( index(df, 1, "grauply"), throws_error() )
})

test_that("output type ascends hierarchy", {
  expect_that( index(df, c(4), c("A")), equals(6))
  expect_that( index(df, c(4,5), c("A", "letter")), equals(c("6", "k")))
  expect_that( index(df, c(4,5,6), c("A", "letter", "lletter")), equals(list("6", "k", 5)))
})
          
test_that("can specify output type", {
  expect_that( index(df, "bar", "A", value=character()), equals("4"))
  expect_that( index(df, c(2,3), c("A", "letter"), value=list()), equals(list(4, "i")))
})

## test_that("WHen output type is specified, is strict")
## test_that("

test_that("can assign ", {
  dftmp <- df
  index(dftmp, "foo", 1) <- 4
  expect_that(index(dftmp, "foo", 1), equals(4))
})

test_that("can assign in parallel ", {
  dftmp <- df
  index(dftmp, c("foo", "bar"), c('letter', 'lletter')) <- c("boo", "bar")
  expect_that(dftmp$letter, equals(I(c("boo", letters[8:13]))))
  expect_that(dftmp[["bar", "lletter"]], equals("bar"))
})
