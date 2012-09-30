context("Argument based dm*ply")

#these are just straight translations of existing d_ply tests

df <- data.frame(x = factor(1:10), y = letters[1:10])

test_that("results ordered in order of split variables", {
  d <- data.frame(x = c("a","b"), y = c("d","c"), stringsAsFactors = FALSE)

  plyed <- dmdply(d, c("x" ,"y"))
  expect_that(plyed$x, equals(c("a", "b")))
  expect_that(plyed$y, equals(c("d", "c")))

  plyed <- dmdply(d, c("y" ,"x"))
  expect_that(plyed$y, equals(c("c", "d")))
  expect_that(plyed$x, equals(c("b", "a")))
})

test_that("character vectors not change to factors", {
  d <- data.frame(x = c("a","b"), y = c("d","c"), stringsAsFactors = FALSE)

  plyed <- dmdply(d, c("x" ,"y"), fun(length(x)))
  expect_that(plyed$x, is_a("character"))
  expect_that(plyed$y, is_a("character"))

  plyed <- dmdply(d, c("x"), mutator())
  expect_that(plyed$x, is_a("character"))
  expect_that(plyed$y, is_a("character"))

  plyed <- dmdply(d, c("x" ,"y"), fun(length(x)), .drop = FALSE)
  expect_that(plyed$x, is_a("character"))
  expect_that(plyed$y, is_a("character"))

  plyed <- ddply(d, c("x"), mutator(), .drop = FALSE)
  expect_that(plyed$x, is_a("character"))
  expect_that(plyed$y, is_a("character"))
})

test_that("column names not changed", {
  d1 <- data.frame(`--WEIRD`=1:5, a = letters[1:5], `-b` = 1:5, 
    check.names = FALSE)
  d2 <- ddply(d1, .(`--WEIRD`), force) 
  expect_that(names(d2), equals(names(d1)))
})

test_that("label variables always preserved", {
  d <- data.frame(x = 101:104, y = 1:4)
  f <- fun(sum(y))
  g <- fun(if(x<=102) sum(y))

  out1 <- dmdply(d, "x", f)
  out2 <- dmdply(d, "x", g)

  expect_that(names(out1), equals(names(out2)))
  expect_that(out1$x[1:2], equals(out2$x))
})

test_that("array reconstruction correct with missing cells", {
  df <- data.frame(i = rep(1:3, each = 12), j = rep(1:3, each = 4), v = 1:36)
  df <- subset(df, i != j)

  da <- dmaply(df, .(i, j), fun(sum(v)))
  dd <- dmdply(df, .(i, j), summariser(v1=sum(v)))

  m <- matrix(NA, 3, 3)
  m[cbind(dd$i, dd$j)] <- dd$v1

  expect_that(da, equals(m, check.attributes = FALSE))
})

test_that("empty data frames returns empty object", {
  df <- data.frame(x = numeric(0), a = numeric(0))
  expect_that(dmdply(df, "a", data.frame), equals(df))
  expect_that(dmlply(df, "a", list), equals(list()))
  expect_that(dmaply(df, "a", c), equals(logical()))
})

test_that("correct number of rows outputted", {
  testdata <- data.frame(a = rep(letters[1:3], each = 5), b = rnorm(15))
  res <- dmdply(testdata, .(a), fun(c(mean(b), sd(b))))

  expect_that(nrow(res), equals(3))
})

test_that("correct order is used", {
  df <- data.frame(x = factor(1:10), y = letters[1:10])

  expect_that(dmdply(df, .(x), .drop = FALSE), equals(df))
  expect_that(dmdply(df, .(x), .drop = TRUE), equals(df))
})
