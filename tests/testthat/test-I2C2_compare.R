context("Testing I2C2 Comparison")

set.seed(20170602)
id = c(1:10, 10:1)
visit = rep(1:2, each = 10)
visit = as.character(visit)
n = length(id)
p = 100
y = matrix(rnorm(n * p), nrow = n, ncol = p)


test_that("Checking I2C2 default truncate = TRUE", {
  expect_silent({
    res <- I2C2.original(y = y, id = id, visit = visit, trun = TRUE)
    res2 = I2C2(y = y, id = id, visit = visit, truncate = TRUE)
  })
  expect_equal(res$lambda, res2$lambda)
})


test_that("Checking I2C2 demean = FALSE", {
  expect_silent({
    res <- I2C2.original(y = y, id = id, visit = visit, demean = FALSE)
    res2 <- I2C2(y = y, id = id, visit = visit, demean = FALSE)
  })
  expect_equal(res$lambda, res2$lambda)
})


test_that("Checking I2C2 symmetric = FALSE", {
  expect_silent({
    res <- I2C2.original(y = y, id = id, visit = visit, symmetric = FALSE)
    res2 <- I2C2(y = y, id = id, visit = visit, symmetric = FALSE)
  })
  expect_equal(res$lambda, res2$lambda)
})

test_that("Checking I2C2 symmetric = FALSE, demean = FALSE", {
  expect_silent({
    res <- I2C2.original(y = y, id = id, visit = visit, symmetric = FALSE, demean = FALSE)
    res2 <- I2C2(y = y, id = id, visit = visit, symmetric = FALSE, demean = FALSE)
  })
  expect_equal(res$lambda, res2$lambda)
})


test_that("Checking I2C2 twoway = FALSE", {
  expect_silent({
    res <- I2C2.original(y = y, id = id, visit = visit, twoway = FALSE)
    res2 <- I2C2(y = y, id = id, visit = visit, twoway = FALSE)
  })
  expect_equal(res$lambda, res2$lambda)
})

test_that("Checking I2C2 twoway = FALSE, demean = FALSE", {
  expect_silent({
    res <- I2C2.original(y = y, id = id, visit = visit, twoway = FALSE, demean = FALSE)
    res2 <- I2C2(y = y, id = id, visit = visit, twoway = FALSE, demean = FALSE)
  })
  expect_equal(res$lambda, res2$lambda)
})


# Haochang needs to check these!

test_that("Checking I2C2 twoway = FALSE, symmetric = TRUE", {
  expect_silent({
    res <- I2C2.original(y = y, id = id, visit = visit, twoway = FALSE, symmetric = TRUE)
    res2 <- I2C2(y = y, id = id, visit = visit, twoway = FALSE, symmetric = TRUE)
  })
  expect_equal(res$lambda, res2$lambda)
})

test_that("Checking I2C2 twoway = FALSE, demean = FALSE, symmetric = TRUE", {
  expect_silent({
    res <- I2C2.original(y = y, id = id, visit = visit, twoway = FALSE,
                         demean = FALSE, symmetric = TRUE)
    res2 <- I2C2(y = y, id = id, visit = visit, twoway = FALSE,
                         demean = FALSE, symmetric = TRUE)
  })
  expect_equal(res$lambda, res2$lambda)
})
#
# test_that("Checking I2C2 twoway = FALSE, symmetric = TRUE, truncate = TRUE", {
#   expect_silent({
#     res <- I2C2.original(y = y, id = id, visit = visit, twoway = FALSE, symmetric = TRUE,
#                 trun = TRUE)
#   })
#   expect_equal(res$lambda, 0)
# })
#
# test_that("Checking I2C2 twoway = FALSE, demean = FALSE, symmetric = TRUE, truncate = TRUE", {
#   expect_silent({
#     res <- I2C2.original(y = y, id = id, visit = visit, twoway = FALSE,
#                 demean = FALSE, symmetric = TRUE, trun = TRUE)
#   })
#   expect_equal(res$lambda, 0)
# })