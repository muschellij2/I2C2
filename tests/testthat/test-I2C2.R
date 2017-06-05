context("Testing I2C2 different defaults")

set.seed(20170602)
id = c(1:10, 10:1)
visit = rep(1:2, each = 10)
visit = as.character(visit)
n = length(id)
p = 100
y = matrix(rnorm(n * p), nrow = n, ncol = p)


test_that("Checking i2c2 default", {
  expect_silent({
    res <- i2c2(y = y, id = id, visit = visit)
  })
  expect_equal(res$lambda, 0.070393118270377946777)
})

test_that("Checking I2C2 default truncate = TRUE", {
  expect_silent({
    res <- I2C2(y = y, id = id, visit = visit, truncate = TRUE)
  })
  expect_equal(res$lambda, 0.070393118270377946777)
})


test_that("Checking I2C2 demean = FALSE", {
  expect_silent({
    res <- I2C2(y = y, id = id, visit = visit, demean = FALSE)
  })
  expect_equal(res$lambda, 0.012814655436895599008)
})


test_that("Checking I2C2 symmetric = FALSE", {
  expect_silent({
    res <- I2C2(y = y, id = id, visit = visit, symmetric = FALSE)
  })
  expect_equal(res$lambda, 0.070393118270377946777)
})

test_that("Checking I2C2 symmetric = FALSE, demean = FALSE", {
  expect_silent({
    res <- I2C2(y = y, id = id, visit = visit, symmetric = FALSE, demean = FALSE)
  })
  expect_equal(res$lambda, 0.012814655436895599008)
})


test_that("Checking I2C2 twoway = FALSE", {
  expect_silent({
    res <- I2C2(y = y, id = id, visit = visit, twoway = FALSE)
  })
  expect_equal(res$lambda, 0.012814655436895599008)
})

test_that("Checking I2C2 twoway = FALSE, demean = FALSE", {
  expect_silent({
    res <- I2C2(y = y, id = id, visit = visit, twoway = FALSE, demean = FALSE)
  })
  expect_equal(res$lambda, 0.012814655436895599008)
})

test_that("Checking I2C2 twoway = FALSE, symmetric = TRUE", {
  expect_silent({
    res <- I2C2(y = y, id = id, visit = visit, twoway = FALSE, symmetric = TRUE)
  })
  expect_equal(res$lambda, 0.012814655436895576457)
})

test_that("Checking I2C2 twoway = FALSE, demean = FALSE, symmetric = TRUE", {
  expect_silent({
    res <- I2C2(y = y, id = id, visit = visit, twoway = FALSE,
                demean = FALSE, symmetric = TRUE)
  })
  expect_equal(res$lambda, 0.012814655436895427271)
})


# test_that("Checking I2C2 twoway = FALSE, symmetric = TRUE, truncate = TRUE", {
#   expect_silent({
#     res <- I2C2(y = y, id = id, visit = visit, twoway = FALSE, symmetric = TRUE,
#                 truncate = TRUE)
#   })
#   expect_equal(res$lambda, 0)
# })
#
# test_that("Checking I2C2 twoway = FALSE, demean = FALSE, symmetric = TRUE, truncate = TRUE", {
#   expect_silent({
#     res <- I2C2(y = y, id = id, visit = visit, twoway = FALSE,
#                 demean = FALSE, symmetric = TRUE, truncate = TRUE)
#   })
#   expect_equal(res$lambda, 0)
# })