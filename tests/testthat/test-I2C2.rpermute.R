context("Testing I2C2 permute")

set.seed(20170602)
id = c(1:10, 10:1)
visit = rep(1:2, each = 10)
visit = as.character(visit)
n = length(id)
p = 100
y = matrix(rnorm(n * p), nrow = n, ncol = p)

test_that("I2C2.rpermute default", {
  expect_silent({
    res <- I2C2.rpermute(y = y, id = id, visit = visit)
  })
  expect_equal(res, 0.066607605435032982144)
})

test_that("I2C2.rpermute diffseed", {
  expect_silent({
    res <- I2C2.rpermute(y = y, id = id, visit = visit, s = 5)
  })
  expect_equal(res, -0.015268850309510527061)
})

test_that("I2C2.rpermute diffseed passthrough", {
  expect_silent({
    res <- I2C2.rpermute(y = y, id = id, visit = visit, s = 5, truncate = TRUE)
  })
  expect_equal(res, 0)
})
