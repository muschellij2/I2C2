context("Testing I2C2 mCI")

set.seed(20170602)
id = c(1:10, 10:1)
visit = rep(1:2, each = 10)
visit = as.character(visit)
n = length(id)
p = 100
y = matrix(rnorm(n * p), nrow = n, ncol = p)

test_that("I2C2.mcNulldist default", {
  expect_silent({
    res <- I2C2.mcNulldist(y = y, id = id, visit = visit)
  })
  expect_equal(mean(res$lambda), 0.049434565167522703799)
})

test_that("I2C2.mcNulldist diffseed", {
  expect_silent({
    res <- I2C2.mcNulldist(y = y, id = id, visit = visit, rseed = 100)
  })
  expect_equal(mean(res$lambda), 0.051689989533292533819)
})
