context("Testing I2C2 mCI")

set.seed(20170602)
id = c(1:10, 10:1)
visit = rep(1:2, each = 10)
visit = as.character(visit)
n = length(id)
p = 100
y = matrix(rnorm(n * p), nrow = n, ncol = p)

test_that("I2C2.mCI default", {
  expect_silent({
    res <- I2C2.mcCI(y = y, id = id, visit = visit)
  })
  expect_equal(mean(res$lambda), 0.072324058227788867725)
  names(res$CI) = NULL
  expect_equal(res$CI,
               c(0.016926506013901491954, 0.126414851980683251575 )
  )
})

test_that("I2C2.mCI default, 4 cores", {
  expect_silent({
    res <- I2C2.mcCI(y = y, id = id, visit = visit, cores = 4)
  })
  expect_equal(mean(res$lambda), 0.072324058227788867725)
  names(res$CI) = NULL
  expect_equal(res$CI,
               c(0.016926506013901491954, 0.126414851980683251575 ))
})


test_that("I2C2.mCI default, newseed", {
  expect_silent({
    res <- I2C2.mcCI(y = y, id = id, visit = visit, rseed = 20172343)
  })
  expect_equal(mean(res$lambda), 0.068677239267206743878)
  names(res$CI) = NULL
  expect_equal(res$CI,
               c(0.0165642985940885, 0.11815000807091))
})