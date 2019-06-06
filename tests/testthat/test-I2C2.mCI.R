context("Testing I2C2 mCI")

suppressWarnings(RNGversion("3.5.0"))
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
  expect_equal(mean(res$lambda), 0.068824712183468320092)
  names(res$CI) = NULL
  expect_equal(res$CI,
               c(0.01604998393998991954, 0.12056360187639839077 )
  )
})

test_that("I2C2.mCI default, 4 cores", {
  expect_silent({
    res <- I2C2.mcCI(y = y, id = id, visit = visit, cores = 4)
  })
  expect_equal(mean(res$lambda), 0.068824712183468320092)
  names(res$CI) = NULL
  expect_equal(res$CI,
               c(0.01604998393998991954, 0.12056360187639839077  ))
})


test_that("I2C2.mCI default, newseed", {
  expect_silent({
    res <- I2C2.mcCI(y = y, id = id, visit = visit, rseed = 20172343)
  })
  expect_equal(mean(res$lambda), 0.065339042996900145721)
  names(res$CI) = NULL
  expect_equal(res$CI,
               c(0.015706886883321103526, 0.112632309305550792899 ))
})