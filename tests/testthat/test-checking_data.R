

context("Testing checking of ids and matrix")

set.seed(20170602)
id = c(1:10, 10:1)
visit = rep(1:2, each = 10)
visit = as.character(visit)
n = length(id)
p = 100
y = matrix(rnorm(n * p), nrow = n, ncol = p)

test_that("Checking ID Visit", {
  expect_silent(check_id_visit(y = y, id = id, visit = visit))
  expect_error(check_id_visit(y = y, id = id[-1], visit = visit))
  expect_error(check_id_visit(y = y, id = id, visit = visit[-1]))
})

test_that("Checking I2C2 default", {
  expect_silent({
    res <- I2C2(y = y, id = id, visit = visit)
    })
  expect_equal(res$lambda, 0.070393118270377946777)
})
