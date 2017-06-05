

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

test_that("Checking data.frame", {
  expect_silent(check_id_visit(y = as.data.frame(y), id = id, visit = visit))
})

test_that("Checking char", {
  expect_error({
    yy = y
    class(yy) = "character"
    check_id_visit(y = yy, id = id, visit = visit)
    }, "y is not a numeric/integer/logical type")
})


test_that("Checking n == 1", {
  expect_error({
    yy = y
    yy = y[1,, drop = FALSE]
    iid = id[1]
    ivisit = visit[1]
    check_id_visit(y = yy, id = iid, visit = ivisit)
  }, "only one observation")
})

test_that("Checking non_matrix y", {
  expect_error({
    yy = y
    yy = y[1,, drop = TRUE]
    iid = id[1]
    ivisit = visit[1]
    check_id_visit(y = yy, id = iid, visit = ivisit)
  }, "argument is of length zero")
})

test_that("Checking non_matrix y", {
  expect_error({
    yy = array(y, dim = c(dim(y), 1))
    check_id_visit(y = yy, id = id, visit = visit)
  }, "y is not a matrix")
})