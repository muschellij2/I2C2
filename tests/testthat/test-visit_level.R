context("Testing I2C2 Original")

set.seed(20170602)
id = c(1:10, 10:1)
visit = rep(1:2, each = 10)
visit = as.character(visit)
n = length(id)
p = 100
y = matrix(rnorm(n * p), nrow = n, ncol = p)
uvisit = unique(visit)
nvisit = length(uvisit)
visit_means = matrix(rpois(nvisit * p, lambda = 5), nrow = nvisit, ncol = p)
rownames(visit_means) = uvisit
mat = matrix(0, nrow = n, ncol = p)
for (ivisit in uvisit) {
  ind =  visit == ivisit
  mat[ ind, ] = matrix(visit_means[ivisit, ], nrow = sum(ind),
                       ncol = p, byrow = TRUE)
}
y2 = y + mat

test_that("Constructing data with visit-level means", {

  vy = colMeans(y[ visit == "1", ])
  vy2 = colMeans(y2[ visit  == "1",])
  testthat::expect_equal(visit_means["1",] + vy, vy2)

  vy = colMeans(y[ visit == "2", ])
  vy2 = colMeans(y2[ visit  == "2",])
  testthat::expect_equal(visit_means["2",] + vy, vy2)
})

test_that("Constructing data with visit-level means", {

  zeroes =  rep(0, p)
  y_visit_means = rbind(colMeans(y[ visit == 1,]),
                        colMeans(y[ visit == 2,]))
  rownames(y_visit_means) = c("1", "2")

  y2_visit_means = rbind(colMeans(y2[ visit == 1,]),
                         colMeans(y2[ visit == 2,]))
  rownames(y2_visit_means) = c("1", "2")
  testthat::expect_equal(y_visit_means + visit_means, y2_visit_means)

  oneway = demean_matrix(y = y2, visit = visit, twoway = FALSE, tol = 0)
  twoway = demean_matrix(y = y2, visit = visit, twoway = TRUE, tol = 0)

  colMeans((oneway - twoway)[ visit == "1",])
  colMeans((oneway - twoway)[ visit == "2",])
  testthat::expect_equal(colMeans(twoway), zeroes)
  testthat::expect_equal(colMeans(twoway[ visit == "2", ]), zeroes)
  testthat::expect_equal(colMeans(twoway[ visit == "1", ]), zeroes)

  testthat::expect_equal(colMeans(oneway), zeroes)
  testthat::expect_equal(colMeans(oneway[ visit == "2", ]), zeroes)
  testthat::expect_equal(colMeans(oneway[ visit == "1", ]), zeroes)

  testthat::expect_equal(
    colMeans(y[ visit == "1", ]) + visit_means[ "1", ],
    colMeans(y2[ visit == "1", ])
  )



  testthat::expect_equal(
    colMeans(y[ visit == "2", ]) + visit_means[ "2", ],
    colMeans(y2[ visit == "2", ])
  )


  colMeans(y2[ visit == "2", ])

  x = colMeans(t(t(visit_means) + colMeans(y)))
  colMeans()
  colMeans(oneway[ visit == "1", ])
  colMeans(oneway)
  colMeans(oneway[ visit == "2", ])
  colMeans(oneway[ visit == "1", ])

  (visit_means["1",] + visit_means["2",])/2 + colMeans(y)

  colMeans(y[ visit == "1",])

})



test_that("Checking I2C2.original default truncate = TRUE", {

  expect_silent({
    res <- I2C2.original(y = y2, id = id, visit = visit, trun = TRUE)
  })
  expect_equal(res$lambda, 0.066936209335110546936)

  expect_silent({
    res <- I2C2.original(y = y, id = id, visit = visit, trun = TRUE)
  })
  expect_equal(res$lambda, 0.066936209335110546936)

})

test_that("Checking I2C2 default truncate = TRUE", {

  expect_silent({
    res <- I2C2(y = y2, id = id, visit = visit,
                demean = TRUE)
  })
  expect_equal(res$lambda, 0.066936209335110546936)


  expect_silent({
    res <- I2C2(y = y2, id = id, visit = visit,
                twoway = FALSE)
  })
  # expect_equal(res$lambda, 0.066936209335110546936)


})

