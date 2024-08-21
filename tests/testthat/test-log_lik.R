test_that("log-likelihood works", {

  npars <- 2
  obs <- c(12, 38, 92, 176, 176, 627, 1283, 2640)
  pred <- c(5.0, 21.256, 55.586, 144.162, 144.845, 574.043, 1323.999, 2632.258)
  res <- log_lik(npars = npars,
                 obs = obs,
                 pred = pred)

  expect_equal(res, -39.074, tolerance = 10e-3)

})


test_that("log-likelihood gives expected error", {

  npars <- 2
  obs <- c(12, 38, 92, 176, 176, 627, 1283, 2640, 666)
  pred <- c(5.0, 21.256, 55.586, 144.162,  144.845,  574.043, 1323.999, 2632.258)

  expect_error(log_lik(npars = npars,
                       obs = obs,
                       pred = pred))

  expect_error(log_lik(npars = "test",
                       obs = obs,
                       pred = pred))
})
