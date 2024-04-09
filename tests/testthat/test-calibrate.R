
test_that("fit basic scenario", {
  tol <- 1e-5

  ##
  ## set up a scenario to create perfect fit data
  ##
  minnow_it %>% simulate() -> rs
  minnow_it %>% set_param(c(kd=0.1)) -> minnow_tofit

  calibrate(minnow_tofit, c(kd=0.1), rs[,-3], "D", as.tibble=FALSE,
            method="Brent", lower=0.001, upper=10) -> calib
  expect_equal(calib$par[["kd"]], minnow_it@param$kd, tolerance=tol)

  # expect a warning with Nelder-Mead but result should be good nonetheless
  expect_warning(calibrate(minnow_tofit, c(kd=0.1), rs[,-3], "D", as.tibble=FALSE,
                           control=list(reltol=1e-12)) -> calib)
  expect_equal(calib$par[["kd"]], minnow_it@param$kd, tolerance=tol)

  ##
  ## fit to several noisy replicates
  ##
  minnow_it %>% simulate() -> rs.ideal
  minnow_it %>% set_param(c(kd=1.1)) %>% simulate() -> rs.lo
  minnow_it %>% set_param(c(kd=1.33)) %>% simulate() -> rs.hi

  set.seed(123)
  df <- cbind(time=rs.lo[,1],
              lo=rs.lo[,2], # bias to low kd
              hi=rs.hi[,2]) # bias to high kd
  # add noisy replicates
  for(i in seq(10))
    df <- cbind(df, noise=rs.ideal[,2] + stats::rnorm(nrow(rs.ideal),sd=0.05))
  # fix first row, should be zero by definition
  df[1,] <- c(0)
  # fit to data
  calibrate(minnow_tofit, c(kd=0.1), df, "D", as.tibble=FALSE,
            method="Brent", lower=0.001, upper=10) -> calib
  # lower precision, but it's based on noisy data, so it is OK
  expect_equal(calib$par[["kd"]], minnow_it@param$kd, tolerance=0.01)

  ##
  ## fit two parameters to the previous data
  ##
  calibrate(minnow_tofit, c(hb=0.1,kd=0.1), df, "D", as.tibble=FALSE) -> calib
  # result of Nelder-Mead fit is sensitive to start conditions
  # hb is irrelevant because it has no influence on 'D' (internal damage/conc)
  expect_equal(calib$par[["kd"]], minnow_it@param$kd, tolerance=0.01)

  # fit with box constraints
  calibrate(minnow_tofit, c(hb=0.1,kd=1), df, "D", as.tibble=FALSE,
            method="L-BFGS-B", lower=c(0,0.001), upper=c(10,10)) -> calib
  expect_equal(calib$par[["kd"]], minnow_it@param$kd, tolerance=0.01)
})

test_that("CalibrationSet", {

})
