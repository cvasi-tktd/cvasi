test_that("fit to simple dataset", {
  tol <- 1e-5

  ##
  ## set up a scenario to create perfect fit data
  ##
  rs <- simulate(minnow_it)

  # modify scenario by setting parameter `kd` to quasi-random value
  tofit <- minnow_it %>% set_param(c(kd=0.1))

  # calibrate modified scenario on synthetic data
  calib <- calibrate(tofit,
                     par=c(kd=0.1),
                     data=rs[,-3],
                     output="D",
                     method="Brent",
                     lower=0.001,
                     upper=10,
                     verbose=FALSE)

  expect_equal(calib$par[["kd"]],
               minnow_it@param$kd,
               tolerance=tol)

  # expect a warning with Nelder-Mead but result should be good nonetheless
  expect_warning(calibrate(tofit,
                           par=c(kd=0.1),
                           data=rs[,-3],
                           output="D",
                           control=list(reltol=1e-12),
                           verbose=FALSE) -> calib)
  expect_equal(calib$par[["kd"]],
               minnow_it@param$kd,
               tolerance=tol)
})

test_that("fit to complex dataset", {
  ##
  ## fit a parameter to several synthetic datasets created using
  ## various values for parameter `kd`
  ##
  minnow_it %>%
    simulate() -> rs.ideal # original kd=1.2296
  minnow_it %>%
    set_param(c(kd=1.1)) %>%
    simulate() %>%
    dplyr::mutate(trial="low") -> rs.lo
  minnow_it %>%
    set_param(c(kd=1.33)) %>%
    simulate() %>%
    dplyr::mutate(trial="high")  -> rs.hi

  set.seed(123)
  df <- dplyr::bind_rows(rs.lo, rs.hi)

  # add noisy replicates
  for(i in seq(10)) {
    rs.ideal %>%
      dplyr::mutate(D=D + stats::rnorm(dplyr::n(), sd=0.05), trial=as.character(i)) %>%
      dplyr::mutate(D=pmax(D, 0)) %>%
      dplyr::bind_rows(df) -> df
  }

  # modify scenario by setting parameter `kd` to quasi-random value
  tofit <- minnow_it %>% set_param(c(kd=0.1))
  # fit to data
  calibrate(tofit,
            par=c(kd=0.1),
            data=df,
            output="D",
            by="trial",
            method="Brent",
            lower=0.001,
            upper=10,
            verbose=FALSE) -> calib
  # we have to use lower precision for comparison purposes, but result is
  # derived from noisy data, so this is OK
  expect_equal(calib$par[["kd"]], minnow_it@param$kd, tolerance=0.01)


  ##
  ## fit two parameters to the previous data
  ##
  #suppressWarnings(
  #  calibrate(tofit,
  #    par=c(hb=0.1, kd=0.1),
  #    data=df,
  #    by="trial",
  #    output="D",
  #    verbose=FALSE)) -> calib
  ## result of Nelder-Mead fit is sensitive to start conditions
  ## hb is irrelevant because it has no influence on 'D' (internal damage/conc)
  #expect_equal(calib$par[["kd"]],
  #             minnow_it@param$kd,
  #             tolerance=0.01)


  # fit with box constraints
  #calibrate(tofit,
  #          par=c(hb=0.1, kd=1),
  #          data=df,
  #          by="trial",
  #          output="D",
  #          method="L-BFGS-B",
  #          lower=c(0,0.001),
  #          upper=c(10,10),
  #          verbose=FALSE) -> calib
  #expect_equal(calib$par[["kd"]],
  #             minnow_it@param$kd,
  #             tolerance=0.01)
})


test_that("fit to calibration set", {
  ## fit a parameter to several synthetic datasets created using
  ## various values for parameter `kd`
  minnow_it %>%
    simulate() %>%
    dplyr::select(time, D) -> rs.ideal

  # modify scenario by setting parameter `kd` to quasi-random value
  tofit <- minnow_it %>% set_param(c(kd=0.1))
  # create list containing a single calibration set
  cs <- list(
    caliset(tofit, rs.ideal)
  )

  calibrate(cs,
            par=c(kd=0.1),
            output="D",
            method="Brent",
            lower=0.001,
            upper=10,
            verbose=FALSE) -> calib

  expect_equal(calib$par[["kd"]],
               minnow_it@param$kd,
               tolerance=1e-5)


  # create additional synthetic data using different values for `kd`
  minnow_it %>%
    set_param(c(kd=1.1296)) %>%
    simulate() %>%
    dplyr::select(time, D) -> rs.lo
  minnow_it %>%
    set_param(c(kd=1.3296)) %>%
    simulate() %>%
    dplyr::select(time, D) -> rs.hi

  # create list of several calibration sets
  cs <- list(
    caliset(tofit, rs.ideal),
    caliset(tofit, rs.lo),
    caliset(tofit, rs.hi)
  )

  calibrate(cs,
            par=c(kd=0.1),
            output="D",
            method="Brent",
            lower=0.001,
            upper=10,
            verbose=FALSE) -> calib

  expect_equal(calib$par[["kd"]],
               minnow_it@param$kd,
               tolerance=0.01)
})

test_that("failed simulations during fitting", {
  source(test_path("class-DummyScenario.R"))

  fail <- DummyFails()
  fail@param$foo <- 1
  fail@param.req <- c("foo")
  # simulation fails completely
  suppressWarnings( # suppress any additional warnings
    expect_warning(
      calibrate(fail,
      par=c("foo"=0),
      data=data.frame("t"=0, "a"=0),
      output="a",
      verbose=FALSE)
    )
  )

})
