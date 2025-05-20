# TODO test calibrate with method L-BFGS-B for a problem which is not computable
#   this way the optim_set will return the error value which needs to be finite
#   for L-BFGS-B to complete

test_that("fit to simple dataset", {
  tol <- 1e-5

  ## set up a scenario to create perfect fit data
  rs <- simulate(minnow_it)
  # modify scenario by setting parameter `kd` to quasi-random value
  tofit <- minnow_it %>% set_param(c(kd=0.01))

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

test_that("fit to dataset with replicates", {
  tol <- 1e-5

  ##
  ## set up a scenario to create perfect fit data
  ##
  rs <- minnow_it %>%
    set_times(rep(minnow_it@times, each=2)) %>%
    simulate()

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
  # create a single calibration set
  cs <- caliset(tofit, rs.ideal)

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

# TODO optim raises one warning about the selected method and then additional
#   ones originating from the dummy scenario, the test does not account for the former
test_that("failed simulations during fitting", {
  source(test_path("dummy.R"), local = TRUE)
  fail <- new("DummyScenario", simulate=function(...) stop("dummy scenario failed"))

  # simulation fails completely
  suppressWarnings( # suppress any additional warnings
    expect_warning(
      calibrate(fail, par=c("baz"=0), data=data.frame("t"=0:2, "a"=0), output="a", verbose=FALSE),
      "dummy scenario failed"
    )
  )

  # some runs are unstable
  instable <- new("DummyScenario", solver=function(...) {
    warning("planned warning")
    df <- data.frame(t=0:2, A=1)
    attr(df, "desolve_diagn") <- list(istate=c(-1, 100), rstate=1) # magic value from deSolve, cf. [num_info()]
    df
  })
  suppressWarnings(
    expect_warning(rs <- calibrate(instable, par=c("baz"=0), data=data.frame("t"=0:2, "A"=0),
                                   output="A", verbose=FALSE, method="Brent", lower=0, upper=1),
                  "num_info")
  )
  expect_equal(attr(rs, "desolve_diagn"), list(istate=c(-1, 100), rstate=1))
})

test_that("fit with weights", {
  # synthetic dataset #1, should be irrelevant due to small weights
  minnow_it %>%
    simulate() %>%
    dplyr::select(time, D) -> rs1
  # synthetic dataset #2, the only relevant one due to large weights
  minnow_it %>%
    set_param(c(kd=0.5)) %>%
    simulate() %>%
    dplyr::select(time, D) -> rs2

  # create list containing calibration sets
  cs <- list(
    caliset(minnow_it, rs1, weight=0.0001),
    caliset(minnow_it, rs2, weight=1000)
  )

  calibrate(cs,
            par=c(kd=minnow_it@param$kd),
            output="D",
            method="Brent",
            lower=0.001,
            upper=10,
            verbose=FALSE) -> calib

  expect_equal(calib$par[["kd"]],
               0.5,
               tolerance=1e-5)

})
test_that("invalid inputs: scenario", {
  sc <- new("EffectScenario")
  # data not a data.frame
  expect_error(calibrate(sc, data=1), "must be a data.frame")
  # data empty
  expect_error(calibrate(sc, data=data.frame()), "is empty")
  # output not in data
  df <- data.frame(t=0:3, foo=1, bar=2)
  expect_error(calibrate(sc, data=df, output="baz"), "not a column")
  # group by not a single character
  expect_error(calibrate(sc, data=df, output="foo", by=c("t", "foo")), "length one")
  expect_error(calibrate(sc, data=df, output="foo", by=c(1)), "must be a character")
  # ... not in data
  expect_error(calibrate(sc, data=df, output="foo", by="baz"), "not a column")
})

test_that("invalid inputs: calisets", {
  sc <- new("EffectScenario") %>% set_times(0:5) %>% set_param(c(foo=1, bar=2))
  cs <- caliset(sc, data.frame(t=0:5, bar=1))

  # data supplied
  expect_error(calibrate(cs, data=data.frame()), "cannot be used in combination")
  # not all elements are calisets
  expect_error(calibrate(list(cs, 1)), "only contain caliset")
  # par is non-numeric
  expect_error(calibrate(cs, par=sc), "must be a numeric")
  expect_error(calibrate(cs, par=c("foo"="b")), "must be a numeric")
  # not all elements in par are named
  expect_error(calibrate(cs, par=c(1)), "must be named")
  expect_error(calibrate(cs, par=c(foo=1, 2)), "must be named")
  # ... are actual parameters of the scenario
  expect_error(calibrate(cs, par=c(baz=0)), "not scenario parameters")
  # output missing
  expect_error(calibrate(cs, par=c(foo=0)), "output. is missing")
  # output invalid length
  expect_error(calibrate(cs, par=c(foo=0), output=character(0)), "output. must be of length one")
  expect_error(calibrate(cs, par=c(foo=0), output=c("a","b")), "output. must be of length one")
  # output not a string
  expect_error(calibrate(cs, par=c(foo=0), output=1), "output. must be a string")
  # output var missing from datasets
  suppressMessages(expect_error(calibrate(cs, par=c(foo=0), output="baz"), "missing from dataset"))
})

test_that("fit with error functions", {
  tol <- 1e-5
  rs <- simulate(minnow_it)
  tofit <- minnow_it %>% set_param(c(kd=0.01))

  # use an alternative error function
  tofit <- tofit %>% set_init(rs[2,c("D", "H")])
  calib <- calibrate(tofit,
                     par=c(kd=0.1),
                     data=rs[-1, -3],
                     output="D",
                     err_fun="log_sse",
                     method="Brent",
                     lower=0.001,
                     upper=10,
                     verbose=FALSE)
  expect_equal(calib$par[["kd"]],
               minnow_it@param$kd,
               tolerance=tol)

  # use a custom error function
  myerr <- function(obs, pred, ...) sum((log(obs) - log(pred))^2)
  calib2 <- calibrate(tofit, par=c(kd=0.1), data=rs[-1, -3],
                      output="D", err_fun=myerr, method="Brent",
                      lower=0.001, upper=10, verbose=FALSE)
  expect_equal(calib2$par[["kd"]],
               calib$par[["kd"]],
               tolerance=tol)
})

test_that("SSE errfun", {
  # basic use
  expect_equal(sse(c(1), c(1)), 0)
  expect_equal(sse(c(1), c(0)), 1)
  expect_equal(sse(c(2), c(0)), 4)
  expect_equal(sse(c(0), c(1)), 1)
  expect_equal(sse(c(0), c(2)), 4)
  expect_equal(sse(c(1, 1), c(1, 1)), 0)
  expect_equal(sse(c(2, 2), c(1, 1)), 2)
  expect_equal(sse(c(3, 3), c(1, 1)), 8)
  # weights
  expect_equal(sse(c(1), c(1), c(1)), 0)
  expect_equal(sse(c(3), c(1), c(1)), 4)
  expect_equal(sse(c(3), c(1), c(0.1)), 0.4)
  expect_equal(sse(c(3, 1), c(1, 1), c(0.1)), 0.4)
  # sizes of arguments dont match
  expect_error(sse(1, numeric(0)), "observed and predicted")
  expect_error(sse(1:3, 1), "observed and predicted")
  expect_error(sse(1, 1:3), "observed and predicted")
  expect_error(sse(1, 1, numeric(0)), "weights and observed")
  expect_error(sse(1, 1, 1:3), "weights and observed")
  # invalid/missing values
  expect_equal(sse(NA_real_, 1), NA_real_)
})

test_that("Log SSE errfun", {
  # basic use
  expect_equal(log_sse(1, 1), 0)
  expect_equal(log_sse(exp(1), 1), 1)
  # TODO how to deal with zeros in data?
  expect_equal(log_sse(c(1, 1), c(1, 1)), 0)
  expect_equal(log_sse(c(exp(1), exp(1)), c(1, 1)), 2)
  # weights
  expect_equal(log_sse(1, 1, 1), 0)
  expect_equal(log_sse(exp(1), 1, 1), 1)
  expect_equal(log_sse(exp(1), 1, 0.1), 0.1)
  # sizes of arguments dont match
  expect_error(log_sse(1, numeric(0)), "observed and predicted")
  expect_error(log_sse(1:3, 1), "observed and predicted")
  expect_error(log_sse(1, 1:3), "observed and predicted")
  expect_error(log_sse(1, 1, numeric(0)), "weights and observed")
  expect_error(log_sse(1, 1, 1:3), "weights and observed")
  # invalid/missing values
  expect_equal(log_sse(NA_real_, 1), NA_real_)
})

test_that("input check: fitted parameter", {
  # parameter to fit not set yet, process should succeed
  val <- minnow_it@param[["kd"]]
  sc <- minnow_it
  sc@param[["kd"]] <- NULL
  moptim <- function(...) list(convergence=0, par=list(kd=0))

  with_mocked_bindings(
    calibrate(sc, par=c(kd=val), data=data.frame(t=0:5, S=0), output="S", verbose=FALSE),
    optim=moptim
  )
  succeed()

  # generic scenario which has an empty param.req slot
  sc <- minnow_it
  sc@param.req <- character(0)
  with_mocked_bindings(
    calibrate(sc, par=c(kd=val), data=data.frame(t=0:5, S=0), output="S", verbose=FALSE),
    optim=moptim
  )
  succeed()

  # parameter does not belong to model
  sc <- minnow_it
  expect_error(with_mocked_bindings(
    calibrate(sc, par=c(foo=1), data=data.frame(t=0:5, S=0), output="S", verbose=FALSE),
    optim=moptim
  ),
  "not scenario parameters")
})


test_that("deprecated arguments", {
  sc <- minnow_it
  data <- data.frame(time=0:5, S=1)
  par <- c(kd = 0)
  moptim <- function(...) list(convergence=0, par=list(kd=0))

  # no lifecycle messages
  with_mocked_bindings(calibrate(sc, data=data, par=par, output="S", verbose=FALSE),
                       optim = moptim)
  succeed()

  # outdated arguments
  lifecycle::expect_deprecated(
    with_mocked_bindings(calibrate(sc, data=data, par=par, endpoint="S", verbose=FALSE),
                       optim = moptim)
  )
})
