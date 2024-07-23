
# excerpt from morse function Surv.SD_Cext() to calculate internal concentrations (D(t))
morse.Cint <- function(Cw, time, kd) {
  time.prec = dplyr::lag(time, 1) ; time.prec[1] = time[1] #   time[1] = tprec[1]
  diff.int = (exp(time %*% t(kd)) + exp(time.prec %*% t(kd)) )*Cw/2 * (time-time.prec) #OK time[1]-tprec[1] = 0
  kd * exp(-kd %*% t(time)) * t(apply(diff.int, 2, cumsum)) # internal damage D(t)
}

gutsef <- function(sc, fac) {
  effect(sc, factor=fac, ep_only=TRUE) %>% unname()
}

gutsmf <- function(sc, lvl) {
  epx(sc, level=lvl, effect_tolerance=0.00001) -> rs
  rs[[1,2]]
}

morsemf <- function(fit, X) {
  morse::MFx(fit, data.frame(time=0:4,conc=c(1)),X=X,mcmc_size=1,accuracy=0.0001,quiet=T) -> mf
  mf$df_MFx[1,2]
}

test_that("GUTS-RED constant exposure", {
  skip_on_cran()

  # GUTS model verification as conducted in
  # EFSA Scientific Opinion on TKTD models, pp. 36
  # doi:10.2903/j.efsa.2018.5377

  #
  # Begin EFSA code: 01.GUTS-implementation-check.R
  #
  exp <- data.frame(seq(0,7,0.01),
                    c(rep(5,401), rep(0,300)))
  colnames(exp) <- c("time","conc")
  kD <- 0.3 # unit: [time^-1]
  hb <- 0 # background mortality rate [time^-1]
  bw <- 0.5 # unit: 1/[D]
  zw <- 2.5 # unit: [D]
  mw <- 2.5 # unit: [D]
  beta <- 2 # dimensionless
  ## Bugfix: EFSA's interpolation uses f=0 which results in increase of D(t)
  ##         after pulse has ended
  efsa.Cint <- function(t,y,parms){
    # parms[1] = kd: dominante rate constant
    # concentration profile interpolated in exp.approx
    dy1 <- parms[1]*(exp.approx(t) - y[1])
    list(dy1)
  }
  exp.approx <- approxfun(exp$time, exp$conc, rule=2, f=1, method="constant")
  Cint_simu_0 <- deSolve::lsoda(y=0, times=exp$time, func=efsa.Cint, parms=kD)
  #
  # EFSA code end
  #

  # internal concentration calculated on an approximation
  Cint_morse <- morse.Cint(exp$conc,exp$time,kD)

  # Compare with morse's standard prediction
  GUTS_RED_IT() %>%
    set_param(c(kd=kD,hb=hb,alpha=mw,beta=beta)) %>%
    set_exposure(exp) %>%
    simulate(approx="constant") -> git
  GUTS_RED_SD() %>%
    set_param(c(kd=kD,hb=hb,kk=bw,z=zw)) %>%
    set_exposure(exp) %>%
    simulate(approx="constant") -> gsd

  # NMAE
  expect_lt(nmae(Cint_morse[1,], git$D), 1e-5, "IT:D approx")
  expect_lt(nmae(Cint_morse[1,], gsd$D), 1e-5, "SD:D approx")
  expect_lt(nmae(Cint_simu_0[,2], git$D), 1e-5, "IT:D approx")
  expect_lt(nmae(Cint_simu_0[,2], gsd$D), 1e-5, "SD:D approx")
  # RMSE
  expect_lt(nrmse(Cint_morse[1,], git$D), 1e-5, "IT:D approx")
  expect_lt(nrmse(Cint_morse[1,], gsd$D), 1e-5, "SD:D approx")
  expect_lt(nrmse(Cint_simu_0[,2], git$D), 1e-5, "IT:D approx")
  expect_lt(nrmse(Cint_simu_0[,2], gsd$D), 1e-5, "SD:D approx")
})

test_that("GUTS-RED extreme cases", {
  skip_on_cran()

  # GUTS model verification as conducted in
  # EFSA Scientific Opinion on TKTD models, pp. 36
  # doi:10.2903/j.efsa.2018.5377

  #
  # Begin EFSA code: 01.GUTS-implementation-check.R
  # Zero exposure (conc=0)
  #
  exp <- data.frame(seq(0,7,0.01), rep(0,701))
  colnames(exp) <- c("time","conc")
  kD <- 0.3 # unit: [time^-1]
  hb <- 0 # background mortality rate [time^-1]
  bw <- 0.5 # unit: 1/[D]
  zw <- 2.5 # unit: [D]
  mw <- 2.5 # unit: [D]
  beta <- 2 # dimensionless
  #
  # EFSA code end
  #

  # internal concentration calculated on an approximation
  Cint_morse <- morse.Cint(exp$conc,exp$time,kD)

  GUTS_RED_IT() %>%
    set_param(c(kd=kD,hb=hb,alpha=mw,beta=beta)) %>%
    set_exposure(exp) %>%
    simulate(approx="constant") -> git
  GUTS_RED_SD() %>%
    set_param(c(kd=kD,hb=hb,kk=bw,z=zw)) %>%
    set_exposure(exp) %>%
    simulate(approx="constant") -> gsd

  expect_lt(nmae(Cint_morse[1,], git$D), 1e-5, "IT:D approx")
  expect_lt(nrmse(Cint_morse[1,], git$D), 1e-5, "IT:D approx")
  expect_lt(nmae(Cint_morse[1,], gsd$D), 1e-5, "SD:D approx")
  expect_lt(nrmse(Cint_morse[1,], gsd$D), 1e-5, "SD:D approx")

  #
  # Begin EFSA code: 01.GUTS-implementation-check.R
  # High exposure over 4 days (conc=100)
  #
  exp <- data.frame(seq(0,7,0.01),
                    c(rep(100,401), rep(0,300)))
  colnames(exp) <- c("time","conc")
  #
  # EFSA code end
  #

  Cint_morse <- morse.Cint(exp$conc,exp$time,kD)

  GUTS_RED_IT() %>%
    set_param(c(kd=kD,hb=hb,alpha=mw,beta=beta)) %>%
    set_exposure(exp) %>%
    simulate(approx="constant") -> git
  GUTS_RED_SD() %>%
    set_param(c(kd=kD,hb=hb,kk=bw,z=zw)) %>%
    set_exposure(exp) %>%
    simulate(approx="constant") -> gsd

  expect_lt(nmae(Cint_morse[1,], git$D), 1e-5, "IT:D approx")
  expect_lt(nrmse(Cint_morse[1,], git$D), 1e-5, "IT:D approx")
  expect_lt(nmae(Cint_morse[1,], gsd$D), 1e-5, "SD:D approx")
  expect_lt(nrmse(Cint_morse[1,], gsd$D), 1e-5, "SD:D approx")

  #
  # Begin EFSA code: 01.GUTS-implementation-check.R
  # Only background mortality (conc=0, hb=0.05)
  #
  skip("EFSA implementation check missing")
  # Concentration profile is identical to zero concentration case

  #
  # EFSA code end
  #
})

test_that("GUTS-RED ring-test C", {
  skip_if_not_installed("morse")

  # the morse rds files are too large to put them in a CRAN source package
  rds_file <- test_path(file.path("..", "minnow_fit.sd.rds"))
  skip_if_not(file.exists(rds_file), "morse GUTS-RED-SD parameter file unavailable")

  # allowed tolerance between morse's MFx estimates and ours.
  tol <- 1e-3

  # GUTS-RED-SD
  fit <- readRDS(rds_file)
  msd <- minnow_sd
  msd@param$hb <- 0 # for compatibility reasons
  expect_equal(0.05, gutsef(msd, morsemf(fit, 5)), tolerance=tol)
  expect_equal(0.1, gutsef(msd, morsemf(fit, 10)), tolerance=tol)
  expect_equal(0.2, gutsef(msd, morsemf(fit, 20)), tolerance=tol)
  expect_equal(0.5, gutsef(msd, morsemf(fit, 50)), tolerance=tol)

  # the morse rds files are too large to put them in a CRAN source package
  rds_file <- test_path(file.path("..", "minnow_fit.it.rds"))
  skip_if_not(file.exists(rds_file), "morse GUTS-RED-IT parameter file unavailable")

  # GUTS-RED-IT
  fit <- readRDS(rds_file)
  mit <- minnow_it
  mit@param$hb <- 0 # for compatibility reasons

  expect_equal(0.05, gutsef(mit, morsemf(fit, 5)), tolerance=tol)
  expect_equal(0.1, gutsef(mit, morsemf(fit, 10)), tolerance=tol)
  expect_equal(0.2, gutsef(mit, morsemf(fit, 20)), tolerance=tol)
  expect_equal(0.5, gutsef(mit, morsemf(fit, 50)), tolerance=tol)
})


test_that("GUTS-RED ring-test C", {
  skip_if_not_installed("morse")

  # the morse rds files are too large to put them in a CRAN source package
  rds_file <- test_path(file.path("..", "minnow_fit.sd.rds"))
  skip_if_not(file.exists(rds_file), "morse GUTS-RED-SD parameter file unavailable")

  # allowed tolerance between morse's MFx estimates and ours.
  tol <- 0.001

  # GUTS-RED-SD
  fit <- readRDS(rds_file)
  msd <- minnow_sd
  msd@param$hb <- 0 # for compatibility reasons

  expect_equal(morsemf(fit, 5),  gutsmf(msd, 5), tolerance=tol)
  expect_equal(morsemf(fit, 10), gutsmf(msd, 10), tolerance=tol)
  expect_equal(morsemf(fit, 20), gutsmf(msd, 20), tolerance=tol)
  expect_equal(morsemf(fit, 50), gutsmf(msd, 50), tolerance=tol)

  # the morse rds files are too large to put them in a CRAN source package
  rds_file <- test_path(file.path("..", "minnow_fit.it.rds"))
  skip_if_not(file.exists(rds_file), "morse GUTS-RED-SD parameter file unavailable")

  # GUTS-RED-IT
  fit <- readRDS(rds_file)
  mit <- minnow_it
  mit@param$hb <- 0 # for compatibility reasons

  expect_equal(morsemf(fit, 5),  gutsmf(mit, 5), tolerance=tol)
  expect_equal(morsemf(fit, 10), gutsmf(mit, 10), tolerance=tol)
  expect_equal(morsemf(fit, 20), gutsmf(mit, 20), tolerance=tol)
  expect_equal(morsemf(fit, 50), gutsmf(mit, 50), tolerance=tol)
})
