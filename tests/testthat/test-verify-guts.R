#
# Verification of the GUTS-SD model implementation by comparing results
# of this package's model with published results.
#


# GUTS-SD with scaled internal concentration Ci* and dose metric M=Ci*.
# This model is functionally identical to GUTS-RED-SD.
#
# Values are compared to the GUTS-RED-SD implementation which was verified
# using results from EFSA (2018), DOI: 10.2903/j.efsa.2018.5377
test_that("GUTS-SD scaled Ci, M=Ci", {

  # Code and parameters from EFSA (2018), Appendix E
  # > 01.GUTS-implementation-check.R
  exp <- data.frame(time=c(0, 1, 2, 3, 4, 4.0001, 5, 6, 7),
                    conc=c(rep(5, 5), rep(0, 4)))
  kD <- 0.3 # unit: [time^-1]
  hb <- 0 # background mortality rate [time^-1]
  bw <- 0.5 # unit: 1/[D]
  zw <- 2.5 # unit: [D]
  mw <- 2.5 # unit: [D]
  beta <- 2 # dimensionless

  # reproduces results shown in Figure 8, p. 37, from EFSA (2018)
  # constant exposure for 4 days

  efsa <- GUTS_RED_SD() %>%
    set_param(c(kd=kD, hb=hb, kk=bw, z=zw)) %>%
    set_exposure(exp)
  rf <- efsa %>% simulate(method="ode45", hmax=0.01)

  local <- GUTS_SD(scaled_ci=TRUE, dose_metric="Ci") %>%
    set_param(c(ke=kD, kk=bw, hb=hb, z=zw)) %>%
    set_exposure(exp)
  rs <- local %>%
    simulate(method="ode45", hmax=0.01) %>%
    dplyr::select(time, D=Ci, H, S)

  expect_equal(rs, rf, ignore_attr=TRUE, tolerance=1e-3)

  # reproduces selected results shown in Figure 9, p. 38, from EFSA (2018)
  # pulsed exposure from day 3 to 5
  exp <- data.frame(time=c(0:3, 3:5, 5:14),
                    conc=c(rep(0, 4), rep(50, 3), rep(0, 10))) # low concentration
  rf <- efsa %>%
    set_exposure(exp) %>%
    simulate(method="ode45", hmax=0.01)

  rs <- local %>%
    set_exposure(exp) %>%
    simulate(method="ode45", hmax=0.01) %>%
    dplyr::select(time, D=Ci, H, S)

  expect_equal(rs, rf, ignore_attr=TRUE, tolerance=1e-3)

  exp <- data.frame(time=c(0:3, 3:5, 5:14),
                    conc=c(rep(0, 4), rep(200, 3), rep(0, 10))) # high concentration
  rf <- efsa %>%
    set_exposure(exp) %>%
    simulate(method="ode45", hmax=0.01)

  rs <- local %>%
    set_exposure(exp) %>%
    simulate(method="ode45", hmax=0.01) %>%
    dplyr::select(time, D=Ci, H, S)

  expect_equal(rs, rf, ignore_attr=TRUE, tolerance=1e-3)
})

# GUTS-IT with scaled internal concentration Ci* and dose metric M=Ci*.
# This model is functionally identical to GUTS-RED-IT
#
# Values are compared to the GUTS-RED-IT implementation which was verified
# using results from EFSA (2018), DOI: 10.2903/j.efsa.2018.5377
test_that("GUTS-IT scaled Ci, M=Ci", {

  # Code and parameters from EFSA (2018), Appendix E
  # > 01.GUTS-implementation-check.R
  exp <- data.frame(time=c(0, 1, 2, 3, 4, 4.0001, 5, 6, 7),
                    conc=c(rep(5, 5), rep(0, 4)))
  kD <- 0.3 # unit: [time^-1]
  hb <- 0 # background mortality rate [time^-1]
  bw <- 0.5 # unit: 1/[D]
  zw <- 2.5 # unit: [D]
  mw <- 2.5 # unit: [D]
  beta <- 2 # dimensionless

  # reproduces results shown in Figure 8, p. 37, from EFSA (2018)
  # constant exposure for 4 days

  efsa <- GUTS_RED_IT() %>%
    set_param(c(kd=kD, hb=hb, alpha=mw, beta=beta)) %>%
    set_exposure(exp)
  rf <- efsa %>% simulate(method="ode45", hmax=0.01)

  local <- GUTS_IT(scaled_ci=TRUE, dose_metric="Ci") %>%
    set_param(c(ke=kD, hb=hb, alpha=mw, beta=beta)) %>%
    set_exposure(exp)
  rs <- local %>%
    simulate(method="ode45", hmax=0.01) %>%
    dplyr::select(time, D=Ci, H, S)

  expect_equal(rs, rf, ignore_attr=TRUE, tolerance=1e-3)

  # reproduces selected results shown in Figure 9, p. 38, from EFSA (2018)
  # pulsed exposure from day 3 to 5

  exp <- data.frame(time=c(0:3, 3:5, 5:14),
                    conc=c(rep(0, 4), rep(50, 3), rep(0, 10))) # low concentration
  rf <- efsa %>%
    set_exposure(exp) %>%
    simulate(method="ode45", hmax=0.01)

  rs <- local %>%
    set_exposure(exp) %>%
    simulate(method="ode45", hmax=0.01) %>%
    dplyr::select(time, D=Ci, H, S)

  expect_equal(rs, rf, ignore_attr=TRUE, tolerance=1e-3)

  exp <- data.frame(time=c(0:3, 3:5, 5:14),
                    conc=c(rep(0, 4), rep(200, 3), rep(0, 10))) # high concentration
  rf <- efsa %>%
    set_exposure(exp) %>%
    simulate(method="ode45", hmax=0.01)

  rs <- local %>%
    set_exposure(exp) %>%
    simulate(method="ode45", hmax=0.01) %>%
    dplyr::select(time, D=Ci, H, S)

  expect_equal(rs, rf, ignore_attr=TRUE, tolerance=1e-3)
})


# GUTS-SD with scaled internal concentration Ci* and dose metric M=Ci*.
# This model is functionally identical to GUTS-RED-SD.
#
# Values are compared to results reported by Jager at al. (2011), DOI: 10.1021/es103092a
# Figure 3, for the effect of naphtalene on fathead minnows. Numerical values
# were not reported by Jager et al., but results matched by visual comparison.
test_that("scaled Ci, M=Ci", {
  ## SD model

  # medium concentration of 47.2 uM
  rs1 <- GUTS_SD(scaled_ci=TRUE, dose_metric="Ci") %>%
    set_param(c( ke=5.53, kk=0.102, hb=0, z=44)) %>%
    set_exposure(data.frame(t=0, c=47.2), reset_times=FALSE) %>%
    set_times(seq(0, 4, 0.1)) %>%
    simulate(hmax=0.01)
  # manual inspection of results
  plot(rs1)
  expect_snapshot_value(rs1, style="serialize", ignore_attr=TRUE, tolerance=1e-3)

  # high concentration of 80.4 uM
  rs2 <- GUTS_SD(scaled_ci=TRUE, dose_metric="Ci") %>%
    set_param(c( ke=5.53, kk=0.102, hb=0, z=44)) %>%
    set_exposure(data.frame(t=0, c=80.4), reset_times=FALSE) %>%
    set_times(seq(0, 4, 0.1)) %>%
    simulate(hmax=0.01)
  # manual inspection of results
  # plot(rs2)
  expect_snapshot_value(rs2, style="serialize", ignore_attr=TRUE, tolerance=1e-3)

  ## IT model

  # TODO requires implementation of log-normal distributed thresholds z (see SI of Jager et al (2011))
  # medium concentration of 47.2 uM
  #rs3 <- GUTS_IT(scaled_ci=TRUE, dose_metric="Ci") %>%
  #  set_param(c( ke=1.48, hb=0, median=46.4, spred=10.4)) %>%
  #  set_exposure(data.frame(t=0, c=47.2), reset_times=FALSE) %>%
  #  set_times(seq(0, 4, 0.1)) %>%
  #  simulate(hmax=0.01)
  ## manual inspection of results
  ## plot(rs3)
  #expect_snapshot_value(rs1, style="serialize", ignore_attr=TRUE, tolerance=1e-3)

  # TODO test high concentration
})


# GUTS-SD with dose metric M=Cw.
# This model is functionally identical to GUTS-RED-SD for sufficiently large `kd`
#
# Verification by plausibility check against GUTS-RED-SD
test_that("GUTS-SD M=Cw", {

  # Code and parameters from EFSA (2018), Appendix E
  # > 01.GUTS-implementation-check.R
  exp <- data.frame(time=c(0, 1, 2, 3, 4, 4.0001, 5, 6, 7),
                    conc=c(rep(5, 5), rep(0, 4)))
  kD <- 100000 # unit: [time^-1], value was modified for this test by expert decision
  hb <- 0 # background mortality rate [time^-1]
  bw <- 0.5 # unit: 1/[D]
  zw <- 2.5 # unit: [D]
  mw <- 2.5 # unit: [D]
  beta <- 2 # dimensionless

  # reproduces results shown in Figure 8, p. 37, from EFSA (2018)
  # constant exposure for 4 days
  rf <- GUTS_RED_SD() %>%
    set_param(c(kd=kD, hb=hb, kk=bw, z=zw)) %>%
    set_exposure(exp) %>%
    simulate(method="ode45", hmax=0.001, rtol=1e-8, maxsteps=1e7) %>%
    dplyr::select(!D)

  rs <- GUTS_SD(dose_metric="Cw") %>%
    set_param(c(kk=bw, hb=hb, z=zw)) %>%
    set_exposure(exp) %>%
    simulate(method="ode45", hmax=0.001) %>%
    dplyr::select(time, H, S) # D is not calculated for this model variant

  expect_equal(rs, rf, ignore_attr=TRUE, tolerance=1e-3)
})


# TODO Some model variants not explicitly covered yet, needs additional literature sources
# non-scaled Ci, dose metrics M=D, Ci
# scaled Ci, dose metric M=D



