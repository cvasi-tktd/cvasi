test_that("Constructor", {
  expect_equal(GUTS_SD(scaled_ci=TRUE)@scaled_ci, TRUE)
  expect_equal(GUTS_SD(scaled_ci=FALSE)@scaled_ci, FALSE)
  expect_equal(GUTS_SD(dose_metric="D")@dose_metric, "D")
  expect_equal(GUTS_SD(dose_metric="Ci")@dose_metric, "Ci")
  expect_equal(GUTS_SD(dose_metric="Cw")@dose_metric, "Cw")
  expect_equal(is(GUTS_SD())[1], "GutsSd")

  expect_equal(GUTS_IT(scaled_ci=TRUE)@scaled_ci, TRUE)
  expect_equal(GUTS_IT(scaled_ci=FALSE)@scaled_ci, FALSE)
  expect_equal(GUTS_IT(dose_metric="D")@dose_metric, "D")
  expect_equal(GUTS_IT(dose_metric="Ci")@dose_metric, "Ci")
  expect_equal(GUTS_IT(dose_metric="Cw")@dose_metric, "Cw")
  expect_equal(is(GUTS_IT())[1], "GutsIt")
})

test_that("SD: parameters", {
  #
  # Non-scaled Ci
  #
  expect_equal(GUTS_SD(scaled_ci=FALSE, dose_metric="D")@param.req, c("ki", "ke", "kr", "kk", "hb", "z"))
  expect_equal(GUTS_SD(scaled_ci=FALSE, dose_metric="Ci")@param.req, c("ki", "ke", "kk", "hb", "z"))
  expect_equal(GUTS_SD(scaled_ci=FALSE, dose_metric="Cw")@param.req, c("kk", "hb", "z"))

  expect_equal(GUTS_IT(scaled_ci=FALSE, dose_metric="D")@param.req, c("ki", "ke", "kr", "hb", "alpha", "beta"))
  expect_equal(GUTS_IT(scaled_ci=FALSE, dose_metric="Ci")@param.req, c("ki", "ke", "hb", "alpha", "beta"))
  expect_equal(GUTS_IT(scaled_ci=FALSE, dose_metric="Cw")@param.req, c("hb", "alpha", "beta"))

  #
  # Scaled Ci
  #
  expect_equal(GUTS_SD(scaled_ci=TRUE, dose_metric="D")@param.req, c("ke", "Kiw", "kr", "kk", "hb", "z"))
  expect_equal(GUTS_SD(scaled_ci=TRUE, dose_metric="Ci")@param.req, c("ke", "Kiw", "kk", "hb", "z"))
  expect_equal(GUTS_IT(scaled_ci=TRUE, dose_metric="D")@param.req, c("ke", "Kiw", "kr", "hb", "alpha", "beta"))
  expect_equal(GUTS_IT(scaled_ci=TRUE, dose_metric="Ci")@param.req, c("ke", "Kiw", "hb", "alpha", "beta"))
  # default value for Kiw
  expect_equal(GUTS_SD(scaled_ci=TRUE, dose_metric="D")@param[["Kiw"]], 1)
  expect_equal(GUTS_SD(scaled_ci=TRUE, dose_metric="Ci")@param[["Kiw"]], 1)
  expect_equal(GUTS_IT(scaled_ci=TRUE, dose_metric="D")@param[["Kiw"]], 1)
  expect_equal(GUTS_IT(scaled_ci=TRUE, dose_metric="Ci")@param[["Kiw"]], 1)
  # invalid combination of arguments
  expect_error(GUTS_SD(scaled_ci=TRUE, dose_metric="Cw"))
  expect_error(GUTS_IT(scaled_ci=TRUE, dose_metric="Cw"))
})

# Tests the basic behavior of the model, checks for plausible changes in state
# variables and concrete values where possible
# Basis for tests: GUTS-SD with non-scaled Ci, and dose metric M=D
test_that("SD: solver", {
  tol <- 1e-5
  base <- GUTS_SD(scaled_ci=FALSE, dose_metric="D") %>%
    set_param(c(ki=1, ke=1, kr=1, kk=1, z=1, hb=0)) %>%
    set_noexposure() %>%
    set_times(0:5)

  # no exposure -> no damage, hazard, etc
  rs <- simulate(base)
  expect_equal(names(rs), c("time","Ci","D","H","S"))
  expect_equal(rs$time, 0:5)
  expect_true(all(rs$Ci == 0))
  expect_true(all(rs$D == 0))
  expect_true(all(rs$H == 0))
  expect_true(all(rs$S == 1))

  # no exposure, but background mortality
  rs <- base %>% set_param(c(hb=1)) %>% simulate()
  expect_true(all(rs$Ci == 0))
  expect_true(all(rs$D == 0))
  expect_equal(rs$H, 0:5, tolerance=tol)
  expect_equal(rs$S, exp(-(0:5)), tolerance=tol)

  # update base scenario, disable all processes by default
  base <- base %>%
    set_param(c(ki=0, ke=0, kr=0, kk=0, z=0, hb=0)) %>%
    set_exposure(data.frame(time=0:5, conc=1))

  # with exposure, uptake only (ki > 0)
  rs <- base %>% set_param(c(ki=1)) %>% simulate()
  expect_equal(rs$Ci, 0:5, tolerance=tol)
  expect_true(all(rs$D == 0))
  expect_true(all(rs$H == 0))
  expect_true(all(rs$S == 1))

  # no exposure, elimination only, (ke > 0)
  rs <- base %>% set_init(c(Ci=1)) %>% set_noexposure() %>% set_param(c(ke=1)) %>% simulate()
  expect_equal(rs$Ci, exp(-(0:5)), tolerance=tol)
  expect_true(all(rs$D == 0))
  expect_true(all(rs$H == 0))
  expect_true(all(rs$S == 1))

  # with exposure, uptake and elimination (ki, ke > 0)
  rs <- base %>% set_param(c(ki=1)) %>% simulate()
  expect_true(all(diff(rs$Ci) > 0)) # strictly increasing Ci
  expect_true(all(rs$Ci <= 0:5)) # Ci smaller compared to scenario w/o elimination
  expect_true(all(rs$D == 0))
  expect_true(all(rs$H == 0))
  expect_true(all(rs$S == 1))

  # no exposure, repair only (kr > 0)
  rs <- base %>% set_init(c(D=1)) %>% set_noexposure() %>% set_param(c(kr=1)) %>% simulate()
  expect_true(all(rs$Ci == 0))
  expect_equal(rs$D, exp(-(0:5)), tolerance=tol)
  expect_true(all(rs$H == 0))
  expect_true(all(rs$S == 1))

  # with exposure, uptake and repair (ki, kr > 0)
  rs <- base %>% set_param(c(ki=1, kr=1)) %>% simulate()
  expect_true(all(diff(rs$Ci) > 0)) # continuously increasing Ci
  expect_true(all(diff(rs$D) > 0))  # continuously increasing D
  expect_true(all(rs$D <= rs$Ci))   # D strictly smaller than Ci (due to repair)
  expect_true(all(rs$H == 0))
  expect_true(all(rs$S == 1))

  # constant damage, killing rate only (kk > 0)
  rs <- base %>% set_init(c(D=1)) %>% set_param(c(kk=1)) %>% simulate()
  expect_true(all(rs$Ci == 0))
  expect_true(all(rs$D == 1))
  expect_equal(rs$H, 0:5, tolerance=tol)
  expect_equal(rs$S, exp(-(0:5)), tolerance=tol)

  # constant damage, killing rate, threshold set (kk == z, kk, z > 0)
  rs <- base %>% set_init(c(D=1)) %>% set_param(c(kk=1, z=1)) %>% simulate()
  expect_true(all(rs$Ci == 0))
  expect_true(all(rs$D == 1))
  expect_true(all(rs$H == 0))
  expect_true(all(rs$S == 1))

  # dose metric: Ci
  rs <- GUTS_SD(scaled_ci=FALSE, dose_metric="Ci") %>%
    set_init(c(Ci=1)) %>%
    set_param(c(ki=0, ke=0, kk=1, z=0, hb=0)) %>%
    set_noexposure() %>%
    set_times(0:5) %>%
    simulate()
  expect_true(all(rs$Ci == 1))
  expect_true(all(is.na(rs$D)))
  expect_equal(rs$H, 0:5, tolerance=tol)
  expect_equal(rs$S, exp(-(0:5)), tolerance=tol)

  # dose metric: Cw
  rs <- GUTS_SD(scaled_ci=FALSE, dose_metric="Cw") %>%
    set_init(c(D=1)) %>%
    set_param(c(kk=1, z=0, hb=0)) %>%
    set_exposure(data.frame(t=0, c=1), reset_times=FALSE) %>%
    set_times(0:5) %>%
    simulate()

  expect_true(all(is.na(rs$Ci)))
  expect_true(all(is.na(rs$D)))
  expect_equal(rs$H, 0:5, tolerance=tol)
  expect_equal(rs$S, exp(-(0:5)), tolerance=tol)
})

test_that("IT: solver", {
  tol <- 1e-5
  base <- GUTS_IT(scaled_ci=FALSE, dose_metric="D") %>%
    set_param(c(ki=1, ke=1, kr=1, alpha=1, beta=1, hb=0)) %>%
    set_noexposure() %>%
    set_times(0:5)

  # no exposure -> no damage, hazard, etc
  rs <- simulate(base)
  expect_equal(names(rs), c("time","Ci","D","H","S"))
  expect_equal(rs$time, 0:5)
  expect_true(all(rs$Ci == 0))
  expect_true(all(rs$D == 0))
  expect_true(all(rs$H == 0))
  expect_true(all(rs$S == 1))

  # no exposure, but background mortality
  rs <- base %>% set_param(c(hb=1)) %>% simulate()
  expect_true(all(rs$Ci == 0))
  expect_true(all(rs$D == 0))
  expect_equal(rs$H, 0:5, tolerance=tol)
  expect_equal(rs$S, exp(-(0:5)), tolerance=tol)

  # update base scenario, disable all processes by default
  base <- base %>%
    set_param(c(ki=0, ke=0, kr=0, alpha=1, beta=1, hb=0)) %>%
    set_exposure(data.frame(time=0:5, conc=1))

  # with exposure, uptake only (ki > 0)
  rs <- base %>% set_param(c(ki=1)) %>% simulate()
  expect_equal(rs$Ci, 0:5, tolerance=tol)
  expect_true(all(rs$D == 0))
  expect_true(all(rs$H == 0))
  expect_true(all(rs$S == 1))

  # no exposure, elimination only, (ke > 0)
  rs <- base %>% set_init(c(Ci=1)) %>% set_noexposure() %>% set_param(c(ke=1)) %>% simulate()
  expect_equal(rs$Ci, exp(-(0:5)), tolerance=tol)
  expect_true(all(rs$D == 0))
  expect_true(all(rs$H == 0))
  expect_true(all(rs$S == 1))

  # with exposure, uptake and elimination (ki, ke > 0)
  rs <- base %>% set_param(c(ki=1)) %>% simulate()
  expect_true(all(diff(rs$Ci) > 0)) # strictly increasing Ci
  expect_true(all(rs$Ci <= 0:5)) # Ci smaller compared to scenario w/o elimination
  expect_true(all(rs$D == 0))
  expect_true(all(rs$H == 0))
  expect_true(all(rs$S == 1))

  # no exposure, repair only (kr > 0)
  rs <- base %>% set_init(c(D=1)) %>% set_noexposure() %>% set_param(c(kr=1)) %>% simulate()
  expect_true(all(rs$Ci == 0))
  expect_equal(rs$D, exp(-(0:5)), tolerance=tol)
  expect_true(all(rs$H == 0))
  expect_true(all(rs$S == 0.5))

  # with exposure, uptake and repair (ki, kr > 0)
  rs <- base %>% set_param(c(ki=1, kr=1)) %>% simulate()
  expect_true(all(diff(rs$Ci) > 0)) # continuously increasing Ci
  expect_true(all(diff(rs$D) > 0))  # continuously increasing D
  expect_true(all(rs$D <= rs$Ci))   # D strictly smaller than Ci (due to repair)
  expect_true(all(rs$H == 0))
  expect_true(all(diff(rs$S) < 0))  # strictly decreasing

  # scale the median of thresholds, alpha
  rs <- base %>% set_init(c(D=1)) %>% set_param(c(alpha=2)) %>% simulate()
  expect_true(all(rs$Ci == 0))
  expect_true(all(rs$D == 1))
  expect_true(all(rs$H == 0))
  expect_equal(rs$S, rep(2/3, nrow(rs)), tolerance=tol)

  # scale the shape factor, beta
  rs <- base %>% set_init(c(D=1)) %>% set_param(c(beta=2)) %>% simulate()
  expect_true(all(rs$Ci == 0))
  expect_true(all(rs$D == 1))
  expect_true(all(rs$H == 0))
  expect_true(all(rs$S == 0.5))

  # scale alpha and beta
  rs <- base %>% set_init(c(D=1)) %>% set_param(c(alpha=2, beta=2)) %>% simulate()
  expect_true(all(rs$Ci == 0))
  expect_true(all(rs$D == 1))
  expect_true(all(rs$H == 0))
  expect_equal(rs$S, rep(1 - 1 / (1 + (1/2)^-2), nrow(rs)))

  # dose metric: Ci
  rs <- GUTS_IT(scaled_ci=FALSE, dose_metric="Ci") %>%
    set_init(c(Ci=1)) %>%
    set_param(c(ki=0, ke=0, alpha=1, beta=1, hb=0)) %>%
    set_noexposure() %>%
    set_times(0:5) %>%
    simulate()
  expect_true(all(rs$Ci == 1))
  expect_true(all(is.na(rs$D)))
  expect_true(all(rs$H == 0))
  expect_equal(rs$S, rep(0.5, nrow(rs)), tolerance=tol)

  # dose metric: Cw
  rs <- GUTS_IT(scaled_ci=FALSE, dose_metric="Cw") %>%
    set_init(c(D=1)) %>%
    set_param(c(alpha=1, beta=1, hb=0)) %>%
    set_exposure(data.frame(t=0, c=1), reset_times=FALSE) %>%
    set_times(0:5) %>%
    simulate()

  expect_true(all(is.na(rs$Ci)))
  expect_true(all(is.na(rs$D)))
  expect_true(all(rs$H == 0))
  expect_equal(rs$S, rep(0.5, nrow(rs)), tolerance=tol)
})

test_that("SD: parameters passed to solver", {
  # set up mocked deSolve function
  capture <- list()
  myode <- function(y, times, parms, ...) {
    capture$y <<- y
    capture$times <<- times
    capture$parms <<- parms
    data.frame(H=0)
  }

  # scenario: non-scaled Ci, M=D
  sc <- GUTS_SD(scaled_ci=FALSE, dose_metric="D") %>%
    set_param(c(ki=1, ke=1, kr=1, kk=1, hb=0, z=1)) %>%
    set_noexposure() %>%
    set_times(0:1)

  # execute
  with_mocked_bindings(
    solver(sc),
    ode = myode
  )
  expect_equal(capture$y, sc@init)
  expect_equal(capture$times, sc@times)
  expect_equal(capture$parms, c(unlist(sc@param), c("dose_metric"=0)))

  # scenario: non-scaled Ci, M=Ci
  sc <- GUTS_SD(scaled_ci=FALSE, dose_metric="Ci") %>%
    set_param(c(ki=1, ke=1, kk=1, hb=0, z=1))
  with_mocked_bindings(
    solver(sc),
    ode = myode
  )
  expect_equal(capture$parms[["dose_metric"]], 1)

  # scenario: non-scaled Ci, M=Cw
  sc <- GUTS_SD(scaled_ci=FALSE, dose_metric="Cw") %>%
    set_param(c(kk=1, hb=0, z=1))
  with_mocked_bindings(
    solver(sc),
    ode = myode
  )
  expect_equal(capture$parms[["dose_metric"]], 2)

  # scenario: scaled Ci, M=D
  sc <- GUTS_SD(scaled_ci=TRUE, dose_metric="D") %>%
    set_param(c(ke=3, Kiw=4, kr=1, kk=1, hb=0, z=1))
  with_mocked_bindings(
    solver(sc),
    ode = myode
  )
  expect_equal(capture$parms[["ke"]], 3)
  expect_equal(capture$parms[["ki"]], 3 * 4) # Kiw := ki/ke
  expect_equal(capture$parms[["dose_metric"]], 0)
})

test_that("IT: parameters passed to solver", {
  # set up mocked deSolve function
  capture <- list()
  myode <- function(y, times, parms, ...) {
    capture$y <<- y
    capture$times <<- times
    capture$parms <<- parms
    data.frame(H=0, CMax=0, Cw=0)
  }

  # scenario: non-scaled Ci, M=D
  sc <- GUTS_IT(scaled_ci=FALSE, dose_metric="D") %>%
    set_param(c(ki=1, ke=1, kr=1, hb=0, alpha=1, beta=1)) %>%
    set_noexposure() %>%
    set_times(0:1)
  myodeparam <- unlist(sc@param[c("ki","ke","kr","hb")])
  myinit <- c(sc@init, "CMax"=0)

  # execute
  with_mocked_bindings(
    solver(sc),
    ode = myode
  )
  expect_equal(capture$y, myinit)
  expect_equal(capture$times, sc@times)
  expect_equal(capture$parms, c(myodeparam, c("dose_metric"=0)))

  # scenario: non-scaled Ci, M=Ci
  sc <- GUTS_IT(scaled_ci=FALSE, dose_metric="Ci") %>%
    set_param(c(ki=1, ke=1, hb=0, alpha=1, beta=1)) %>%
    set_noexposure() %>%
    set_times(0:1)
  with_mocked_bindings(
    solver(sc),
    ode = myode
  )
  expect_equal(capture$parms[["dose_metric"]], 1)

  # scenario: non-scaled Ci, M=Cw
  sc <- GUTS_IT(scaled_ci=FALSE, dose_metric="Cw") %>%
    set_param(c(hb=0, alpha=1, beta=1)) %>%
    set_noexposure() %>%
    set_times(0:1)
  with_mocked_bindings(
    solver(sc),
    ode = myode
  )
  expect_equal(capture$parms[["dose_metric"]], 2)

  # scenario: scaled Ci, M=D
  sc <- GUTS_IT(scaled_ci=TRUE, dose_metric="D") %>%
    set_param(c(ke=3, Kiw=4, kr=5, hb=0, alpha=1, beta=1))
  with_mocked_bindings(
    solver(sc),
    ode = myode
  )
  expect_equal(capture$parms[["ke"]], 3)
  expect_equal(capture$parms[["ki"]], 3 * 4) # Kiw := ki/ke
  expect_equal(capture$parms[["dose_metric"]], 0)
})

test_that("SD: effects", {
  sc  <- GUTS_SD(scaled_ci=FALSE, dose_metric="D") %>%
    set_init(c(Ci=1, D=0.5, H=0.2)) %>%
    set_param(c(ki=1, ke=1, kr=1, kk=1, z=0.3, hb=0)) %>%
    set_noexposure() %>%
    set_times(0:5)

  expect_equal(
    effect(sc, ep_only=TRUE, max_only=FALSE)$L, # lethality calculated by effect/fx
    1 - tail(simulate(sc), n=1)$S,              # lethality calculated manually
    ignore_attr=TRUE,
    tolerance=1e-5
  )
})

test_that("IT: effects", {
  sc  <- GUTS_IT(scaled_ci=FALSE, dose_metric="D") %>%
    set_init(c(Ci=1, D=0.5, H=0.2)) %>%
    set_param(c(ki=0, ke=0, kr=0, hb=0, alpha=1, beta=1)) %>%
    set_noexposure() %>%
    set_times(0:1)

  expect_equal(
    effect(sc, ep_only=TRUE, max_only=FALSE)$L, # lethality calculated by effect/fx
    1 - tail(simulate(sc), n=1)$S,              # lethality calculated manually
    ignore_attr=TRUE,
    tolerance=1e-5
  )
})

# additional model verification is implemented in test `verify-guts`
