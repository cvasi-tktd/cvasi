# Extensive tests of model behavior and outputs are part of the automated model
# verification, see `test-verify-guts_red.R`

test_that("background mortality", {
  # RED-IT
  ctrl <- minnow_it %>% set_noexposure() %>% set_param(c(hb=0))
  mort <- ctrl %>% set_param(c(hb=1))

  expect_equal(simulate(ctrl)$S, rep(1, 5))
  expect_true(all(diff(simulate(mort)$S) < 0)) # survival prob is strictly decreasing

  # RED-SD
  ctrl <- minnow_sd %>% set_noexposure() %>% set_param(c(hb=0))
  mort <- ctrl %>% set_param(c(hb=1))

  expect_equal(simulate(ctrl)$S, rep(1, 5))
  expect_true(all(diff(simulate(mort)$S) < 0)) # survival prob is strictly decreasing
})

test_that("C code errors", {
  # invalid parameter values
  expect_error(minnow_it %>% set_param(c(kd=-1)) %>% simulate(), "smaller than zero")
  expect_error(minnow_it %>% set_param(c(hb=-1)) %>% simulate(), "smaller than zero")

  # invalid exposure values
  expect_error(minnow_it %>% set_exposure(data.frame(t=0, -1), FALSE) %>% simulate())

  # invalid parameter values
  expect_error(minnow_sd %>% set_param(c(kd=-1)) %>% simulate(), "smaller than zero")
  expect_error(minnow_sd %>% set_param(c(hb=-1)) %>% simulate(), "smaller than zero")
  expect_error(minnow_sd %>% set_param(c(kk=-1)) %>% simulate(), "smaller than zero")
  expect_error(minnow_sd %>% set_param(c(z=-1)) %>% simulate(), "smaller than zero")

  # invalid exposure values
  expect_error(minnow_sd %>% set_exposure(data.frame(t=0, -1), FALSE) %>% simulate())
})

test_that("output variables", {
  rs <- minnow_it %>% simulate(nout=0)
  rs2 <- minnow_it %>% simulate(nout=1)

  expect_equal(length(names(rs2)), length(names(rs)) + 1)
  # S is calculated on the return value of deSolve and therefore appears at the
  # end of the columns, after the output variable Cw, unfortunately
  expect_equal(names(rs2), c("time","D","H","Cw","S"))
  expect_equal(rs2$Cw, minnow_it@exposure@series[,2]) # external concentration

  rs <- minnow_sd %>% simulate(nout=0)
  rs2 <- minnow_sd %>% simulate(nout=1)

  expect_equal(length(names(rs2)), length(names(rs)) + 1)
  # S is calculated on the return value of deSolve and therefore appears at the
  # end of the columns, after the output variable Cw, unfortunately
  expect_equal(names(rs2), c("time","D","H","Cw","S"))
  expect_equal(rs2$Cw, minnow_sd@exposure@series[,2]) # external concentration
})

test_that("solvers", {
  # RED-IT
  expect_equal(solver(minnow_it)$time, minnow_it@times)
  # RED-SD
  expect_equal(solver(minnow_it)$time, minnow_it@times)
})

test_that("effects", {
  # RED-IT
  ctrl <- minnow_it %>% set_noexposure() %>% simulate()
  trl <- minnow_it %>% simulate()
  myeffect <- 1 - tail(trl$S, n=1) / tail(ctrl$S, n=1)
  expect_equal(myeffect, effect(minnow_it)$L)

  # RED-SD
  ctrl <- minnow_sd %>% set_noexposure() %>% simulate()
  trl <- minnow_sd %>% simulate()
  myeffect <- 1 - tail(trl$S, n=1) / tail(ctrl$S, n=1)
  expect_equal(myeffect, effect(minnow_sd)$L)
})
