
# test routine ---------------------------------------------------------------------
# Simulation with the Algae_Simple model, compare against reference values
test_that("Algae_Simple simulation", {
  tol <- 1e-5

  # Simulate Algae_Simple

  model_base <- Algae_Simple()

  # sim setup
  sim_end <- 7
  times <- seq(from = 0, to = sim_end, by = 1)
  y_init <- c(A = 1, Dw = 0)

  # parms
  parms <- c(mu_max = 1,
             const_growth = 0,
             EC_50 = 1,
             b = 2,
             scaled = 0,
             kD = 200,
             dose_response = 0)

  # forcings
  forc_fgrowth <- data.frame(times = times, f_growth = rep(1, length(times)))
  forc_C_in <- data.frame(times = times, C = rep(0, length(times)))

  # control run
  effect_scenario <- model_base %>%
    set_param(parms) %>%
    set_tag("control run") %>%
    set_exposure(forc_C_in) %>%
    set_times(times) %>%
    set_forcings(f_growth = forc_fgrowth) %>%
    set_init(y_init)
  results <- effect_scenario %>% simulate(nout = 6)

  # check biomass growth against values from analytical solution
  # generated in R with initial_value * exp(growth_max * t)
  expect_equal(results[[2, "time"]], 1)
  expect_equal(results[[2, "A"]], 2.718282, tolerance = tol)

  expect_equal(results[[3, "time"]], 2)
  expect_equal(results[[3, "A"]], 7.389056, tolerance = tol)

  expect_equal(results[[4, "time"]], 3)
  expect_equal(results[[4, "A"]], 20.085537, tolerance = tol)

  expect_equal(results[[5, "time"]], 4)
  expect_equal(results[[5, "A"]], 54.598150, tolerance = tol)

  expect_equal(results[[8, "time"]], 7)
  expect_equal(results[[8, "A"]], 1096.633158, tolerance = tol)

#------------------------------------------------------------------------------#

  # control run, constant growth
  # parms
  parms <- c(mu_max = 1,
             const_growth = 1,
             EC_50 = 1,
             b = 2,
             scaled = 0,
             kD = 200,
             dose_response = 0)

  # forcings
  forc_fgrowth <- data.frame(times = times, f_growth = rep(0, length(times)))
  forc_C_in <- data.frame(times = times, C = rep(0, length(times)))

  # control run
  # control run
  effect_scenario <- model_base %>%
    set_param(parms) %>%
    set_tag("control run") %>%
    set_exposure(forc_C_in) %>%
    set_times(times) %>%
    set_init(y_init)
  results <- effect_scenario %>% simulate(nout = 6)

  # check biomass growth against values from analytical solution
  # generated in R with initial_value * exp(growth_max * t)
  expect_equal(results[[2, "time"]], 1)
  expect_equal(results[[2, "A"]], 2.718282, tolerance = tol)

  expect_equal(results[[3, "time"]], 2)
  expect_equal(results[[3, "A"]], 7.389056, tolerance = tol)

  expect_equal(results[[4, "time"]], 3)
  expect_equal(results[[4, "A"]], 20.085537, tolerance = tol)

  expect_equal(results[[5, "time"]], 4)
  expect_equal(results[[5, "A"]], 54.598150, tolerance = tol)

  expect_equal(results[[8, "time"]], 7)
  expect_equal(results[[8, "A"]], 1096.633158, tolerance = tol)

#------------------------------------------------------------------------------#

  #EC50 run logit
  # forcings
  forc_C_in <- data.frame(times = times, C = rep(1, length(times)))

  effect_scenario <- model_base %>%
    set_param(parms) %>%
    set_tag("control run") %>%
    set_exposure(forc_C_in) %>%
    set_times(times) %>%
    set_init(y_init)
  result_epx <- epx(effect_scenario, level = 50)

  expect_equal(result_epx$r.EP50, 1)

#------------------------------------------------------------------------------#

  #EC50 run probit
  # parms
  parms <- c(mu_max = 1,
             const_growth = 1,
             EC_50 = 1,
             b = 2,
             scaled = 0,
             kD = 200,
             dose_response = 1)

  effect_scenario <- effect_scenario %>%
    set_param(parms)

  result_epx <- epx(effect_scenario, level = 50)

  expect_equal(result_epx$r.EP50, 1)

#------------------------------------------------------------------------------#

  #EC50 run probit scaled
  # parms
  parms <- c(mu_max = 1,
             const_growth = 1,
             EC_50 = 1,
             b = 2,
             scaled = 0,
             kD = 200,
             dose_response = 1)

  effect_scenario <- effect_scenario %>%
    set_param(parms)
  result_epx <- epx(effect_scenario, level = 50)

  expect_equal(result_epx$r.EP50, 1)
})
