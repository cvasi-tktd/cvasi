
# test routine ---------------------------------------------------------------------
# Simulation with the Algae_Weber model, compare against reference values
# derived from a simulation with this model for R. subcapitata and isoproturon
# (i.e., recreating the fig in EFSA Scientific Opinion on TKTD, Fig 32
# doi.org/10.2903/j.efsa.2018.5377, showing the flow through exp. of Weber et
# al. 2012) This EFSA example uses the Algae_Weber model, however, to use it for
# testing the Algae_TKTD model (where there is no dilution, but there is an
# internal scaled damage), the background mortality parameter was set so it
# included both the value of background mortality and dilution from Weber's flow
# through, additionally, a high KD parameter value was taken to represent high
# uptake hence the scaled damage tracks almost instantly the water concentration
test_that("Algae_Weber simulation", {
  tol <- 1e-4

  # Simulate Algae_TKTD for Rsubcapitata exposed to isoproturon
  # sim setup
  sim_end <- 72
  y_0 <- c(A = 1, Q = 0.01, P = 0.36 * 0.5)
  times <- seq(from = 0, to = sim_end, by = sim_end / 1000)
  # parms
  params <- c(mu_max = 1.7380, m_max = 0.5500, v_max = 0.0520, k_s = 0.0680,
              Q_min = 0.0011, Q_max = 0.0144,
              T_opt = 27, T_min = 0, T_max = 35, I_opt = 120,
              EC_50 = 115, b = 1.268, k = 0.2
  )
  # forcings
  forc_I <- data.frame(times = sim_end, I = rep(100, sim_end))
  forc_T <- data.frame(times = sim_end, T_act = rep(24, sim_end))
  forcings <- list(forc_I, forc_T)
  # exposure
  weber_exposure <- Rsubcapitata@exposure@series
  # Create Eff.Scen.
  Rsubcap_Isopr <- Algae_Weber() %>%
    set_param(params) %>%
    set_exposure(weber_exposure) %>%
    set_forcings(I = forc_I,
                 T_act = forc_T) %>%
    set_times(times)
  # simulate
  Rsubcap_Isopr %>% simulate() -> out
  # calc % biomass
  out <- out %>%
    dplyr::mutate(perc = A/max(A)*100)

  # tests for starting values and sim setup
  expect_equal(out[, "time"], seq(from = 0, to = 72, 72 / 1000)) # simulation duration
  expect_equal(out[[1, "A"]], 1) # starting biomass value
  expect_equal(out[[1, "Q"]], 0.01) # starting Q value
  expect_equal(out[[1, "P"]], 0.18) # starting P value
  expect_equal(out[[1, "perc"]], 0.758399, tolerance = tol) # starting %A value

  # discard burnin to steady state
  out <- out %>%
    dplyr::filter(time > 12)
  # identify largest drop in biomass time and % (timing is derived from "out")
  drop_time <- out[which(out$A == min(out$A)), "time"]
  drop_perc <- out[which(out$A == min(out$A)), "perc"]
  expect_equal(drop_time, 32.256, tolerance = tol)
  expect_equal(drop_perc, 7.096122, tolerance = tol)

  # check timing and magnitude of other peaks (timing is based on known values)
  # first drop
  expect_equal(out[[225, "time"]], 28.152)
  expect_equal(out[[225, "perc"]], 18.6998, tolerance = tol)
  # second drop
  expect_equal(out[[286, "time"]], 32.544)
  expect_equal(out[[286, "perc"]], 7.194282, tolerance = tol)
  # third drop
  expect_equal(out[[407, "time"]], 41.256)
  expect_equal(out[[407, "perc"]], 97.89552, tolerance = tol)

})


# test routine ---------------------------------------------------------------------
# Simulation with the Algae_TKTD model, compare against reference values
# derived from a simulation with this model for R. subcapitata and isoproturon
# (i.e., recreating the fig in EFSA Scientific Opinion on TKTD, Fig 32
# doi.org/10.2903/j.efsa.2018.5377, showing the flow through exp. of Weber et
# al. 2012) This EFSA example uses the Algae_Weber model, however, to use it for
# testing the Algae_TKTD model (where there is no dilution, but there is an
# internal scaled damage), the background mortality parameter was set so it
# included both the value of background mortality and dilution from Weber's flow
# through, additionally, a high KD parameter value was taken to represent high
# uptake hence the scaled damage tracks almost instantly the water concentration
test_that("Algae_TKTD simulation", {
  tol <- 1e-5

  # Simulate Algae_TKTD for R. subcapitata exposed to isoproturon

  # sim setup
  sim_end <- 72
  y_0 <- c(A = 1, Q = 0.01, P = 0.36 * 0.5, Dw = 0)
  times <- seq(from = 0, to = sim_end, by = sim_end / 1000)

  # parms
  params <- c(mu_max = 1.7380, m_max = 0.5500, v_max = 0.0520, k_s = 0.0680,
              Q_min = 0.0011, Q_max = 0.0144,
              T_opt = 27, T_min = 0, T_max = 35, I_opt = 120,
              EC_50 = 115, b = 1.268, kD = 100, dose_resp = 0
  )
  # forcings
  forc_I <- data.frame(times = sim_end, I = rep(100, sim_end))
  forc_T <- data.frame(times = sim_end, T_act = rep(24, sim_end))
  forcings <- list(forc_I, forc_T)
  # exposure
  weber_exposure <- Rsubcapitata@exposure@series
  # Create Eff.Scen.
  Rsubcap_Isopr <- Algae_TKTD() %>%
    set_param(params) %>%
    set_exposure(weber_exposure) %>%
    set_forcings(I = forc_I,
                 T_act = forc_T) %>%
    set_times(times)
  # simulate
  Rsubcap_Isopr %>% simulate() -> out
  # calc % biomass
  out <- out %>%
    dplyr::mutate(perc = A/max(A)*100)

  # tests for starting values and sim setup
  expect_equal(out[, "time"], seq(from = 0, to = 72, 72 / 1000)) # simulation duration
  expect_equal(out[[1, "A"]], 1) # starting biomass value
  expect_equal(out[[1, "Q"]], 0.01) # starting Q value
  expect_equal(out[[1, "P"]], 0.18) # starting P value
  expect_equal(out[[1, "Dw"]], 0) # starting exposure value
  expect_equal(out[[1, "perc"]], 5.179975e-04, tolerance = tol) # starting %A value

  # discard burnin to steady state
  out <- out %>%
    dplyr::filter(time > 12)
  # identify largest drop in biomass time and % (timing is derived from "out")
  drop_time <- out[which(out$A == min(out$A)), "time"]
  drop_perc <- out[which(out$A == min(out$A)), "perc"]
  expect_equal(drop_time, 31.104, tolerance = tol)
  expect_equal(drop_perc, 57.79968, tolerance = tol)

  # check timing and magnitude of other peaks (timing is based on known values)
  # first drop
  expect_equal(out[[225, "time"]], 28.152)
  expect_equal(out[[225, "perc"]], 59.80685, tolerance = tol)
  # second drop
  expect_equal(out[[286, "time"]], 32.544)
  expect_equal(out[[286, "perc"]], 65.50502, tolerance = tol)
  # third drop
  expect_equal(out[[407, "time"]], 41.256)
  expect_equal(out[[407, "perc"]], 99.91928, tolerance = tol)

})


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
  results <- effect_scenario %>% simulate(nout = 5)

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
             EC_50 = 1,
             b = 2,
             scaled = 0,
             kD = 200,
             dose_response = 0)

  # forcings
  forc_fgrowth <- data.frame(times = times, f_growth = rep(0, length(times)))
  forc_C_in <- data.frame(times = times, C = rep(0, length(times)))

  # control run
  effect_scenario <- model_base %>%
    set_param(parms) %>%
    set_tag("control run") %>%
    set_exposure(forc_C_in) %>%
    set_times(times) %>%
    set_init(y_init)
  results <- effect_scenario %>% simulate(nout = 5)

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


test_that("output variables", {
  sc <- Rsubcapitata
  rs <- sc %>% simulate(nout=0)
  rs2 <- sc %>% simulate(nout=4)

  expect_equal(length(rs2), length(rs) + 4)

  df <- dplyr::select(rs2, dplyr::all_of(seq(length(rs)+1, length(rs2))))
  expect_equal(names(df), c("dA", "dQ", "dP", "dDw"))
  expect_true(any(df[, 1] != 0))
  expect_true(any(df[, 2] != 0))
  expect_true(any(df[, 3] != 0))
  expect_true(any(df[, 4] != 0))
})


test_that("algae effects", {
  sc <- Rsubcapitata
  ctrl <- sc %>% set_noexposure() %>% simulate()
  t1 <- sc %>% simulate()

  myeffect <- 1 - tail(t1$A, n=1)/tail(ctrl$A, n=1)
  expect_equal(effect(sc)$A[1], myeffect, tolerance=1e-5)

  # growth rate cannot be determined if transfers are enabled
  sc2 <- sc %>% set_transfer(interval=7)
  expect_error(effect(sc2), regexp="biomass transfer")
})
