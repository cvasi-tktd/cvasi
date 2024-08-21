
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
