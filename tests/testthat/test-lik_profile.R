
test_that("log-likelihood gives expected error", {

  # error for too many params
  expect_error(lik_profile(model = Lemna_Schmitt(),
                           data = data.frame(t = c(0, 3, 5, 7, 7.01, 10, 12, 14),
                                             obs = c(12, 38, 92, 176, 176, 627, 1283, 2640)),
                           output = "BM",
                           pars_profile = 1:11))

  # error for model
  # error because of misspecified model (not Calibrationset nor EffectScenario)
  expect_error(lik_profile(model = "model",
                           output = "BM",
              pars_profile = c(5.6, 1.9)))

  # error because misspecified model (list, but not Calibrationset)
  expect_error(lik_profile(model = list(Lemna_Schmitt()),
                           output = "BM",
                           pars_profile = c(5.6, 1.9)))

  # error because data missing for EffectScenario
  expect_error(lik_profile(model = Lemna_Schmitt(),
                           output = "BM",
                           pars_profile = c(5.6, 1.9)))

  # errors for pars_profile
  # error because parameter vector is not a vector
  expect_error(lik_profile(model = Lemna_Schmitt(),
                           output = "BM",
                           data = data.frame(t = c(0, 3, 5, 7, 7.01, 10, 12, 14),
                                             obs = c(12, 38, 92, 176, 176, 627, 1283, 2640)),
                           pars_profile = list(5.6, 1.9)))

  # error because parameter vector is not named
  expect_error(lik_profile(model = Lemna_Schmitt(),
                           output = "BM",
                           data = data.frame(t = c(0, 3, 5, 7, 7.01, 10, 12, 14),
                                            obs = c(12, 38, 92, 176, 176, 627, 1283, 2640)),
                           pars_profile = c(5.6, 1.9)))

  # errors for output
  expect_error(lik_profile(model = Lemna_Schmitt(),
                           output = list("BM"),
                           data = data.frame(t = c(0, 3, 5, 7, 7.01, 10, 12, 14),
                                             obs = c(12, 38, 92, 176, 176, 627, 1283, 2640)),
                           pars_profile = c(k_phot_max = 5.6,
                                            k_resp = 1.9)))

  # errors for profile type
  expect_error(lik_profile(model = Lemna_Schmitt(),
                           output = "BM",
                           data = data.frame(t = c(0, 3, 5, 7, 7.01, 10, 12, 14),
                                             obs = c(12, 38, 92, 176, 176, 627, 1283, 2640)),
                           pars_profile = c(k_phot_max = 5.6,
                                            k_resp = 1.9),
                           prof_type = "slow"))

})



test_that("likelihood profiling works", {

  # Inputs for likelihood profiling
  obs <- Schmitt2013 %>%
    dplyr::filter(ID == "T0") %>%
    dplyr::select(t, obs)
  colnames(obs) = c("t", "BM")
  params <- c(k_phot_max = 3.979200,
              k_resp =  7.700779e-08)
  sc <- metsulfuron %>%
    set_param(params)

  # Likelihood profiling
  suppressMessages(
    res <- lik_profile(
      x = sc,
      data = obs,
      output = "BM",
      par = params[1],
      type = "fine"
    )
  )

  # tests
  expect_equal(res$k_phot_max$confidence_interval,
               c(0.0000, 3.9792), tolerance = 10e-3)
  expect_equal(dim(res$k_phot_max$likelihood_profile),
               c(13,4))
  expect_equal(res$k_phot_max$orig_par_value, 3.9792)
})



