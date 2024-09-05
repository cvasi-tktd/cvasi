
test_that("log-likelihood gives expected error", {

  # error for too many params
  expect_error(suppressMessages(lik_profile(x = Lemna_Schmitt(),
                           par = Lemna_Schmitt()@param,
                           data = data.frame(t = c(0, 3, 5, 7, 7.01, 10, 12, 14),
                                             obs = c(12, 38, 92, 176, 176, 627, 1283, 2640)),
                           output = "BM")))

  # error for model
  # # error because of misspecified model (not Calibrationset nor EffectScenario)
  # expect_error(lik_profile(x = "model",
  #                          par = c(k_phot_max = 5.6,
  #                                  k_resp = 1.9),
  #                          output = "BM"))

  # error because data missing for EffectScenario
  expect_error(suppressMessages(lik_profile(x = Lemna_Schmitt(),
                           output = "BM",
                           par = c(k_phot_max = 5.6,
                                         k_resp = 1.9))))

  # errors for pars_profile
  # error because parameter vector is not a vector
  expect_error(suppressMessages(lik_profile(x = Lemna_Schmitt(),
                           output = "BM",
                           data = data.frame(t = c(0, 3, 5, 7, 7.01, 10, 12, 14),
                                             obs = c(12, 38, 92, 176, 176, 627, 1283, 2640)),
                           par = Lemna_Schmitt()@param[c(1,2)])))

  # # error because parameter vector is not named
  # expect_error(lik_profile(x = Lemna_Schmitt(),
  #                          output = "BM",
  #                          data = data.frame(t = c(0, 3, 5, 7, 7.01, 10, 12, 14),
  #                                            obs = c(12, 38, 92, 176, 176, 627, 1283, 2640)),
  #                          par = as.numeric(Lemna_Schmitt()@param[c(1,2)])))

  # errors for output
  expect_error(suppressMessages(lik_profile(x = Lemna_Schmitt(),
                           output = list("BM"),
                           data = data.frame(t = c(0, 3, 5, 7, 7.01, 10, 12, 14),
                                             obs = c(12, 38, 92, 176, 176, 627, 1283, 2640)),
                           par = c(k_phot_max = 5.6,
                                            k_resp = 1.9))))

  # errors for profile type
  expect_error(suppressMessages(lik_profile(x = Lemna_Schmitt(),
                           output = "BM",
                           data = data.frame(t = c(0, 3, 5, 7, 7.01, 10, 12, 14),
                                             obs = c(12, 38, 92, 176, 176, 627, 1283, 2640)),
                           par = c(k_phot_max = 5.6,
                                            k_resp = 1.9),
                           type = "slow")))

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
  expect_equal(length(res$k_phot_max$confidence_interval), 2)
  expect_equal(dim(res$k_phot_max$likelihood_profile)[2], 4)
  expect_equal(res$k_phot_max$orig_par_value, 3.9792)
})



