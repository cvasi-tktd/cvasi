
test_that("set param bounds works", {

  # test for EffectScenario
  sc <- metsulfuron %>%
    set_param_bounds(pars_bound = list(k_phot_max = list(0,30),
                                       k_resp = list(0,10)))

  expect_equal(sc@param.low[c("k_phot_max", "k_resp")],
               list(k_phot_max = 0, k_resp = 0))
  expect_equal(sc@param.up[c("k_phot_max", "k_resp")],
               list(k_phot_max = 30, k_resp = 10))

  # test for CalibrationSet and list of CalibrationSets
  Schmitt2013 %>%
    dplyr::group_by(ID) %>%
    dplyr::group_map(function(data, key) {
      exp <- data %>%  dplyr::select(t, conc)
      obs <- data %>%  dplyr::select(t, obs)
      sc <- metsulfuron %>% set_exposure(exp)
      CalibrationSet(sc, obs)
    }) -> cs # list of CalibrationSets
  Cset <- cs[[1]] # CalibrationSet

  cs <- cs %>%
    set_param_bounds(pars_bound = list(k_phot_max = list(0,70)))
  Cset <- Cset %>%
    set_param_bounds(pars_bound = list(k_phot_max = list(0,70)))

  expect_equal(cs[[7]]@scenario@param.up[c("k_phot_max")],
               list(k_phot_max = 70))
  expect_equal(Cset@scenario@param.up[c("k_phot_max")],
               list(k_phot_max = 70))


})
