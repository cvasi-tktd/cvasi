test_that("is_scenario", {
  expect_true(is_scenario(GUTS_RED_IT()))
  expect_true(is_scenario(new("EffectScenario")))
  expect_equal(is_scenario(c(GUTS_RED_IT(),Lemna_Schmitt())), c(TRUE,TRUE))
  expect_equal(is_scenario(c(DEB_abj(),NA)), c(TRUE,FALSE))
  # non-scenario arguments
  expect_false(is_scenario(c()))
  expect_false(is_scenario(NA))
  expect_false(is_scenario(NULL))
  expect_false(is_scenario(1))
  expect_equal(is_scenario(1:5), FALSE)
})

test_that("is_parameter_set", {
  expect_true(is_parameter_set(parameter_set("test")))
  expect_equal(is_parameter_set(c(parameter_set("test"),parameter_set("test"))), c(TRUE,TRUE))
  # invalid arguments
  expect_false(is_parameter_set(NA))
  expect_false(is_parameter_set(NULL))
  expect_false(is_parameter_set(1))
  expect_equal(is_parameter_set(1:5), FALSE)
})

test_that("is_param_match", {
  sc_unt <- new("EffectScenario", name="foo", tag="")
  sc_tgd <- new("EffectScenario", name="foo", tag="bar")
  sc_other <- new("EffectScenario", name="baz")
  ps_unt <- parameter_set(model="foo", tag="")
  ps_tgd <- parameter_set(model="foo", tag="bar")
  ps_other <- parameter_set(model="baz", tag="")

  # match
  expect_true(is_param_match(sc_unt, ps_unt))
  expect_true(is_param_match(sc_tgd, ps_tgd))
  # no match
  expect_false(is_param_match(sc_unt, ps_tgd))
  expect_false(is_param_match(sc_unt, ps_other))
  expect_false(is_param_match(sc_tgd, ps_unt))
  expect_false(is_param_match(sc_tgd, ps_other))
  expect_false(is_param_match(sc_other, ps_unt))
  expect_false(is_param_match(sc_other, ps_tgd))
  # vectorised parameter sets
  expect_equal(c(FALSE,FALSE), is_param_match(sc_unt, c(ps_tgd, ps_tgd)))
  expect_equal(c(TRUE,FALSE),  is_param_match(sc_unt, c(ps_unt, ps_tgd)))
  expect_equal(c(TRUE,FALSE),  is_param_match(sc_unt, c(ps_unt, ps_other)))
  expect_equal(c(TRUE,TRUE),   is_param_match(sc_tgd, c(ps_tgd, ps_tgd)))

  # invalid inputs
  expect_error(is_param_match(NULL, NULL))
  expect_error(is_param_match(c(), c()))
  expect_error(is_param_match(sc_unt, c()))
  expect_error(is_param_match(c(), ps_unt))
})

test_that("is_GUTS", {
  expect_true(is_GUTS(GUTS_RED_IT()))
  expect_true(is_GUTS(GUTS_RED_SD()))
  expect_equal(is_GUTS(c(GUTS_RED_IT(),GUTS_RED_IT())), c(TRUE,TRUE))
  # invalid arguments
  expect_false(is_GUTS(list()))
  expect_false(is_GUTS(NA))
  expect_false(is_GUTS(NULL))
  expect_false(is_GUTS(1))
  expect_equal(is_GUTS(1:5), FALSE)
})

test_that("is_GUTS_IT", {
  expect_true(is_GUTS_IT(GUTS_RED_IT()))
  expect_equal(is_GUTS_IT(c(GUTS_RED_IT(),GUTS_RED_IT())), c(TRUE,TRUE))
  # invalid arguments
  expect_false(is_GUTS_IT(list()))
  expect_false(is_GUTS_IT(GUTS_RED_SD()))
  expect_false(is_GUTS_IT(NA))
  expect_false(is_GUTS_IT(NULL))
  expect_false(is_GUTS_IT(1))
  expect_equal(is_GUTS_IT(1:5), FALSE)
})

test_that("is_GUTS_SD", {
  expect_true(is_GUTS_SD(GUTS_RED_SD()))
  expect_equal(is_GUTS_SD(c(GUTS_RED_SD(),GUTS_RED_SD())), c(TRUE,TRUE))
  # invalid arguments
  expect_false(is_GUTS_SD(list()))
  expect_false(is_GUTS_SD(GUTS_RED_IT()))
  expect_false(is_GUTS_SD(NA))
  expect_false(is_GUTS_SD(NULL))
  expect_false(is_GUTS_SD(1))
  expect_equal(is_GUTS_SD(1:5), FALSE)
})

test_that("is_DEB", {
  expect_true(is_DEB(DEB_abj()))
  expect_equal(is_DEB(c(DEB_abj(),DEB_abj())), c(TRUE,TRUE))
  # invalid arguments
  expect_false(is_DEB(list()))
  expect_false(is_DEB(GUTS_RED_IT()))
  expect_false(is_DEB(NA))
  expect_false(is_DEB(NULL))
  expect_false(is_DEB(1))
  expect_equal(is_DEB(1:5), FALSE)
})

test_that("is_Lemna", {
  expect_true(is_Lemna(Lemna_Schmitt()))
  expect_true(is_Lemna(Lemna_SchmittThold()))
  expect_true(is_Lemna(Lemna_SETAC()))
  expect_equal(is_Lemna(c(Lemna_Schmitt(),Lemna_Schmitt())), c(TRUE,TRUE))
  # invalid arguments
  expect_false(is_Lemna(list()))
  expect_false(is_Lemna(GUTS_RED_IT()))
  expect_false(is_Lemna(NA))
  expect_false(is_Lemna(NULL))
  expect_false(is_Lemna(1))
  expect_equal(is_Lemna(1:5), FALSE)
})

test_that("is_LemnaThreshold", {
  expect_true(is_LemnaThreshold(Lemna_SchmittThold()))
  expect_equal(is_LemnaThreshold(c(Lemna_SchmittThold(),Lemna_SchmittThold())), c(TRUE,TRUE))
  # invalid arguments
  expect_false(is_LemnaThreshold(list()))
  expect_false(is_LemnaThreshold(Lemna_Schmitt()))
  expect_false(is_LemnaThreshold(NA))
  expect_false(is_LemnaThreshold(NULL))
  expect_false(is_LemnaThreshold(1))
  expect_equal(is_LemnaThreshold(1:5), FALSE)
})

test_that("is_regular_series", {
  # exposure series
  expect_false(is_regular_series(ExposureSeries(series=data.frame(t=0:5, c=0:5))))
  expect_true(is_regular_series(ExposureSeries(series=data.frame(t=0:5, c=0:5), meta=list(regular.series=TRUE))))
  # some numeric vector of output times
  expect_true(is_regular_series(1))
  expect_true(is_regular_series(0:10))
  expect_true(is_regular_series(seq(0,10,0.1)))
  expect_true(is_regular_series(seq(0,10,0.3)))
  expect_true(is_regular_series(seq(0,10.12,0.127)))
  # invalid arguments
  expect_error(is_regular_series(NA))
  expect_error(is_regular_series(NULL))
  expect_error(is_regular_series(c()))
  expect_error(is_regular_series(c(NA,NA)))
})

test_that("is_control_required", {
  expect_true(is_control_required(Lemna_Schmitt()))
  expect_true(is_control_required(DEB_abj()))
  expect_equal(is_control_required(c(Lemna_Schmitt(),Lemna_Schmitt())), c(TRUE,TRUE))
  # invalid arguments
  expect_false(is_control_required(GUTS_RED_IT()))
  expect_false(is_control_required(NA))
  expect_false(is_control_required(NULL))
  expect_false(is_control_required(1))
  expect_equal(is_control_required(1:5), FALSE)
})
