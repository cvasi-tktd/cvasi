test_that("import from RDS", {
  fp <- test_path("../data/morse/fit_sd.rds")
  fit <- suppressMessages(import_morse(fp, find_sd=TRUE, find_it=FALSE,
                                       param="estim", reset_hb=FALSE))

  expect_equal(length(fit), 1)
  fit <- fit[[1]]
  expect_true(is_parameter_set(fit))
  expect_equal(fit@model, "GUTS-RED-SD")
  expect_true(fit@param$kd > 0)
  expect_true(fit@param$hb > 0)
  expect_true(fit@param$z > 0)
  expect_true(fit@param$kk > 0)


  fp <- test_path("../data/morse/fit_it.rds")
  fit <- suppressMessages(import_morse(fp, find_sd=FALSE, find_it=TRUE,
                                       param="estim", reset_hb=FALSE))

  expect_equal(length(fit), 1)
  fit <- fit[[1]]
  expect_true(is_parameter_set(fit))
  expect_equal(fit@model, "GUTS-RED-IT")
  expect_true(fit@param$kd > 0)
  expect_true(fit@param$hb > 0)
  expect_true(fit@param$alpha > 0)
  expect_true(fit@param$beta > 0)
})

test_that("import from RData", {
  fp <- test_path("../data/morse/fit_sd.rdata")
  fit <- suppressMessages(import_morse(fp, find_sd=TRUE, find_it=FALSE,
                                       param="estim", reset_hb=FALSE))

  expect_equal(length(fit), 1)
  fit <- fit[[1]]
  expect_true(is_parameter_set(fit))
  expect_equal(fit@model, "GUTS-RED-SD")
  expect_true(fit@param$kd > 0)
  expect_true(fit@param$hb > 0)
  expect_true(fit@param$z > 0)
  expect_true(fit@param$kk > 0)
})

test_that("import from object", {
  fp <- test_path("../data/morse/fit_sd.rds")
  mfit <- readRDS(fp)
  fit <- suppressMessages(import_morse(mfit, find_sd=TRUE, find_it=FALSE,
                                       param="estim", reset_hb=FALSE))

  expect_equal(length(fit), 1)
  fit <- fit[[1]]
  expect_true(is_parameter_set(fit))
  expect_equal(fit@model, "GUTS-RED-SD")
  expect_true(fit@param$kd > 0)
  expect_true(fit@param$hb > 0)
  expect_true(fit@param$z > 0)
  expect_true(fit@param$kk > 0)
})

test_that("reset hb", {
  fp <- test_path("../data/morse/fit_sd.rds")
  fit <- suppressMessages(import_morse(fp, find_sd=TRUE, find_it=FALSE,
                                       param="estim", reset_hb=FALSE))
  fit <- fit[[1]]
  expect_true(fit@param$hb > 0)

  fit <- suppressMessages(import_morse(fp, find_sd=TRUE, find_it=FALSE,
                                       param="estim", reset_hb=TRUE))
  fit <- fit[[1]]
  expect_true(fit@param$hb == 0)
})

test_that("import MCMC samples", {
  fp <- test_path("../data/morse/fit_sd.rds")
  fit <- suppressMessages(import_morse(fp, find_sd=TRUE, find_it=FALSE,
                                       param="all", reset_hb=FALSE))
  expect_equal(length(fit), 150)
  expect_true(all(sapply(fit, slot, name="model") == "GUTS-RED-SD"))

  fit <- fit[[1]]
  expect_true(fit@param$kd > 0)
  expect_true(fit@param$hb > 0)
  expect_true(fit@param$z > 0)
  expect_true(fit@param$kk > 0)

  fit <- suppressMessages(import_morse(fp, find_sd=TRUE, find_it=FALSE,
                                       param="all", mcmc_size=10))
  expect_equal(length(fit), 30)
  expect_true(all(sapply(fit, slot, name="model") == "GUTS-RED-SD"))
})

test_that("deprecated parameters", {
  fp <- test_path("../data/morse/fit_sd.rds")
  expect_warning(suppressMessages(import_morse(fp, find.IT=FALSE)))
  expect_warning(suppressMessages(import_morse(file=fp, find_it=FALSE)))
  expect_warning(suppressMessages(import_morse(fp, find_it=FALSE, reset.hb=TRUE)))
  expect_warning(suppressMessages(import_morse(fp, find_it=FALSE, mcmc.size=1)))

  fp <- test_path("../data/morse/fit_it.rds")
  expect_warning(suppressMessages(import_morse(fp, find.SD=FALSE)))
})

test_that("invalid arguments", {
  expect_error(import_morse("foo"))
  expect_error(import_morse(list()))
})
