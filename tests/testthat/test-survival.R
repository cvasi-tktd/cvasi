test_that("basic functionality", {
  # GUTS-RED-IT
  sim <- minnow_it %>% simulate()
  expect_warning(surv <- minnow_it %>% survival())
  expect_equal(surv$survival, sim$S)
  # GUTS-RED-SD
  sim <- minnow_sd %>% simulate()
  expect_warning(surv <- minnow_sd %>% survival())
  expect_equal(surv$survival, sim$S)
})

test_that("unsupported model", {
  expect_error(expect_warning(survival(metsulfuron)))
})

