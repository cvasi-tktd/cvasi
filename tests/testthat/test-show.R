test_that("show_scenario", {
  capture.output(suppressMessages(show_scenario(minnow_it)))
  succeed()
})

test_that("show_exposure", {
  capture.output(suppressMessages(show_exposure(minnow_it@exposure)))
  succeed()
})
