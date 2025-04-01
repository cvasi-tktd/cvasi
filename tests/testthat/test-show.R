test_that("show_scenario", {
  capture.output(suppressMessages(show_scenario(minnow_it)))
  succeed()
})

test_that("show_exposure", {
  capture.output(suppressMessages(show_exposure(minnow_it@exposure)))
  succeed()
})

test_that("show biomass transfers", {
  out <- cli::cli_fmt(capture.output(show_scenario(metsulfuron %>% set_transfer(interval=23, biomass=42))))
  expect_true(any(stringr::str_detect(out, "regular.*interval=23.*BM=42")))

  out <- cli::cli_fmt(capture.output(show_scenario(metsulfuron %>% set_transfer(times=c(0,1,2), biomass=42))))
  expect_true(any(stringr::str_detect(out, "times=c\\(0,1,2\\).*BM=42")))
})

test_that("show moving windows", {
  out <- cli::cli_fmt(capture.output(show_scenario(metsulfuron %>% set_window(length=1, interval=2))))
  expect_true(any(stringr::str_detect(out, "length=1.*interval=2")))
})
