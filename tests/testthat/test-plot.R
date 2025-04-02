test_that("cvasi_drc", {
  df <- dose_response(minnow_it)
  plot.cvasi_drc(df)
  succeed()
})

test_that("cvasi_simulate", {
  df <- simulate(minnow_it)
  plot.cvasi_simulate(df)
  succeed()
})
