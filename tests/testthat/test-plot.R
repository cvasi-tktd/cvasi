test_that("cvasi.drc", {
  df <- dose_response(minnow_it)
  plot.cvasi.drc(df)
  succeed()
})

test_that("cvasi.simulate", {
  df <- simulate(minnow_it)
  plot.cvasi.simulate(df)
  succeed()
})
