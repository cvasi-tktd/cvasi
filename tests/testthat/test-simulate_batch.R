test_that("simulate_batch", {

  exposure <- data.frame(time = Schmitt2013$t,
              conc = Schmitt2013$conc,
              trial = Schmitt2013$ID)

  out <- simulate_batch(model_base = metsulfuron,
                               treatments = exposure)
  expect_equal(nrow(exposure), nrow(out))

  tol <- 1e-5

  expect_equal(out[1,"BM"], 50)
  expect_equal(out[41,"BM"], 50)
  expect_equal(out[1,"trial"], "T0")
  expect_equal(out[41,"trial"], "T3.2")
  expect_equal(out[8,"BM"],91.13051, tolerance=tol)
  expect_equal(out[16,"BM"], 85.35812, tolerance=tol)
  expect_equal(out[8,"M_int"], 0, tolerance=tol)
  expect_equal(out[16,"M_int"], 9.645036, tolerance=tol)

})
