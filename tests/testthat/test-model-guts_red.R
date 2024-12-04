test_that("GUTS-RED solver", {
  # IT model
  expect_equal(solver(minnow_it)$time, minnow_it@times)
  # SD model
  expect_equal(solver(minnow_it)$time, minnow_it@times)
})

