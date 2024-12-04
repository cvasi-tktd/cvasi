test_that("GUTS-RED solver", {
  # IT model
  expect_equal(solver(minnow_it)$time, minnow_it@times)
  expect_equal(solver(minnow_it, times=0:2)$time, 0:2)
  # SD model
  expect_equal(solver(minnow_it)$time, minnow_it@times)
  expect_equal(solver(minnow_it, times=0:2)$time, 0:2)
})

