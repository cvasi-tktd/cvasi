test_that("raised warnings", {
  # sim aborted
  expect_warning(minnow_it %>% simulate(maxsteps=10), "Simulation aborted")

  # suppress warnings
  foo <- minnow_it %>% simulate(maxsteps=10, .suppress=TRUE)
  expect_true(num_aborted(foo))
})

test_that("raised errors", {
  # unknown symbol
  expect_error(minnow_it %>% simulate(hmax=foobar), "foobar' not found")

  # invalid argument to solver
  expect_error(minnow_it %>% simulate(hmax=-1), "non-negative value")

  # suppress errors
  foo <- minnow_it %>% simulate(hmax=-1, .suppress=TRUE)
  expect_true(num_error(foo))
})
