test_that("raised warnings", {
  # sim aborted
  expect_warning(minnow_it %>% simulate(maxsteps=10), "Simulation aborted")

  # suppress warnings
  foo <- minnow_it %>% simulate(maxsteps=10, .suppress=TRUE)
  expect_true(num_aborted(foo))

  # warning raised by solver
  source(test_path("dummy.R"), local = TRUE)
  instable <- new("DummyScenario", solver=function(...) {
    warning("planned warning")
    df <- data.frame(t=0, A=1)
    attr(df, "desolve_diagn") <- list(istate=-42) # magic value from deSolve, cf. [num_info()]
    df
  })
  expect_warning(rs <- simulate(instable), "planned warning")
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
