test_that("arg times", {
  sc <- minnow_it
  orig_times <- sc@times
  new_times <- orig_times + 1

  rs <- simulate(sc)
  expect_equal(rs$time, orig_times)

  rs <- simulate(sc, times=new_times)
  expect_equal(rs$time, new_times)

  # invalid
  sc@times <- 1
  expect_error(simulate(sc), "at least two output time")
})

#TODO in simulate_scenario, it appears if the conditions array may sometimes have an unknown format?
# ─cvasi:::simulate_scenario(scenario, times = times, ...) at cvasi/R/simulate.R:244:3
# 7.       └─rlang::abort(c("Simulation failed", err[!is.na(err)]), class = "cvasi_error") at cvasi/R/simulate.R:216:7
