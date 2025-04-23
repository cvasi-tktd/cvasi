test_that("supported types", {
  tms <- 0:5
  sc <- new("EffectScenario") %>% set_times(tms)

  # scenarios
  expect_equal(get_times(sc), tms)
  expect_equal(get_times(list(sc, sc)), list(tms, tms))

  # sequences
  suppressWarnings(sq <- sequence(list(sc)))
  expect_equal(get_times(sq), tms)
  sq <- sequence(list(sc, sc), breaks=3)
  expect_equal(get_times(sq), tms)
})

test_that("unsupported types", {
  expect_error(get_times(1), "not supported")
})
