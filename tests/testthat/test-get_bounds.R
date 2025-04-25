test_that("scenario", {
  sc <- new("EffectScenario", param.bounds=list())
  expect_equal(get_bounds(sc), list())

  sc <- new("EffectScenario", param.bounds=list(foo=c(0, 1)))
  expect_equal(get_bounds(sc), list(foo=c(0, 1)))
})

test_that("sequence", {
  sc <- new("EffectScenario", param.bounds=list()) %>% set_times(0:2)
  suppressWarnings(sq <- sequence(list(sc)))
  expect_equal(get_bounds(sq), list())

  sc <- new("EffectScenario", param.bounds=list(foo=c(0, 1)))
  suppressWarnings(sq <- sequence(list(sc)))
  expect_equal(get_bounds(sq), list(foo=c(0, 1)))
})

test_that("caliset", {
  sc <- new("EffectScenario", param.bounds=list())
  cs <- caliset(sc, data.frame(t=0, n=0))
  expect_equal(get_bounds(cs), list())

  sc <- new("EffectScenario", param.bounds=list(foo=c(0, 1)))
  cs <- caliset(sc, data.frame(t=0, n=0))
  expect_equal(get_bounds(cs), list(foo=c(0, 1)))
})

test_that("invalid arguments", {
  expect_error(get_bounds(), "missing")
  expect_error(get_bounds(NULL), "must be a scenario")
  expect_error(get_bounds(NA), "must be a scenario")
  sc <- new("EffectScenario")
  expect_error(get_bounds(list(sc, sc)), "length one")
})
