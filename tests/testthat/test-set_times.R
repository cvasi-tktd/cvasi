test_that("basic use", {
  sc <- new("EffectScenario")
  tms <- 0:10

  sc <- set_times(sc, tms)
  expect_equal(sc@times, tms)
})

test_that("set units", {
  sc <- new("EffectScenario")
  tms <- units::set_units(0:10, "h")
  sc <- set_times(sc, tms)
  expect_equal(sc@times, 0:10)
})

test_that("sequence", {
  sc <- new("EffectScenario") %>% set_times(0:5)
  sq <- sequence(list(sc, sc), breaks=3)

  expect_equal(get_times(sq), 0:5)

  sq <- sq %>% set_times(1:5)
  expect_equal(get_times(sq), 1:5)

  sq <- sq %>% set_times(10:15)
  expect_equal(get_times(sq), 10:15)
})
