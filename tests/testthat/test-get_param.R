test_that("supported types", {
  param <- list("foo"=1, "bar"=2)
  # empty argument
  expect_equal(get_param(list()), list())
  # scenario
  sc <- new("EffectScenario") %>%
    set_param(param) %>%
    set_times(0:5)
  expect_equal(get_param(sc), param)
  # list of scenarios
  expect_equal(get_param(c(sc, sc)), list(param, param))

  # sequence
  suppressMessages(scs <- sequence(list(sc, sc), breaks=3))
  expect_equal(get_param(scs), list(param, param))

  # parameter set
  ps <- parameter_set("foobar", param)
  expect_equal(get_param(ps), param)
})

test_that("unsupported types", {
  expect_error(get_param(1))
})

test_that("get_rparam", {
  rp <- c("foo", "bar")
  sc1 <- new("EffectScenario", param.req=rp) %>% set_times(1:5)

  # empty argument
  expect_equal(get_rparam(list()), list())
  # single scenario
  expect_equal(get_rparam(sc1), rp)
  # multiple scenarios
  expect_equal(get_rparam(list(sc1, sc1)), list(rp, rp))
  # sequence
  suppressMessages(sq <- sequence(list(sc1, sc1), breaks=3))
  expect_equal(get_rparam(sq), list(rp, rp))

  # invalid argument
  expect_error(get_rparam(1))
})
