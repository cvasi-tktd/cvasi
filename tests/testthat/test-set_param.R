test_that("vanilla usage", {
  sc1 <- new("EffectScenario", name="foo")
  sc2 <- new("EffectScenario", name="foo", tag="bar")

  p <- list(kd=1, hb=2, alpha=3, beta=4)
  ps1 <- parameter_set("foo", p)

  ## vanilla scenarios
  # single scenario, set vector of atomic values
  expect_equal(set_param(sc1, unlist(p))@param, p)
  # single scenario, single parameter set
  expect_equal(set_param(sc1, ps1)@param, p)
  # multiple scenarios, atomic vector
  lst <- set_param(c(sc1,sc1), unlist(p))
  expect_equal(lst[[1]]@param, p)
  expect_equal(lst[[2]]@param, p)
  # multiple scenarios, parameter set
  lst <- set_param(c(sc1,sc1), ps1)
  expect_equal(lst[[1]]@param, p)
  expect_equal(lst[[2]]@param, p)
})

test_that("special cases", {
  sc1 <- minnow_it
  sc2 <- sc1 %>% set_times(sc1@times + max(sc1@times))
  p <- list(kd=1)
  ps1 <- parameter_set(sc1@name, p, tag=sc1@tag)

  # all scenarios within a sequence need to be modified
  sequence(seq=c(sc1, sc2)) %>%
    set_param(ps1) -> seq
  expect_equal(length(scenarios(seq)), 2)
  expect_equal(scenarios(seq)[[1]]@param$kd, p$kd)
  expect_equal(scenarios(seq)[[2]]@param$kd, p$kd)
})

test_that("invalid arguments", {
  sc1 <- new("EffectScenario", name="foo")
  sc2 <- new("EffectScenario", name="foo", tag="bar")

  p <- list(kd=1)
  ps1 <- parameter_set("foo", p)
  ps2 <- parameter_set("foo", tag="bar", param=p)

  # multiple scenarios, one mismatch
  expect_error(set_param(c(sc1, sc2), ps1))
  # multiple parameter sets match, i.e. ambiguous assignments
  suppressMessages(expect_error(set_param(sc1, list(ps1, ps1))))
  # inconsistent types
  expect_error(set_param(sc1, list(1, ps1)))
  # model & tag dont match
  expect_error(set_param(sc1, ps2))
  # warn if invalid parameters were passed as argument
  sc3 <- new("EffectScenario", param.req=c("a"))
  expect_warning(set_param(sc3, c("b"=1)))
})
