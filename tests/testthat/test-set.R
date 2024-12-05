test_that("set_init", {
  # basics
  expect_equal(set_init(GUTS_RED_IT(),c(D=23))@init, c(D=23,H=0))
  expect_equal(set_init(GUTS_RED_IT(),c(D=23,H=42))@init, c(D=23,H=42))

  # auto-convert lists to vectors
  expect_equal(set_init(GUTS_RED_IT(),list(D=23))@init, c(D=23,H=0))
  # do not accept non-numeric init values
  expect_error(set_init(GUTS_RED_IT(),c(D="23")))
  # prints a warning when invalid/unused parameters are supplied
  expect_warning(set_init(GUTS_RED_IT(),c(XX=0)))
  # prints a warning when unnamed values are supplied
  expect_warning(set_init(GUTS_RED_IT(),c(0)))
  # apply to vector of scenarios
  set_init(c(GUTS_RED_IT(),GUTS_RED_SD()), c(D=23)) -> lst
  expect_equal(lst[[1]]@init, c(D=23,H=0))
  expect_equal(lst[[2]]@init, c(D=23,H=0))

  # error if no argument provided
  expect_error(set_init(GUTS_RED_IT()))

  # gracefully accept any parameter if init is empty
  expect_equal(set_init(new("EffectScenario"), list(a=1,b=2))@init, c(a=1,b=2))
  # ... but reject additional values on the second call
  expect_warning(new("EffectScenario") %>% set_init(c(a=1)) %>% set_init(c(b=2)),
                 regexp="unused init variables: b")
})

test_that("set_window", {
  # set both arguments at once
  Lemna_Schmitt() %>%
    set_window(length=1, interval=2) -> sc
  expect_equal(sc@window.length, 1)
  expect_equal(sc@window.interval, 2)

  # set only a single argument: length
  Lemna_Schmitt() %>%
    set_window(interval=1, length=2) %>%
    set_window(length=42) -> sc
  expect_equal(sc@window.length, 42)
  expect_equal(sc@window.interval, 1)

  # set only a single argument: interval
  Lemna_Schmitt() %>%
    set_window(interval=1, length=2) %>%
    set_window(interval=42) -> sc
  expect_equal(sc@window.length, 2)
  expect_equal(sc@window.interval, 42)

  # disable both by setting one to -1
  Lemna_Schmitt() %>%
    set_window(length=1, interval=2) %>%
    set_window(length=-1) -> sc
  expect_equal(sc@window.length, -1)
  expect_equal(sc@window.interval, -1)

  Lemna_Schmitt() %>%
    set_window(length=1, interval=2) %>%
    set_window(interval=-1) -> sc
  expect_equal(sc@window.length, -1)
  expect_equal(sc@window.interval, -1)

  # vectorized scenario argument
  c(Lemna_Schmitt(), Lemna_Schmitt()) %>% set_window(length=1, interval=2) -> scs
  expect_equal(length(scs), 2)
  expect_equal(scs[[1]]@window.length, 1)
  expect_equal(scs[[2]]@window.length, 1)
  expect_equal(scs[[1]]@window.interval, 2)
  expect_equal(scs[[2]]@window.interval, 2)

  # disable windows
  sc <- new("EffectScenario") %>%
    set_window(length=1, interval=2) %>%
    set_nowindow()
  expect_equal(sc@window.length, -1)
  expect_equal(sc@window.interval, -1)

  sc <- new("EffectScenario") %>%
    set_window(length=1, interval=2)
  scl <- set_nowindow(c(sc, sc))
  expect_equal(scl[[1]]@window.interval, -1)
  expect_equal(scl[[2]]@window.interval, -1)

  # invalid inputs
  sc <- new("EffectScenario")
  expect_error(set_window(sc, length=0))
  expect_error(set_window(sc, length=c(1,2)))
  expect_error(set_window(sc, interval=c(1,2)))
  expect_error(set_window(sc, interval=1))
  expect_error(set_window(sc, length=1))
})

test_that("set_endpoints", {
  # single scenario
  expect_equal(set_endpoints(new("EffectScenario"), "A")@endpoints, "A")
  expect_equal(set_endpoints(new("EffectScenario"), c("A","B"))@endpoints, c("A","B"))

  # vectorized input
  expect_equal(set_endpoints(c(new("EffectScenario"),new("EffectScenario")), "A") %>%
                 sapply(function(x) slot(x,"endpoints")), c("A","A"))

  # invalid input ?
})

test_that("set_forcings", {
  sc <- new("EffectScenario", forcings.req=c("rad","tmp"))
  # std evaluation
  expect_equal(set_forcings(sc, list(rad=data.frame(time=0:2, rad=0)))@forcings, list(rad=data.frame(time=0:2, rad=0)))
  expect_equal(set_forcings(sc, list(rad=1))@forcings, list(rad=data.frame(time=0, rad=1)))
  expect_equal(set_forcings(sc, list(rad=1, tmp=2))@forcings, list(rad=data.frame(time=0, rad=1),
                                                                   tmp=data.frame(time=0, tmp=2)))
  # non-standard evaluation
  expect_equal(set_forcings(sc, rad=1)@forcings, list(rad=data.frame(time=0, rad=1)))
  expect_equal(set_forcings(sc, rad=1, tmp=2)@forcings, list(rad=data.frame(time=0, rad=1),
                                                             tmp=data.frame(time=0, tmp=2)))
  ts <- data.frame(time=c(0,1,2), val=c(2.1,3,4.2))
  expect_equal(set_forcings(sc, rad=ts)@forcings, list(rad=ts))

  # vectorized scenarios
  expect_equal(set_forcings(c(sc,sc), rad=1) %>% sapply(function(x) slot(x, "forcings")),
               list(rad=data.frame(time=0, rad=1), rad=data.frame(time=0, rad=1)))

  # invalid series name
  expect_warning(set_forcings(sc, foo=1), regexp="unused forcing")
  expect_warning(set_forcings(sc, list(foo=1)), regexp="unused forcing")
  # unnamed series
  expect_warning(set_forcings(sc, list(1)), regexp="unnamed forcing")
  # wrong number of columns in data.frame
  expect_error(set_forcings(sc, rad=data.frame(a=0, b=1, c=2)), regexp="two columns")
  # vectorized input in non-std eval
  expect_error(set_forcings(sc, rad=c(1,2,3)), regexp="rad.*invalid type")
})

test_that("set_moa", {
  sc <- new("EffectScenario", param.req=c("foo", "MoA"))
  # std input
  expect_equal(set_mode_of_action(sc, 1)@param, list(MoA=1))
  expect_equal(set_moa(sc, 1)@param, list(MoA=1))
  # vectorized scenarios
  expect_equal(set_moa(c(sc,sc), 1) %>% sapply(function(x) slot(x, "param")), list(MoA=1, MoA=1))

  # MoA not supported
  expect_error(set_moa(new("EffectScenario"), 1), regexp="does not support modes of action")
})
