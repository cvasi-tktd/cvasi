test_that("basic arguments", {
  # single scenario, basic data.frame
  sc <- new("EffectScenario", name="a") %>% set_times(1:3)
  rs <- set_exposure(sc, data.frame(t=1,c=2))
  expect_true(is_scenario(rs))
  expect_equal(rs@exposure@series$c, c(2))

  # single scenario, exposure series object
  es <- ExposureSeries(data.frame(t=1, c=2))
  rs <- set_exposure(sc, es, reset_times=TRUE)
  expect_true(is_scenario(rs))
  expect_equal(rs@exposure@series, es@series)
  expect_equal(rs@times, c(1))

  # single scenario, exposure series object, do not reset times
  rs <- set_exposure(sc, es, reset_times=FALSE)
  expect_true(is_scenario(rs))
  expect_equal(rs@exposure@series, es@series)
  expect_equal(rs@times, 1:3)

  # single scenario, data.frame containing column with exposure series
  df <- tibble::tibble(foo="bar", baz=1, series=c(es))
  rs <- set_exposure(sc, df, reset_times=FALSE)
  expect_type(rs, "list")
  expect_equal(length(rs), 1)
  expect_true(all(is_scenario(rs)))
  expect_equal(rs[[1]]@exposure@series, es@series)
  expect_equal(rs[[1]]@times, 1:3)
})

test_that("vectorized arguments", {
  sc <- new("EffectScenario", name="a") %>% set_times(1:3)
  sc2 <- new("EffectScenario", name="b") %>% set_times(1:3)
  es <- ExposureSeries(data.frame(t=1, c=2))
  es2 <- ExposureSeries(data.frame(t=1,c=3))

  # list of scenarios, single series
  rs <- set_exposure(list(sc,sc2), es, reset_times=FALSE)
  expect_type(rs, "list")
  expect_equal(length(rs), 2)
  expect_true(all(is_scenario(rs)))
  expect_equal(rs[[1]]@exposure@series, es@series)
  expect_equal(rs[[1]]@times, 1:3)
  expect_equal(rs[[1]]@name,  "a")
  expect_equal(rs[[2]]@exposure@series, es@series)
  expect_equal(rs[[2]]@times, 1:3)
  expect_equal(rs[[2]]@name,  "b")

  # single scenario, list of exposure series
  rs <- set_exposure(sc, list(es, es2), reset_times=FALSE)
  expect_type(rs, "list")
  expect_equal(length(rs), 2)
  expect_true(all(is_scenario(rs)))
  expect_equal(rs[[1]]@exposure@series, es@series)
  expect_equal(rs[[1]]@times, 1:3)
  expect_equal(rs[[2]]@exposure@series, es2@series)
  expect_equal(rs[[2]]@times, 1:3)

  # list of scenarios, list of exposure series
  rs <- set_exposure(list(sc,sc2), list(es,es2), reset_times=FALSE)
  expect_type(rs, "list")
  expect_equal(length(rs), 4)
  expect_true(all(is_scenario(rs)))
  expect_equal(rs[[1]]@exposure@series, es@series)
  expect_equal(rs[[1]]@times, 1:3)
  expect_equal(rs[[1]]@name,  "a")
  expect_equal(rs[[2]]@exposure@series, es2@series)
  expect_equal(rs[[2]]@times, 1:3)
  expect_equal(rs[[2]]@name,  "a")
  expect_equal(rs[[3]]@exposure@series, es@series)
  expect_equal(rs[[3]]@times, 1:3)
  expect_equal(rs[[3]]@name,  "b")
  expect_equal(rs[[4]]@exposure@series, es2@series)
  expect_equal(rs[[4]]@times, 1:3)
  expect_equal(rs[[4]]@name,  "b")
})

test_that("set_noexposure", {
  sc <- new("EffectScenario", name="a") %>%
    set_exposure(data.frame(time=1, conc=2)) %>%
    set_times(1:3)

  rs <- set_noexposure(sc)
  # times unmodified
  expect_equal(rs@times, sc@times)
  # exposure set to zero
  expect_true(all(rs@exposure@series[,2] == 0))
})

test_that("non-standard argument types", {
  # check that data.frame-like types, such as tibbles, work as expected
  tib <- tibble::tibble(t=0:3, c=0)
  rs <- set_exposure(new("EffectScenario", name="a"), tib)
  expect_equal(rs@exposure@series, as.data.frame(tib))

  # data.frames can contain numerical columns which carry units
  df <- data.frame(time=0:3, conc=0)
  units(df$time) <- units::as_units("days")
  units(df$conc) <- units::as_units("ug/L")
  sc <- set_exposure(new("EffectScenario", name="a"), df)
  # units must not be present afterwards
  expect_false(any(has_units(sc@exposure@series[,1])))
  expect_false(any(has_units(sc@exposure@series[,2])))
})

test_that("invalid arguments", {
  # nonsense
  expect_error(set_exposure(sc, 1))
  expect_error(set_exposure(1, es))
  # invalid time-series structure
  expect_error(set_exposure(1, data.frame()))
  expect_error(set_exposure(1, data.frame(time=1)))
  expect_error(set_exposure(1, data.frame(time="1")))
  expect_error(set_exposure(1, data.frame(time=1, conc="a")))
})
