test_that("basic arguments", {
  # single scenario, basic data.frame
  sc <- new("EffectScenario", name="a") %>% set_times(1:3)
  rs <- set_exposure(sc, data.frame(t=1, c=2), FALSE)
  expect_true(is_scenario(rs))
  expect_equal(rs@exposure@series$c, c(2))

  # single scenario, exposure series object
  es <- ExposureSeries(data.frame(t=1:2, c=2))
  rs <- set_exposure(sc, es, reset_times=TRUE)
  expect_true(is_scenario(rs))
  expect_equal(rs@exposure@series, es@series)
  expect_equal(rs@times, c(1:2))

  # single scenario, exposure series object, do not reset times
  rs <- set_exposure(sc, es, reset_times=FALSE)
  expect_true(is_scenario(rs))
  expect_equal(rs@exposure@series, es@series)
  expect_equal(rs@times, 1:3)

  # drop additional columns
  expect_warning(rs <- set_exposure(sc, data.frame(a=0, b=1, c=2), reset_times=FALSE))
  expect_equal(length(rs@exposure@series), 2)
  expect_equal(names(rs@exposure@series), c("a", "b"))
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

test_that("scenario sequence", {
  sc <- minnow_it %>% set_noexposure()
  suppressMessages(seq <- sequence(list(sc, sc), breaks=2))

  df <- data.frame(foo=1, bar=2)
  seq <- seq %>% set_exposure(df, reset_times=FALSE)
  expect_equal(seq@scenarios[[1]]@exposure@series, df)
  expect_equal(seq@scenarios[[2]]@exposure@series, df)
})

test_that("set_noexposure", {
  sc <- new("EffectScenario", name="a") %>%
    set_exposure(data.frame(time=1, conc=2), FALSE) %>%
    set_times(1:3)

  rs <- sc %>% set_exposure(no_exposure())
  # times unmodified
  expect_equal(rs@times, sc@times)
  # exposure set to zero
  expect_true(all(rs@exposure@series[,2] == 0))

  rs <- set_noexposure(sc)
  # times unmodified
  expect_equal(rs@times, sc@times)
  # exposure set to zero
  expect_true(all(rs@exposure@series[,2] == 0))

  # multiple scenarios at once
  rs <- list(sc, sc) %>% set_noexposure()
  expect_true(all(rs[[1]]@exposure@series[,2] == 0))
  expect_true(all(rs[[2]]@exposure@series[,2] == 0))
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
  expect_false(any(has_units(sc@exposure@series[, 1])))
  expect_false(any(has_units(sc@exposure@series[, 2])))
})

test_that("invalid arguments", {
  sc <- minnow_it
  es <- sc@exposure
  # exposure series too short
  expect_error(set_exposure(sc, data.frame(t=0, c=0)))

  # nonsense
  expect_error(set_exposure(sc, 1))
  expect_error(set_exposure(1, es))
  # invalid time-series structure
  expect_error(set_exposure(1, data.frame()))
  expect_error(set_exposure(1, data.frame(time=1)))
  expect_error(set_exposure(1, data.frame(time="1")))
  expect_error(set_exposure(1, data.frame(time=1, conc="a")))
})

test_that("check_exposure", {
  # no series
  expect_error(check_exposure())
  expect_error(check_exposure(foo))
  expect_error(check_exposure(1))
  expect_error(check_exposure(NULL))
  # too few columns
  expect_error(check_exposure(data.frame()), "two columns")
  expect_error(check_exposure(data.frame(a=0)), "two columns")
  # empty series
  expect_error(check_exposure(data.frame(t=numeric(0), e=numeric(0))), "least one row")
  # non-numeric columns
  expect_error(check_exposure(data.frame(t="foo", e=0)), "numeric values")
  expect_error(check_exposure(data.frame(t=0, e="bar")), "numeric values")
  expect_error(check_exposure(data.frame(t="foo", e="bar")), "numeric values")
  # missing/invalid values
  expect_error(check_exposure(data.frame(t=NA_real_, e=0)), "invalid values")
  expect_error(check_exposure(data.frame(t=Inf, e=0)), "invalid values")
  expect_error(check_exposure(data.frame(t=NaN, e=0)), "invalid values")
  expect_error(check_exposure(data.frame(t=0, e=NA_real_)), "invalid values")
  expect_error(check_exposure(data.frame(t=0, e=Inf)), "invalid values")
  expect_error(check_exposure(data.frame(t=0, e=NaN)), "invalid values")
  # time not sorted
  expect_error(check_exposure(data.frame(t=c(0,1,0), e=0)), "ascending order")

  # additional columns
  expect_warning(check_exposure(data.frame(t=0, e=0, f=0)), "additional columns")

  # valid argument
  check_exposure(data.frame(t=0:3, e=0))
  # valid arguments with units
  foo <- units::set_units(1, sec)
  check_exposure(data.frame(t=c(foo, foo), e=0))
  check_exposure(data.frame(t=0, e=c(foo, foo)))
})

test_that("ExposureSeries constructor", {
  # shortened test set
  expect_error(ExposureSeries())
  expect_error(ExposureSeries(1))
  expect_error(ExposureSeries(data.frame()), "two columns")
  expect_error(ExposureSeries(data.frame(t=numeric(0), e=numeric(0))), "least one row")
  expect_error(ExposureSeries(data.frame(t="foo", e=0)), "numeric values")
  expect_error(ExposureSeries(data.frame(t=NA_real_, e=0)), "invalid values")
  expect_warning(es <- ExposureSeries(data.frame(t=0, e=0, f=0)), "additional columns")
  # where additional columns dropped?
  expect_equal(length(es@series), 2)

  # valid arguments
  df <- data.frame(t=0:3, e=0)
  es <- ExposureSeries(df)
  expect_equal(es@series, df)
  # valid arguments with units
  foo <- units::set_units(1, sec)
  df <- data.frame(t=c(foo, foo), e=0)
  es <- ExposureSeries(df)
  expect_equal(es@series, df)
})
