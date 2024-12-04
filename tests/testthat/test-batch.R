test_that("exposure levels", {
  base <- new("EffectScenario")

  # single level
  suppressMessages(b <- batch(base, list(23)))
  expect_s4_class(b, "SimulationBatch")
  expect_equal(length(b@scenarios), 1)
  expect_equal(b@scenarios[[1]]@exposure@series[,2], 23)

  # multiple levels
  suppressMessages(b <- batch(base, list(0, 1, 2, 3)))
  expect_s4_class(b, "SimulationBatch")
  expect_equal(length(b@scenarios), 4)
  expect_equal(b@scenarios[[1]]@exposure@series[,2], 0)
  expect_equal(b@scenarios[[4]]@exposure@series[,2], 3)
})

test_that("list of exposure types", {
  base <- new("EffectScenario")

  # constant values
  suppressMessages(b <- batch(base, list(1, 2)))
  expect_equal(length(b@scenarios), 2)
  expect_equal(b@scenarios[[1]]@exposure@series[,2], 1)
  expect_equal(b@scenarios[[2]]@exposure@series[,2], 2)

  # data.frames
  suppressMessages(b <- batch(base, list(
    data.frame(t=0, c=3),
    data.frame(t=0, c=4)
  )))
  expect_s4_class(b, "SimulationBatch")
  expect_equal(length(b@scenarios), 2)
  expect_equal(b@scenarios[[1]]@exposure@series[,2], 3)
  expect_equal(b@scenarios[[2]]@exposure@series[,2], 4)

  # ExposureSeries objects
  suppressMessages(b <- batch(base, list(
    ExposureSeries(data.frame(t=0, c=5)),
    ExposureSeries(data.frame(t=0, c=6))
  )))
  expect_s4_class(b, "SimulationBatch")
  expect_equal(length(b@scenarios), 2)
  expect_equal(b@scenarios[[1]]@exposure@series[,2], 5)
  expect_equal(b@scenarios[[2]]@exposure@series[,2], 6)
})

test_that("data.frame of exposure series", {
  base <- new("EffectScenario")

  # single trial
  df1 <- data.frame(t=0:4, c=1, trial="foo")
  suppressMessages(b <- batch(base, df1))
  expect_equal(length(b@scenarios), 1)
  expect_equal(names(b@scenarios), "foo")
  expect_equal(b@scenarios[[1]]@exposure@series[,1], 0:4)
  expect_equal(b@scenarios[[1]]@exposure@series[,2], rep(1, 5))

  # multiple trials, custom id column name
  df2 <- rbind(data.frame(t=0, c=1, baz="foo"),
               data.frame(t=0, c=2, baz="bar"))
  suppressMessages(b <- batch(base, df2, id_col="baz"))
  expect_equal(length(b@scenarios), 2)
  expect_equal(names(b@scenarios), c("foo","bar"))
  expect_equal(b@scenarios[[1]]@exposure@series[,2], 1)
  expect_equal(b@scenarios[[2]]@exposure@series[,2], 2)

  ## error modes
  # too few columns
  expect_error(batch(base, data.frame(t=0:1, trial="foo")))
  # too many columns
  expect_error(batch(base, data.frame(t=0:1, conc=0, trial="foo", bar="baz")))
  # id column not present
  expect_error(batch(base, data.frame(t=0:1, conc=0, bar="baz")))
  # missing trial ids
  expect_error(batch(base, data.frame(t=0:1, conc=0, trial=c("foo", NA_character_))))
})

test_that("id column", {
  base <- minnow_it

  # default value
  suppressMessages(b <- batch(base, list(0)))
  expect_true(is.character(b@id_col))

  # custom value
  suppressMessages(b <- batch(base, list(0), id_col="foo"))
  expect_equal(b@id_col, "foo")
})

test_that("unique exposure ids", {
  base <- minnow_it

  # automatic ids
  suppressMessages(b <- batch(base, list(0, 1)))
  expect_equal(names(b@scenarios), c("trial1", "trial2"))

  # manual ids
  b <- batch(base, list(a=0, b=1))
  expect_equal(names(b@scenarios), c("a", "b"))
})

test_that("unique exposure ids", {
  base <- new("EffectScenario") %>% set_times(0:10)

  # times from scenario
  suppressMessages(b <- batch(base, list(0), times_from="scenario"))
  expect_equal(b@scenarios[[1]]@times, 0:10)
  suppressMessages(b <- batch(base, list(data.frame(t=1, c=0)), times_from="scenario"))
  expect_equal(b@scenarios[[1]]@times, 0:10)

  # times from exposure
  expect_error(suppressMessage(batch(base, list(0), times_from="exposure")))
  suppressMessages(b <- batch(base, list(data.frame(t=1:4, c=0)), times_from="exposure"))
  expect_equal(b@scenarios[[1]]@times, 1:4)
})

test_that("format argument", {
  base <- new("EffectScenario")

  suppressMessages(b <- batch(base, list(0), format="long"))
  expect_equal(b@format, "long")
  suppressMessages(b <- batch(base, list(0), format="wide"))
  expect_equal(b@format, "wide")
})

test_that("select argument", {
  base <- new("EffectScenario")

  suppressMessages(b <- batch(base, list(0), select=NULL))
  expect_null(b@select)
  suppressMessages(b <- batch(base, list(0), select="foo"))
  expect_equal(b@select, "foo")
})

test_that("invalid arguments", {
  expect_error(batch())
  expect_error(batch(NULL, list(0)))
  expect_error(batch(list(), list(0)))
  expect_error(batch(list(1), list(0)))

  expect_error(batch(minnow_it))
  expect_error(batch(minnow_it, 1))
  expect_error(batch(minnow_it, list()))
})
