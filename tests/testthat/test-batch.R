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

test_that("exposure types", {
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
  suppressMessages(b <- batch(base, list(0), times_from="exposure"))
  expect_equal(b@scenarios[[1]]@times, 0)
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
