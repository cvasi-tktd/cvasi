test_that("ids", {
  base <- minnow_it %>% set_times(0:4)

  suppressMessages(b <- batch(base, list("bar"=0, "baz"=1), id_col="foo"))
  df <- simulate(b)
  expect_contains(names(df), "foo")
  expect_equal(df[, "foo"], rep(c("bar", "baz"), each=5))
})

test_that("format argument", {
  base <- minnow_it

  # long format
  suppressMessages(b <- batch(base, list(foo=0), format="long"))
  df <- simulate(b)
  expect_equal(names(df), c("time","D","H","S","trial"))

  # wide format
  suppressMessages(b <- batch(base, list(foo=0), format="wide"))
  df <- simulate(b)
  expect_equal(names(df), c("time","D_foo","H_foo","S_foo"))

  # wide format, single column
  suppressMessages(b <- batch(base, list(foo=0), format="wide", select="S"))
  df <- simulate(b)
  expect_equal(names(df), c("time","foo"))
})

test_that("select argument", {
  base <- minnow_it

  suppressMessages(b <- batch(base, list(0), select="S"))
  df <- simulate(b)
  expect_equal(names(df), c("time","S", "trial"))
})
