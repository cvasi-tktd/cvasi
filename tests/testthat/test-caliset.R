test_that("invalid scenario argument", {
  sc <- minnow_it

  expect_error(caliset(), "scenario. is missing")
  expect_error(caliset(scenario=NULL), "must be a scenario")
  expect_error(caliset(scenario=list()), "must be a scenario")
  expect_error(caliset(scenario=list(sc, sc)), "must be a scenario")
  expect_error(caliset(scenario=1), "must be a scenario")
})

test_that("invalid data argument", {
  sc <- minnow_it

  expect_error(caliset(sc), "is missing")
  expect_error(caliset(sc, 1), "data.frame")
  expect_error(caliset(sc, data.frame()), "two columns")
  expect_error(caliset(sc, data.frame(t=1)), "two columns")
  expect_error(caliset(sc, data.frame(t=numeric(0), o=numeric(0))), "not be empty")
  expect_error(caliset(sc, data.frame(t=1, o="a")), "be numerical")
  expect_error(caliset(sc, data.frame(t="a", o=1)), "be numerical")
  expect_error(caliset(sc, data.frame(t="a", o="a")), "be numerical")
  # 1st column
  expect_error(caliset(sc, data.frame(t=NA, o=1)), "invalid value")
  expect_error(caliset(sc, data.frame(t=NaN, o=1)), "invalid value")
  expect_error(caliset(sc, data.frame(t=Inf, o=1)), "invalid value")
  expect_error(caliset(sc, data.frame(t=c(1, 0), o=1)), "ascending order")
  # 2nd column
  expect_error(caliset(sc, data.frame(t=1, o=NaN)), "invalid value")
  expect_error(caliset(sc, data.frame(t=1, o=Inf)), "invalid value")
})

test_that("invalid weight argument", {
  sc <- minnow_it
  df1 <- data.frame(t=1, o=2)
  dfn <- data.frame(t=1:8, o=2)

  expect_error(caliset(sc, df1, NA))
  expect_error(caliset(sc, df1, NaN))
  expect_error(caliset(sc, df1, Inf))
  expect_error(caliset(sc, df1, "foo"))

  expect_error(caliset(sc, df1, 1:3))
  expect_error(caliset(sc, dfn, 1:3))
})

test_that("caliset creation", {
  sc <- minnow_it
  df <- data.frame(t=1:10, o=0)
  wgt1 <- 1
  wgtn <- 2:11
  tag <- list("foo", 23)

  cs <- caliset(sc, df, wgt1, tag)
  expect_equal(cs@scenario, sc)
  expect_equal(cs@data, df)
  expect_equal(cs@weight, rep(wgt1, nrow(df)))
  expect_equal(cs@tag, tag)

  cs <- caliset(sc, df, wgtn)
  expect_equal(cs@weight, wgtn)
  expect_null(cs@tag)
})

test_that("caliset with sequences", {
  sc <- new("EffectScenario") %>% set_times(0:6)
  sq <- sequence(list(sc, sc), breaks=3)
  cs <- caliset(sq, data.frame(t=0:10, n=0))

  expect_equal(cs@scenario, sq)
})
