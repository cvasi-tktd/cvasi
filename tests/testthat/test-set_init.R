test_that("single scenario", {
  sc <- GUTS_RED_IT()

  # basic use
  expect_equal(set_init(sc, c(D=23))@init, c(D=23, H=0))
  expect_equal(set_init(sc, c(D=23,H=42))@init, c(D=23, H=42))
  expect_equal(set_init(sc, c())@init, c(D=0, H=0))

  # lists
  expect_equal(set_init(sc, list(D=23))@init, c(D=23, H=0))
})

test_that("vector of scenarios", {
  sc1 <- GUTS_RED_IT() %>% set_init(c(D=1))
  sc2 <- GUTS_RED_IT() %>% set_init(c(D=2))

  rs <- set_init(c(sc1, sc2), c(D=23))
  expect_equal(rs[[1]]@init, c(D=23, H=0))
  expect_equal(rs[[2]]@init, c(D=23, H=0))
})

test_that("scenario w/o state vars", {
  sc <- new("EffectScenario")
  # gracefully accept any parameter if init is empty
  expect_equal(set_init(sc, list(a=1, b=2))@init, c(a=1, b=2))
  # ... but reject additional values on the second call
  expect_warning(sc %>% set_init(c(a=1)) %>% set_init(c(b=2)), "not state variables")
})

test_that("scenario sequence", {
  sc1 <- minnow_it %>% set_init(c(D=1))
  sc2 <- minnow_it %>% set_init(c(D=2))
  suppressMessages(seq <- sequence(list(sc1, sc2), breaks=2))

  seq <- seq %>% set_init(c(D=5))
  expect_equal(seq@scenarios[[1]]@init, c(D=5, H=0))
  expect_equal(seq@scenarios[[2]]@init, c(D=5, H=0))
})

test_that("invalid arguments", {
  sc <- GUTS_RED_IT()

  expect_error(set_init(sc))
  expect_error(set_init(sc, c(D=23, 42)), "unnamed elements")
  expect_error(set_init(sc, c(D="a")), "must be numeric")
  expect_error(set_init(sc, c(D=NA_real_)), "invalid values")
  expect_error(set_init(sc, c(D=NaN)), "invalid values")

  expect_warning(set_init(sc, c(foo=0)), "not state variable")
})
