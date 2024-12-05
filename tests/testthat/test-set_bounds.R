test_that("get/set on scenario", {
  # single boundary
  bds <- list(kd=c(2, 3))
  sc <- GUTS_RED_IT() %>% set_bounds(bds)
  expect_equal(get_bounds(sc), bds)

  bds <- list(kd=c(23, 24))
  sc <- GUTS_RED_IT() %>% set_bounds(bds)
  expect_equal(get_bounds(sc), bds)

  # multiple boundaries
  bds <- list(kd=c(0, 1), hb=c(2, 3))
  sc <- GUTS_RED_IT() %>% set_bounds(bds)
  expect_equal(get_bounds(sc), bds)

  # update boundaries
  GUTS_RED_IT() %>%
    set_bounds(list(hb=c(0, 1))) %>%
    set_bounds(list(hb=c(3, 4))) -> sc
    expect_equal(get_bounds(sc), list(hb=c(3,4)))
})

test_that("on caliset", {
  bds <- list(kd=c(23,24))
  cs <- caliset(GUTS_RED_IT(), data.frame(t=0, a=1))
  cs <- set_bounds(cs, bds)
  expect_equal(get_bounds(cs@scenario), bds)
})

test_that("list of scenarios", {
  bds <- list(kd=c(2, 3))
  scs <- list(GUTS_RED_IT(), GUTS_RED_IT()) %>%
    set_bounds(bds)

  expect_equal(get_bounds(scs[[1]]), bds)
  expect_equal(get_bounds(scs[[2]]), bds)
})

test_that("invalid arguments", {
  sc <- GUTS_RED_IT() %>% set_bounds(list(kd=c(2, 3)))
  # unknown parameter
  expect_warning(set_bounds(sc, list(foobar=c(1,2))))
  # not a list
  expect_error(check_bounds("a")) # check function explicitly
  # no elements
  expect_error(set_bounds(sc, list()))
  # no names
  expect_error(set_bounds(sc, list(1)))
  # unnamed elements
  expect_error(set_bounds(sc, list(a=c(1,2), "b")))
  # values not of length 2
  expect_warning(expect_error(set_bounds(sc, list(a=c(1)))))
  # values not numeric
  expect_warning(expect_error(set_bounds(sc, list(a=c("a","b")))))
  # values contain NA
  expect_warning(expect_error(set_bounds(sc, list(a=c(1, NA_real_)))))
  # wrong order
  expect_warning(expect_error(set_bounds(sc, list(kd=c(3, 2)))))
})

test_that("get_bounds", {
  sc <- new("EffectScenario")
  sc@param.req <- c("foo", "bar")

  bnds <- list(foo=c(1, 3))
  sc <- set_bounds(sc, bnds)
  expect_equal(get_bounds(sc), bnds)

  # invalid arguments
  expect_error(get_bounds("foo"))
  expect_error(get_bounds(c(sc, sc)))
})
