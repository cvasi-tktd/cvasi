test_that("scenario", {
  # single boundary
  bds <- list(kd=c(2, 3))
  sc <- new("EffectScenario") %>% set_bounds(bds)
  expect_equal(get_bounds(sc), bds)

  # update boundary
  bds <- list(kd=c(23, 24))
  sc <- sc %>% set_bounds(bds)
  expect_equal(get_bounds(sc), bds)

  # multiple boundaries
  bds <- list(kd=c(0, 1), hb=c(2, 3))
  sc <- new("EffectScenario") %>% set_bounds(bds)
  expect_equal(get_bounds(sc), bds)
})

test_that("sequence", {
  bds <- list(kd=c(23, 24))
  sc <- new("EffectScenario") %>% set_times(0:5)
  suppressWarnings(sq <- sequence(list(sc)))
  sq <- set_bounds(sq, bds)
  expect_equal(get_bounds(sq), bds)
})

test_that("caliset", {
  bds <- list(kd=c(23, 24))
  cs <- caliset(new("EffectScenario"), data.frame(t=0, a=1))
  cs <- set_bounds(cs, bds)
  expect_equal(get_bounds(cs), bds)
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
