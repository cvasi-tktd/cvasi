
effect4tolerance <- function(tolerance) {
  mf <- epx(minnow_it,level=50,effect_tolerance=tolerance,ep_only=TRUE)[[1,1]]
  effect(minnow_it, factor=mf, ep_only=TRUE)
}

test_that("effect levels", {
  # avoid some common bogus messages on package load
  suppressMessages(suppressWarnings(require(purrr, quietly=TRUE)))

  ## requested effect levels
  expect_named(epx(minnow_it,level=23),c("scenario","L.EP23"))
  expect_named(epx(minnow_it,level=c(10,20)),c("scenario","L.EP10","L.EP20"))
})

test_that("effect_tolerance", {
  ## effect.tolerance
  ## result are getting more precise if we decrease the tolerance
  exact <- effect4tolerance(1e-10)
  expect_gt(abs(exact-effect4tolerance(1e-1)), abs(exact-effect4tolerance(1e-2)))
  expect_gt(abs(exact - effect4tolerance(1e-1)), 0.01)
  expect_lt(abs(exact - effect4tolerance(1e-1)), 1e-1)
  expect_lt(abs(exact - effect4tolerance(1e-2)), 1e-2)
  expect_lt(abs(exact - effect4tolerance(1e-3)), 1e-3)
  expect_lt(abs(exact - effect4tolerance(1e-4)), 1e-4)
  rm(exact)
})

test_that("min/max factor", {
  scen <- set_exposure(minnow_it, data.frame(t=1:5,c=1e-5)) # low exposure
  expect_warning(epx(scen,level=50,max_factor=1e3),regexp="multiplication factor out of range")
  scen <- set_exposure(minnow_it, data.frame(t=1:5,c=1e5)) # high exposure
  expect_warning(epx(scen,level=50,min_factor=1e-3),regexp="multiplication factor out of range")
  rm(scen)
  # one of two effect level fails, i.e. BM.EP50
  expect_warning(metsulfuron %>% set_endpoints("BM") %>% epx())
  suppressWarnings(rs <- metsulfuron %>% set_endpoints("BM") %>% epx())
  expect_equal(rs[[1,"BM.EP10"]], 0.395, tolerance=0.1)
  expect_equal(rs[[1,"BM.EP50"]], NA_real_)
})

test_that("factor cutoff", {
  cutoff <- 2000
  scen <- set_exposure(minnow_it, data.frame(t=1:5,c=1e-5))
  expect_true(all(epx(scen,ep_only=TRUE) > cutoff))
  expect_true(all(epx(scen,factor_cutoff=cutoff,ep_only=TRUE) == cutoff))
  rm(cutoff)

})

test_that("ep_only", {
  source(test_path("dummy.R"), local = TRUE)

  sc <- new("DummyScenario", endpoints="foo", fx=function(scenario, ...) {
    return(c(foo=scenario@exposure@series[1, 2]))
  })

  # single scenario, single EPx level
  rs <- epx(sc, level=10, ep_only=TRUE)
  expect_equal(nrow(rs), 1)
  expect_equal(names(rs), "foo.EP10")
  expect_equal(rs[[1, 1]], 0.1)

  # two scenarios, a two EPx levels
  rs <- epx(c(sc, sc), level=c(10, 50), ep_only=TRUE, effect_tolerance = 1e-5)
  expect_equal(nrow(rs), 2)
  expect_equal(names(rs), c("foo.EP10","foo.EP50"))
  expect_equal(rs[[1, 1]], 0.1)
  expect_equal(rs[[1, 2]], 0.5, tolerance=1e-5)
})

test_that("long_format", {
  source(test_path("dummy.R"), local = TRUE)
  sc <- new("DummyScenario", endpoints="foo", fx=function(scenario, ...) {
    return(c(foo=scenario@exposure@series[1, 2]))
  })

  # single scenario, single EPx level
  rs <- epx(sc, level=10, long_format=TRUE)
  expect_equal(nrow(rs), 1)
  expect_equal(names(rs), c("scenario","endpoint","level","EPx"))
  # single scenario, two EPx levels
  rs <- epx(sc, level=c(10, 50), long_format=TRUE)
  expect_equal(nrow(rs), 2)
  # two scenarios, a two EPx levels
  rs <- epx(c(sc,sc), level=c(10, 50), long_format=TRUE)
  expect_equal(nrow(rs), 4)
  expect_equal(names(rs), c("scenario","endpoint","level","EPx"))
  # again, but only endpoints=TRUE
  rs <- epx(c(sc,sc), level=c(10, 50), long_format=TRUE, ep_only=TRUE)
  expect_equal(nrow(rs), 4)
  expect_equal(names(rs), c("endpoint","level","EPx"))
})

test_that("error handling", {
  source(test_path("dummy.R"), local = TRUE)

  # single scenario, simulation fails
  sc <- new("DummyScenario", endpoints="foo", fx=function(...) stop("planned error"))

  expect_warning(rs <- epx(sc, level=10))
  expect_equal(names(rs), c("scenario","foo.EP10", "error"))
  expect_true(is.na(rs[['foo.EP10']]))
  expect_equal(rs[['error']], "planned error")

  # single scenario, two levels, one without valid results
  sc <- new("DummyScenario", endpoints="foo", fx=function(scenario, ...) {
    c <- scenario@exposure@series[1, 2]
    if(c > 60) return(NA_real_)
    return(c(foo=c/100))
  })

  expect_warning(rs <- epx(sc, level=c(10,90)), "Some scenarios have failed")
  expect_equal(names(rs), c("scenario","foo.EP10", "foo.EP90"))
  expect_equal(rs[['foo.EP10']], 10)
  expect_true(is.na(rs[['foo.EP90']]))

  # two scenarios, one passes, one fails
  sc1 <- new("DummyScenario", endpoints="foo", simulate=function(...) stop("planned error"))
  sc2 <- new("DummyScenario", endpoints="foo", fx=function(scenario, ...) {
    return(c(foo=scenario@exposure@series[1, 2]))
  })

  expect_warning(rs <- epx(list(sc1, sc2))) # continue with warning
  expect_true(all(!is.na(rs[2,c("foo.EP10","foo.EP50")]))) # EPx present
  expect_true(is.na(rs[2,"error"])) # no error msg
  expect_true(all(is.na(rs[1,c("foo.EP10","foo.EP50")]))) # no EPx
  expect_true(!is.na(rs[1,"error"])) # error msg
  rm(rs)

  # two scenarios, both failed
  expect_warning(rs <- epx(list(sc1, sc1), level=10))
  expect_equal(names(rs), c("scenario","foo.EP10", "error"))
  expect_true(all(is.na(rs[1:2,2])))
  expect_true(all(!is.na(rs[1:2,3])))
})
