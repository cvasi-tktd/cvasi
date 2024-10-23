
effect4tolerance <- function(tolerance) {
  mf <- epx(minnow_it,level=50,effect_tolerance=tolerance,ep_only=TRUE)[[1,1]]
  effect(minnow_it, factor=mf, ep_only=TRUE)
}

test_that("effect levels", {
  ## requested effect levels
  expect_named(epx(minnow_it,level=23),c("scenario","L.EP23"))
  expect_named(epx(minnow_it,level=c(10,20)),c("scenario","L.EP10","L.EP20"))
})

test_that("effect_tolerance", {
  ## effect.tolerance
  ## result are geting more precise if we decrease the tolerance
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
  source(test_path("class-DummyScenario.R"), local=TRUE)

  sc <- DummyScenario()

  # single scenario, single EPx level
  rs <- epx(sc, level=10, ep_only=TRUE)
  expect_equal(nrow(rs), 1)
  expect_equal(names(rs), c("L.EP10"))

  # two scenarios, a two EPx levels
  rs <- epx(c(sc, sc), level=c(10, 50), ep_only=TRUE)
  expect_equal(nrow(rs), 2)
  expect_equal(names(rs), c("L.EP10","L.EP50"))
})

test_that("long_format", {
  source(test_path("class-DummyScenario.R"), local=TRUE)

  sc <- DummyScenario() %>% set_exposure(data.frame(t=0, c=1))

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
  source(test_path("class-DummyScenario.R"), local=TRUE)

  # only a single scenario and it failed
  expect_warning(rs <- epx(DummyFails(), level=10))
  expect_equal(names(rs), c("scenario","L.EP10", "error"))
  expect_true(is.na(rs[['L.EP10']]))

  # single actual scenario, two levels, one fails
  expect_warning(rs <- epx(set_endpoints(metsulfuron,"BM"), level=c(10,50)))
  expect_equal(names(rs), c("scenario","BM.EP10", "BM.EP50", "error"))
  expect_true(is.na(rs[['BM.EP50']]))

  # two scenarios, one passed, one failed
  scen <- c(DummyScenario(), DummyFails())

  expect_warning(epx(scen) -> rs) # continue with warning
  expect_true(all(!is.na(rs[1,c("L.EP10","L.EP50")]))) # EPx present
  expect_true(is.na(rs[1,"error"])) # no error msg
  expect_true(all(is.na(rs[2,c("L.EP10","L.EP50")]))) # no EPx
  expect_true(!is.na(rs[2,"error"])) # error msg
  rm(scen,rs)

  # two scenarios, both failed
  expect_warning(rs <- epx(c(DummyFails(), DummyFails()), level=10))
  expect_equal(names(rs), c("scenario","L.EP10", "error"))
  expect_true(all(is.na(rs[1:2,2])))
  expect_true(all(!is.na(rs[1:2,3])))
})
