
test_that("caching of control simulations", {
  ## scenarios not needing control sims
  minnow_it %>% cache_controls() -> scc
  expect_equal(scc@control, list(c(window.start=0,window.end=4)))

  ## scenarios requiring control sims

  # non-constant forcings
  sc <- metsulfuron %>%
    set_window(length=7,interval=1) %>%
    set_exposure(data.frame(t=0:14, c=c(rep(1,9),rep(0,6))))
  expect_equal(sc@control, NULL) # should be empty when we start

  # calculate control simulations
  scc <- cache_controls(sc)
  # number of controls
  expect_equal(length(scc@control), 8)
  # controls should all be equal
  expect_equal(dplyr::bind_rows(scc@control)$BM, rep(scc@control[[1]]["BM"],8), tolerance=0.001, ignore_attr=TRUE)
  expect_equal(dplyr::bind_rows(scc@control)$r, rep(scc@control[[1]]["r"],8), tolerance=0.001, ignore_attr=TRUE)
  # check if control value matches with manual result
  scm <- sc %>%  set_exposure(data.frame(t=0:7, c=0))
  scm@control.req <- FALSE
  efx <- effect(scm)

  expect_equal(scc@control[[1]]["BM"], efx$BM[1], tolerance=0.001, ignore_attr=TRUE)
  expect_equal(scc@control[[1]]["r"], efx$r[1], tolerance=0.001, ignore_attr=TRUE)

  # constant exposure
  scc <- metsulfuron %>%
    set_window(length=7,interval=1) %>%
    set_exposure(data.frame(t=0, c=1)) %>%
    set_times(0:14) %>%
    cache_controls()

  # controls should all be identical
  expect_equal(length(scc@control), 8)
  expect_equal(dplyr::bind_rows(scc@control)$BM, rep(scc@control[[1]]["BM"],8),ignore_attr=TRUE)
  expect_equal(dplyr::bind_rows(scc@control)$r, rep(scc@control[[1]]["r"],8), ignore_attr=TRUE)
})


