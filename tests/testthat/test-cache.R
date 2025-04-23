test_that("no windows", {
  sc <- new("EffectScenario") %>% set_times(1:7) %>% set_window(length=-1)
  sc@control.req <- FALSE

  env <- cache_windows(sc)
  expect_equal(env$windows, list(c("window.start"=1, "window.end"=7)))
  expect_equal(env$controls, list())
})

test_that("windows", {
  sc <- new("EffectScenario") %>% set_times(1:7) %>% set_window(length=3, interval=2)
  sc@control.req <- FALSE
  windws <- list(
    c("window.start"=1, "window.end"=4),
    c("window.start"=3, "window.end"=6)
  )

  # from scenario
  env <- cache_windows(sc)
  expect_equal(env$windows, windws)
  expect_equal(env$controls, list())

  # from sequence
  sq <- sequence(list(sc, sc), breaks=2)
  env <- cache_windows(sq)
  expect_equal(env$windows, windws)
  expect_equal(env$controls, list())
})

test_that("controls", {
  sc <- minnow_it %>%
    set_param(c(kd=1000, hb=0)) %>%
    set_times(1:7) %>%
    set_window(length=3, interval=2)
  sc@control.req <- TRUE
  scc <- sc %>% set_noexposure()
  scv <- sc %>% set_exposure(data.frame(t=0:10, c=runif(11)), reset_times=FALSE)
  windws <- list(
    c("window.start"=1, "window.end"=4),
    c("window.start"=3, "window.end"=6)
  )
  effcts <- list(c(L=0), c(L=0))

  # from scenario with constant forcings
  env <- cache_windows(scc)
  expect_equal(env$windows, windws)
  expect_equal(env$controls, effcts)

  # from scenario with variable forcings
  env <- cache_windows(scv)
  expect_equal(env$windows, windws)
  expect_equal(env$controls, effcts)

  # from sequence
  sq <- sequence(list(scv, scv), breaks=2)
  env <- cache_windows(sq)
  expect_equal(env$windows, windws)
  expect_equal(env$controls, effcts)
})

test_that("control value", {
  sc <- metsulfuron %>%
    set_times(1:7) %>%
    set_window(length=3, interval=2)
  scc <- sc %>% set_noexposure()
  scc@control.req <- FALSE

  env <- cache_windows(sc)
  expect_equal(length(env$controls), 2)

  # first window
  expect_equal(env$controls[[1]], scc %>% set_times(1:4) %>% effect(ep_only=TRUE), tolerance=1e-5)
  # second window
  expect_equal(env$controls[[2]], scc %>% set_times(3:6) %>% effect(ep_only=TRUE), tolerance=1e-5)
})

test_that("invalid arguments", {
  expect_error(cache_windows(1))
  expect_error(cache_windows(c(1, 2)))
})

test_that("window_candidates", {
  sc <- new("EffectScenario")
  sc@control.req <- FALSE

  # no moving windows
  sc <- sc %>% set_times(0:5) %>% set_nowindow()
  win <- cache_windows(sc)$windows
  expect_equal(win, list(c(window.start=0, window.end=5)))

  # scenario starting at zero
  sc <- sc %>% set_times(0:5) %>% set_window(length=1, interval=1)
  win <- dplyr::bind_rows(cache_windows(sc)$windows)
  expect_equal(win, data.frame(window.start=0:4, window.end=1:5), ignore_attr=TRUE)

  # scenario starting at non-zero
  sc <- sc %>% set_times(1:5) %>% set_window(length=1, interval=1)
  win <- dplyr::bind_rows(cache_windows(sc)$windows)
  expect_equal(win, data.frame(window.start=1:4, window.end=2:5), ignore_attr=TRUE)

  # ending not on last time point
  sc <- sc %>% set_times(1:6) %>% set_window(length=2, interval=3)
  win <- dplyr::bind_rows(cache_windows(sc)$windows)
  expect_equal(win, data.frame(window.start=c(1,4), window.end=c(3,6)), ignore_attr=TRUE)

  # window length does not conform to output time step
  sc <- sc %>% set_times(0:3) %>% set_window(length=0.5, interval=1)
  win <- dplyr::bind_rows(cache_windows(sc)$windows)
  expect_equal(win, data.frame(window.start=c(0:2), window.end=0:2 + 0.5), ignore_attr=TRUE)

  # window interval does not conform to output time step
  sc <- sc %>% set_times(0:3) %>% set_window(length=1, interval=0.5)
  win <- dplyr::bind_rows(cache_windows(sc)$windows)
  expect_equal(win, data.frame(window.start=seq(0, 2, 0.5), window.end=seq(1, 3, 0.5)), ignore_attr=TRUE)

  # window longer than scenario times
  sc <- sc %>% set_times(0:3) %>% set_window(length=5, interval=1)
  win <- cache_windows(sc)$windows
  expect_equal(win, list(c(window.start=0, window.end=5)))

  # no output times
  sc@times <- numeric(0)
  expect_error(cache_windows(sc))
})
