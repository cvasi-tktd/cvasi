
# helper function
wins2df <- function(scenario, skip) {
  as.data.frame(dplyr::bind_rows(window_candidates(scenario, skipZeroExposure=skip)))
}

test_that("window_candidates", {
  Lemna_Schmitt() %>%
    set_times(0:10) %>%
    set_window(length=3,interval=2) -> sc

  # integer length and interval
  win <- dplyr::bind_rows(window_candidates(sc))
  expect_equal(win, data.frame(window.start=c(0,2,4,6), window.end=c(3,5,7,9)), ignore_attr=TRUE)

  # non-integer length
  win <- dplyr::bind_rows(window_candidates(sc %>% set_window(3.1,2)))
  expect_equal(win, data.frame(window.start=c(0,2,4,6), window.end=c(3.1,5.1,7.1,9.1)), ignore_attr=TRUE)

  # non-integer interval
  win <- dplyr::bind_rows(window_candidates(sc %>% set_window(3,2.1)))
  expect_equal(win, data.frame(window.start=c(0,2.1,4.2,6.3), window.end=c(3,5.1,7.2,9.3)), ignore_attr=TRUE)

  # skip periods without exposure
  Lemna_Schmitt() %>%
    set_exposure(data.frame(t=0:5,c=c(0,0,0,1,1,0))) %>%
    set_window(length=1, interval=1) -> sc

  win <- dplyr::bind_rows(window_candidates(sc, skipZeroExposure=TRUE))
  expect_equal(win, data.frame(window.start=c(2,3,4), window.end=c(3,4,5)), ignore_attr=TRUE)

  ## scenario without moving windows
  Lemna_Schmitt() %>%
    set_window(length=-1) %>%
    set_exposure(data.frame(t=0:20,c=1:21)) -> sc
  expect_equal(window_candidates(sc,skipZeroExposure=FALSE),
               list(c(window.start=0,window.end=20)))
  expect_equal(window_candidates(sc,skipZeroExposure=TRUE),
               list(c(window.start=0,window.end=20)))

  ## scenarios with moving windows
  base <- set_window(Lemna_Schmitt(), length=5, interval=1)

  # constant zero exposure
  set_exposure(base, data.frame(t=0:20,c=0)) -> sc
  expect_equal(wins2df(sc,FALSE),data.frame(window.start=0:15,window.end=5:20))
  expect_equal(wins2df(sc,TRUE),data.frame(window.start=0,window.end=5))

  # constant non-zero exposure
  set_exposure(base, data.frame(t=0:20,c=1)) -> sc
  expect_equal(wins2df(sc,FALSE),data.frame(window.start=0:15,window.end=5:20))
  expect_equal(wins2df(sc,TRUE),data.frame(window.start=0:15,window.end=5:20))

  # non-zero exposure at the beginning
  set_exposure(base, data.frame(t=0:20,c=c(rep(1,10),rep(0,11)))) -> sc
  expect_equal(wins2df(sc,FALSE),data.frame(window.start=0:15,window.end=5:20))
  expect_equal(wins2df(sc,TRUE),data.frame(window.start=0:9,window.end=5:14))

  # non-zero exposure in the middle
  set_exposure(base, data.frame(t=0:20,c=c(rep(0,8),rep(1,5),rep(0,8)))) -> sc
  expect_equal(wins2df(sc,FALSE),data.frame(window.start=0:15,window.end=5:20))
  expect_equal(wins2df(sc,TRUE),data.frame(window.start=3:12,window.end=8:17))

  # non-zero exposure at the end
  set_exposure(base, data.frame(t=0:20,c=c(rep(0,11),rep(1,10)))) -> sc
  expect_equal(wins2df(sc,FALSE),data.frame(window.start=0:15,window.end=5:20))
  expect_equal(wins2df(sc,TRUE),data.frame(window.start=6:15,window.end=11:20))

})

## todo move tests covering moving windows here?

