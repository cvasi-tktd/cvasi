effect_IT <- function(df) {
  minnow_it %>% set_exposure(df) %>% effect(ep_only=TRUE, rtol=0) %>% unname()
}

effect_SD <- function(df) {
  minnow_sd %>% set_exposure(df) %>% effect(ep_only=TRUE, rtol=0, hmax=0.01) %>% unname()
}

window_IT <- function(df,window=-1,interval=-1) {
  mod <- minnow_it
  if(window > 0)
    mod <- set_window(mod, window, interval)
  mod %>%
    set_exposure(df) %>%
    effect(max_only=FALSE, hmax=0.001, method="ode45") %>%
    dplyr::mutate(L=round(L, 5)) %>% # avoid marginal differences due to numerical errors
    dplyr::arrange(dplyr::desc(L)) %>%
    dplyr::slice(1) -> tib
  c(tib[[1,"dat.start"]],tib[[1,"dat.end"]])
}

test_that("return types", {
  sc <- minnow_it

  expect_true(is.numeric(effect(sc, max_only=TRUE, ep_only=TRUE)))
  expect_true(tibble::is_tibble(effect(sc, max_only=TRUE, ep_only=FALSE)))
  expect_true(tibble::is_tibble(effect(sc, max_only=FALSE, ep_only=TRUE)))
  expect_true(tibble::is_tibble(effect(sc, max_only=FALSE, ep_only=FALSE)))
})

test_that("factor argument", {
  sc <- minnow_it
  sc0 <- sc %>% set_noexposure()
  sc10 <- sc %>% set_exposure(data.frame(t=0, c=10), reset_times=FALSE)
  sc20 <- sc %>% set_exposure(data.frame(t=0, c=20), reset_times=FALSE)

  expect_equal(effect(sc, factor=0, ep_only=TRUE), effect(sc0, ep_only=TRUE))
  expect_equal(effect(sc, factor=10, ep_only=TRUE), effect(sc10, ep_only=TRUE))
  expect_equal(effect(sc, factor=20, ep_only=TRUE), effect(sc20, ep_only=TRUE))
})

test_that("generic effect calculation", {
  # no effect
  expect_equal(calc_effect(1, 1), 0)
  expect_equal(calc_effect(0, 0), 0)
  expect_equal(calc_effect(-1, -1), 0)
  # positive finite effects, i.e. treatment is lower than control
  expect_equal(calc_effect(0.9, 1), 0.1)
  expect_equal(calc_effect(0,  1), 1)
  expect_equal(calc_effect(-1, 1), 2)

  expect_equal(calc_effect(-1.1, -1), 0.1)
  expect_equal(calc_effect(-2,   -1), 1)
  expect_equal(calc_effect(-3,   -1), 2)
  # negative finite effects, i.e. treatment is higher than control
  expect_equal(calc_effect(1.1, 1), -0.1)
  expect_equal(calc_effect(2,   1), -1)
  expect_equal(calc_effect(3,   1), -2)

  expect_equal(calc_effect(-0.9, -1), -0.1)
  expect_equal(calc_effect( 0,   -1), -1)
  expect_equal(calc_effect( 1,   -1), -2)
  # infinite effects, i.e. control is zero
  expect_equal(calc_effect(0.1,   0), -Inf)
  expect_equal(calc_effect(-0.1,  0), Inf)
})

test_that("GUTS-RED pulsed exposure", {
  # allowed numerical tolerance between model runs
  tol <- 0.001
  # define some exposure patterns
  conc <- 10
  constant <- data.frame(t=c(1,10),c=c(conc))
  pulse_start <- data.frame(t=c(1,10,10.001,20),c=c(conc,conc,0,0))
  pulse_mid <- data.frame(t=c(1,4.999,5,14,14.001,20),c=c(0,0,conc,conc,0,0))
  pulse_end <- data.frame(t=c(1,10.999,11,20),c=c(0,0,conc,conc))

  # IT model
  expect_equal(effect_IT(constant), 0.578722, tolerance=tol)
  expect_equal(effect_IT(constant), effect_IT(pulse_start), tolerance=tol)
  expect_equal(effect_IT(constant), effect_IT(pulse_mid), tolerance=tol)
  expect_equal(effect_IT(constant), effect_IT(pulse_end), tolerance=tol)
  # SD model
  expect_equal(effect_SD(constant), 0.9376296, tolerance=tol)
  expect_equal(effect_SD(constant), effect_SD(pulse_start), tolerance=tol)
  expect_equal(effect_SD(constant), effect_SD(pulse_mid), tolerance=tol)
  expect_equal(effect_SD(constant), effect_SD(pulse_end), tolerance=tol)
})

test_that("GUTS-RED background mortality", {
  # hb>0 should not affect effect levels
  expect_error(effect(minnow_it %>% set_param(c(hb=1)), minnow_it))
  expect_error(effect(minnow_sd %>% set_param(c(hb=1)), minnow_sd))
})

#test if function correctly finds maximum effect within a time-series
test_that("window finding GUTS", {
  # define some exposure patterns
  conc <- 10
  constant <- data.frame(t=1:10,c=c(conc))
  pulse_start <- data.frame(t=1:20,c=c(rep(conc,5),rep(0,15)))
  pulse_mid <- data.frame(t=1:20,c=c(rep(0,5),rep(conc,5),rep(0,10)))
  pulse_end <- data.frame(t=1:20,c=c(rep(0,15),rep(conc,5)))

  # full range of profile
  expect_equal(window_IT(constant), c(1,10))
  expect_equal(window_IT(pulse_start), c(1,20))
  # should return first window of multiple candidates of equal effect
  expect_equal(window_IT(constant,window=5,interval=1), c(1,6))
  # identification of max effect window, full exposure
  expect_equal(window_IT(pulse_start,window=5,interval=1), c(1,6))
  expect_equal(window_IT(pulse_mid,window=5,interval=1), c(5,10))
  expect_equal(window_IT(pulse_end,window=5,interval=1), c(15,20))
  # partial exposure
  expect_equal(window_IT(pulse_start,window=10,interval=1), c(1,11))
  expect_equal(window_IT(pulse_mid,window=10,interval=1), c(1,11))
  expect_equal(window_IT(pulse_end,window=10,interval=1), c(10,20))
})

test_that("window finding DEB", {
  tol = 0.1 # relative error of 10%, large tolerance because DEBtool results are uncertain
  ## results for Americamysis bahia
  # no exposure
  expect_equal(effect(set_exposure(americamysis,data.frame(t=1:28,c=0)),ep_only=TRUE),
               c(L=0,R=0),
               tolerance=tol)
  # medium exposure
  expect_equal(effect(americamysis,ep_only=TRUE),
               c(L=1-.89151/1.0041,R=1-5.0677/9.4513),
               tolerance=tol)
  # high exposure
  expect_equal(effect(set_exposure(americamysis,data.frame(t=1:28,c=3.46)),ep_only=TRUE),
               c(L=1-0.60046/1.0041,R=1),
               tolerance=tol)
  # effects for all windows
  # provide controls as argument

})

test_that("window finding Lemna", {
  metsulfuron %>% set_window(5,1) -> lemna
  conc <- 1
  pulse_start <- data.frame(t=1:20,c=c(rep(conc,5),rep(0,15)))
  pulse_mid <- data.frame(t=1:20,c=c(rep(0,5),rep(conc,5),rep(0,10)))
  pulse_end <- data.frame(t=1:20,c=c(rep(0,14),rep(conc,5),0))

  effect(set_exposure(lemna,pulse_start)) -> estr
  effect(set_exposure(lemna,pulse_mid)) -> emid
  effect(set_exposure(lemna,pulse_end)) -> eend

  # time of peak effect
  expect_equal(estr$BM.dat.start, 1)
  expect_equal(estr$BM.dat.end, 6)
  expect_equal(emid$BM.dat.start, 6)
  expect_equal(emid$BM.dat.end, 11)
  expect_equal(eend$BM.dat.start, 15)
  expect_equal(eend$BM.dat.end, 20)

  # absolute value of effect
  expect_gt(estr$BM, 0.19)
  expect_equal(estr$BM, emid$BM, tolerance=1e-5)
  expect_equal(estr$BM, eend$BM, tolerance=1e-5)
})

test_that("Lemna effects", {
  # no effects
  metsulfuron %>%
    set_exposure(data.frame(t=0:14,c=0)) -> sc
  expect_equal(sc %>% effect(ep_only=TRUE), c("BM"=0,"r"=0))

  # check that r does not exceed 1.0 in case of negative growth rates
  metsulfuron %>%
    set_exposure(data.frame(t=0:14, c=1000)) -> sc
  sc %>% simulate() -> out
  # check that we indeed have negative growth
  expect_true(out$BM[1] > tail(out$BM,1))
  # r effect endpoint should be greater than 1.0
  sc %>% effect() -> efx
  expect_gt(efx$r[1], 1)
  # all endpoints contained in output?
  expect_true(all(c("BM","r") %in% names(efx)))
})

test_that("Myriophyllum", {
  Myrio() %>%
    set_init(c(BM=1)) %>%
    set_exposure(metsulfuron@exposure@series) %>%
    set_param(c(EC50_int=0.5,b=0.2,P=0.01,r_DW_TSL=1)) %>%
    set_endpoints(c("BM","r")) %>%
    effect() -> efx
  # all endpoints contained in output?
  expect_true(all(c("BM","r") %in% names(efx)))
})

test_that("general arguments", {
  metsulfuron %>% set_window(7,1) -> lemna

  # results for all relevant windows
  effect(lemna, max_only=FALSE, hmax=1, method="ode45") -> ep
  expect_equal(ep$dat.start,    0:7)
  expect_equal(ep$dat.end,      7:14)
  expect_true(all(ep$BM[1:7] > 0))
  expect_true(ep$BM[8] == 0)

  effect(lemna, hmax=1) -> epmax
  expect_equal(epmax$BM[1], max(ep$BM), tolerance=1e-5)

  # factor applied to exposure series
  factor <- 3.21
  lemna2 <- lemna
  lemna2@exposure@series[,2] <- lemna2@exposure@series[,2]*factor
  expect_equal(effect(lemna, factor=factor), effect(lemna2), tolerance=1e-5)
})

test_that("marginal effects", {
  # this DEB scenario creates a negative effect in one exposure window due
  # to instable numerics
  americamysis %>%
    set_window(7) %>%
    set_exposure(data.frame(t=c(0,3,4,7,8), c=c(0,0,3,3,0))) %>%
    set_times(0:14) -> deb

  # make sure a negative effect level exists
  efx1 <- dplyr::pull(effect(deb, max_only=FALSE), "L")
  expect_lt(min(efx1), 0)
  expect_gt(min(efx1), -0.01)
  # test marginal effect threshold
  efx2 <- dplyr::pull(effect(deb, max_only=FALSE, marginal_effect=1e-5), "L")
  expect_gte(min(efx2), 0)
  expect_lte(max(abs(efx1 - efx2)), 1e-5)

  # questionable inputs
  expect_warning(effect(minnow_it, marginal_effect=0.1))

})

test_that("invalid arguments", {
  ## invalid inputs
  # multiple scenarios
  expect_error(effect(c(americamysis, americamysis)))
  expect_error(effect(list(minnow_it, minnow_sd)))
})
