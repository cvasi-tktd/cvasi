
#
# Lemna simulation without transfers to new medium.
# Reference values were calculated using the original model of Schmitt et al. (1993)
# doi.org/10.1016/j.ecolmodel.2013.01.017
#
test_that("Lemna_Schmitt simulation", {
  tol <- 1e-5
  # no exposure
  metsulfuron %>%
    set_exposure(no_exposure(), FALSE) %>%
    set_times(0:14) %>%
    simulate(method="lsoda", hmax=0.1) -> out

  expect_equal(out[,"time"], 0:14)

  expect_equal(out[1,"BM"], 50)
  expect_equal(out[8,"BM"], 70.52071, tolerance=tol)
  expect_equal(out[15,"BM"], 91.13051, tolerance=tol)

  expect_equal(out[,"E"], rep(1,15))
  expect_equal(out[,"M_int"], rep(0,15))
  expect_equal(out[,"C_int"], rep(0,15))

  expect_equal(out[1,"FrondNo"], 500000)
  expect_equal(out[8,"FrondNo"], 705207.1, tolerance=tol)
  expect_equal(out[15,"FrondNo"], 911305.1, tolerance=tol)

  # constant exposure
  metsulfuron %>%
    set_exposure(data.frame(t=0, c=1), FALSE) %>%
    set_times(0:14) %>%
    simulate(method="lsoda", hmax=0.1) -> out

  expect_equal(out[,"time"], 0:14)

  expect_equal(out[1,"BM"], 50)
  expect_equal(out[8,"BM"], 51.62533, tolerance=tol)
  expect_equal(out[15,"BM"], 50.32514, tolerance=tol)

  expect_equal(out[,"E"], rep(1,15))

  # M_int not reported by original model, cannot test
  # expect_equal(out[,"M_int"], NA)

  expect_equal(out[1,"C_int"], 0)
  expect_equal(out[8,"C_int"], 0.6898937, tolerance=tol)
  expect_equal(out[15,"C_int"], 0.7206542, tolerance=tol)

  expect_equal(out[1,"FrondNo"], 500000)
  expect_equal(out[8,"FrondNo"], 516253.3, tolerance=tol)
  expect_equal(out[15,"FrondNo"], 503251.4, tolerance=tol)

  # step function exposure
  metsulfuron %>%
    set_exposure(data.frame(t=0:14,c=c(rep(1,7),rep(0,8)))) %>%
    simulate(method="lsoda", hmax=0.1) -> out

  expect_equal(out[,"time"], 0:14)

  expect_equal(out[1,"BM"], 50)
  expect_equal(out[8,"BM"], 51.63740, tolerance=tol)
  expect_equal(out[15,"BM"], 65.95882, tolerance=tol)

  expect_equal(out[,"E"], rep(1,15))

  # M_int not reported by original model, cannot test
  # expect_equal(out[,"M_int"], NA)

  expect_equal(out[1,"C_int"], 0)
  expect_equal(out[8,"C_int"], 0.54974771, tolerance=tol)
  expect_equal(out[15,"C_int"], 0.01825732, tolerance=tol)

  expect_equal(out[1,"FrondNo"], 500000)
  expect_equal(out[8,"FrondNo"], 516374.0, tolerance=tol)
  expect_equal(out[15,"FrondNo"], 659588.2, tolerance=tol)
})

test_that("Lemna_SchmittThold simulation", {
  sc <- Lemna_SchmittThold() %>% set_all(metsulfuron)
  sc@init <- c(sc@init, AUC=0)

  out_orig <- simulate(metsulfuron, method="lsoda")

  # no threshold set, no influence on results
  expect_equal(simulate(sc, method="lsoda")[-5], out_orig)

  # results should be identical for very large, i.e. irrelevant, thresholds
  sc %>% set_param(c(threshold=1e10)) %>% simulate(method="lsoda") -> out
  expect_equal(out[-5], out_orig)

  # plausibility of threshold dynamics
  sc %>% set_param(c(threshold=2)) %>% simulate(method="lsoda") -> out
  expect_equal(out$AUC, c(0:6, rep(6.5, 8)), tolerance=1e-5) # AUC values
  expect_equal(out[1:3,][-5], out_orig[1:3,], tolerance=1e-5) # unaffected growth until threshold
  expect_true(all(diff(out$BM[-c(1,2)]) < 0)) # BM declines after threshold is exceeded

  # compare to values from Schmitt et al. implementation
  # the results are not extremely accurate because trapz(), which is used in
  # the Schmitt et al. model for AUC calculation, deviates visibly from exact results
  expect_equal(out[3,"BM"], 52.43495, tolerance=1e-3) # before threshold
  expect_equal(out[4,"BM"], 51.42340, tolerance=1e-3) # after threshold
  expect_equal(out[14,"BM"], 41.97315, tolerance=1e-3)

  # error on missing AUC state variable
  expect_error(Lemna_SchmittThold() %>% set_all(metsulfuron) %>% set_param(c(threshold=1)) %>% simulate(),
               regexp="AUC state variable")
})

#
# Lemna simulation without transfers to new medium.
# Reference values were calculated using the original model of Schmitt et al. (1993)
# doi.org/10.1016/j.ecolmodel.2013.01.017
#
test_that("Lemna simulation, regular transfers", {
  tol <- 1e-4
  ## no exposure, no transfer
  metsulfuron %>%
    set_init(c(BM=0.0012,E=1,M_int=0)) %>%
    set_exposure(no_exposure(), FALSE) %>%
    set_transfer(interval=7) %>%
    set_times(0:7) %>%
    simulate(method="lsoda", hmax=0.1) -> out

  expect_equal(out[,"time"], 0:7)

  expect_equal(out[1,"BM"], 0.0012, tolerance=tol)
  expect_equal(out[8,"BM"], 0.002177663, tolerance=tol)

  expect_equal(out[,"E"], rep(1,8))
  expect_equal(out[,"M_int"], rep(0,8))
  expect_equal(out[,"C_int"], rep(0,8))

  expect_equal(out[1,"FrondNo"], 12)
  expect_equal(out[8,"FrondNo"], 21.77663, tolerance=tol)

  ## no exposure, one transfer
  metsulfuron %>%
    set_init(c(BM=0.0012,E=1,M_int=0)) %>%
    set_exposure(no_exposure(), FALSE) %>%
    set_transfer(interval=7) %>%
    set_times(0:14) %>%
    simulate(method="lsoda", hmax=0.1) -> out

  expect_equal(out[,"time"], 0:14)

  expect_equal(out[1,"BM"], 0.0012, tolerance=tol)
  expect_equal(out[8,"BM"], 0.002177663, tolerance=tol)
  expect_equal(out[9:15,"BM"], out[2:8,"BM"], tolerance=tol)

  expect_equal(out[,"E"], rep(1,15))
  expect_equal(out[,"M_int"], rep(0,15))
  expect_equal(out[,"C_int"], rep(0,15))

  expect_equal(out[1,"FrondNo"], 12)
  expect_equal(out[8,"FrondNo"], 21.77663, tolerance=tol)
  expect_equal(out[9:15,"FrondNo"], out[2:8,"FrondNo"], tolerance=tol)

  ## no exposure, multiple transfers
  metsulfuron %>%
    set_init(c(BM=0.0012,E=1,M_int=0)) %>%
    set_noexposure() %>%
    set_transfer(interval=7) %>%
    set_times(0:28) %>%
    simulate(method="lsoda", hmax=0.1) -> out

  expect_equal(out[,"time"], 0:28)

  expect_equal(out[1,"BM"], 0.0012, tolerance=tol)
  expect_equal(out[8,"BM"], 0.002177663, tolerance=tol)
  expect_equal(out[9:15,"BM"], out[2:8,"BM"], tolerance=tol)
  expect_equal(out[9:15,"BM"], out[2:8,"BM"], tolerance=tol)
  expect_equal(out[16:22,"BM"], out[2:8,"BM"], tolerance=tol)
  expect_equal(out[23:29,"BM"], out[2:8,"BM"], tolerance=tol)

  expect_equal(out[,"E"], rep(1,29))
  expect_equal(out[,"M_int"], rep(0,29))
  expect_equal(out[,"C_int"], rep(0,29))

  expect_equal(out[1,"FrondNo"], 12)
  expect_equal(out[8,"FrondNo"], 21.77663, tolerance=tol)
  expect_equal(out[9:15,"FrondNo"], out[2:8,"FrondNo"], tolerance=tol)
  expect_equal(out[16:22,"FrondNo"], out[2:8,"FrondNo"], tolerance=tol)
  expect_equal(out[23:29,"FrondNo"], out[2:8,"FrondNo"], tolerance=tol)

  ## no exposure, irregular simulation periods with transfer
  # sim stops before transfer
  metsulfuron %>%
    set_init(c(BM=0.0012,E=1,M_int=0)) %>%
    set_noexposure() %>%
    set_transfer(interval=7) %>%
    set_times(0:6) %>%
    simulate(method="lsoda", hmax=0.1) -> out

  expect_equal(out[,"time"], 0:6)
  expect_equal(out[1,"FrondNo"], 12)
  expect_equal(out[2,"FrondNo"], 13.06635, tolerance=tol)
  expect_equal(out[7,"FrondNo"], 19.99945, tolerance=tol)

  # sim entails 1st transfer
  metsulfuron %>%
    set_init(c(BM=0.001836730,E=1,M_int=0)) %>%
    set_noexposure() %>%
    set_transfer(interval=7) %>%
    set_times(5:10) %>%
    simulate(method="lsoda", hmax=0.1) -> out

  expect_equal(out[,"time"], 5:10)
  expect_equal(out[1,"FrondNo"], 18.36730, tolerance=tol) # t=5
  expect_equal(out[2,"FrondNo"], 19.99945, tolerance=tol) # t=6
  expect_equal(out[3,"FrondNo"], 21.77663, tolerance=tol) # t=7
  expect_equal(out[4,"FrondNo"], 13.06635, tolerance=tol) # t=8, frond number reset
  expect_equal(out[5,"FrondNo"], 14.22745, tolerance=tol) # t=9
  expect_equal(out[6,"FrondNo"], 15.49173, tolerance=tol) # t=10

  # sim starts and ends in 2nd interval, no transfer occurring
  metsulfuron %>%
    set_init(c(BM=0.0012,E=1,M_int=0)) %>%
    set_noexposure() %>%
    set_transfer(interval=7) %>%
    set_times(7:10) %>%
    simulate(method="lsoda", hmax=0.1) -> out

  expect_equal(out[,"time"], 7:10)
  expect_equal(out[1,"FrondNo"], 12)
  expect_equal(out[2,"FrondNo"], 13.06635, tolerance=tol)
  expect_equal(out[4,"FrondNo"], 15.49173, tolerance=tol)

  ## constant exposure, no transfer
  metsulfuron %>%
    set_init(c(BM=0.0012, E=1, M_int=0)) %>%
    set_exposure(data.frame(t=0, c=1), FALSE) %>%
    set_transfer(interval=7) %>%
    set_times(0:7) %>%
    simulate(method="lsoda", hmax=0.1) -> out

  expect_equal(out[,"time"], 0:7)
  expect_equal(out[1,"BM"], 0.0012)
  expect_equal(out[8,"BM"], 0.001334016, tolerance=tol)
  expect_equal(out[,"E"], rep(1,8))
  expect_equal(out[1,"M_int"], 0)
  expect_equal(out[1,"C_int"], 0)
  expect_equal(out[8,"C_int"], 0.6804143, tolerance=tol)
  expect_equal(out[1,"FrondNo"], 12)
  expect_equal(out[8,"FrondNo"], 13.34016, tolerance=tol)

  ## constant exposure, one transfer
  metsulfuron %>%
    set_init(c(BM=0.0012, E=1, M_int=0)) %>%
    set_exposure(data.frame(t=0, c=1), FALSE) %>%
    set_transfer(interval=7,biomass=0.0012) %>%
    set_times(0:14) %>%
    simulate(method="lsoda", hmax=0.1) -> out

  expect_equal(out[,"time"], 0:14)
  expect_equal(out[1,"BM"], 0.0012)
  expect_equal(out[8,"BM"], 0.001334016, tolerance=tol)
  expect_equal(out[9,"BM"], 0.001203635, tolerance=3e-4)
  expect_equal(out[15,"BM"], 0.001226337, tolerance=8e-4)
  expect_equal(out[,"E"], rep(1,15))
  expect_equal(out[1,"M_int"], 0)
  expect_equal(out[1,"C_int"], 0)
  expect_equal(out[8,"C_int"], 0.6804143, tolerance=tol)
  expect_equal(out[9,"C_int"], 0.6916387, tolerance=tol)
  expect_equal(out[15,"C_int"], 0.7099914, tolerance=tol)
  expect_equal(out[1,"FrondNo"], 12)
  expect_equal(out[8,"FrondNo"], 13.34016, tolerance=1e-4)
  expect_equal(out[9,"FrondNo"], 12.03918, tolerance=tol)
  expect_equal(out[15,"FrondNo"], 12.27103, tolerance=tol)
})

test_that("Lemna simulation irregular transfers", {
  tol <- 1e-5
  ## irregular transfer times

  # vanilla use case, one transfer in the middle of the simulated period
  metsulfuron %>%
    set_init(c(BM=0.001, E=1, M_int=0)) %>%
    set_exposure(data.frame(t=0, c=0), FALSE) %>%
    set_transfer(interval=7, biomass=0.001) %>%
    set_times(0:14) -> sc.reg
  sc.reg %>% set_transfer(times=c(7)) -> sc.irg

  expect_equal(simulate(sc.irg, method="lsoda", hmax=0.1),
               simulate(sc.reg, method="lsoda", hmax=0.1))


  sc.reg %>%
    set_transfer(interval=5) %>%
    set_times(0:12) %>%
    simulate(method="lsoda", hmax=0.01) -> out.reg
  sc.reg %>%
    set_transfer(times=c(5,10,12,14,16)) %>%
    set_times(0:20) %>%
    simulate(method="lsoda", hmax=0.01) -> out.irg
  # same results until first transfer?
  expect_equal(out.irg[1:6,-1], out.reg[1:6,-1], tolerance=tol)
  # same results in 2nd interval?
  expect_equal(out.irg[6:11,-1], out.reg[6:11,-1], tolerance=tol)
  # same results at beginning of 3rd interval?
  expect_equal(out.irg[6:8,"BM"], out.reg[11:13,"BM"], ignore_attr=T, tolerance=tol)
  # same results at consecutive irregular intervals?
  expect_equal(out.irg[11:13,"BM"], out.reg[6:8,"BM"], ignore_attr=T, tolerance=tol)
  expect_equal(out.irg[14:15,"BM"], out.reg[7:8,"BM"], ignore_attr=T, tolerance=tol)
  expect_equal(out.irg[16:17,"BM"], out.reg[7:8,"BM"], ignore_attr=T, tolerance=tol)
  expect_equal(out.irg[18:21,"BM"], out.reg[7:10,"BM"], ignore_attr=T, tolerance=tol)

  # one transfer at end of period
  metsulfuron %>%
    set_init(c(BM=0.001, E=1, M_int=0)) %>%
    set_exposure(data.frame(t=0, c=0), FALSE) %>%
    set_times(0:14) %>%
    set_transfer(interval=-1) -> sc.reg
  sc.reg %>% set_transfer(times=c(14)) -> sc.irg
  expect_equal(simulate(sc.reg), simulate(sc.irg), tolerance=tol)

  # one transfer at beginning of period
  sc.reg %>% set_transfer(times=c(0)) -> sc.irg
  expect_equal(simulate(sc.reg), simulate(sc.irg), tolerance=tol)

  # no relevant transfers in period
  sc.reg %>% set_transfer(times=c(21)) -> sc.irg
  expect_equal(simulate(sc.reg), simulate(sc.irg), tolerance=tol)
})

test_that("Lemna solver", {
  # Schmitt model
  expect_equal(solver(metsulfuron)$time, metsulfuron@times)

  # Schmitt Threshold model
  Lemna_SchmittThold() %>%
    set_init(metsulfuron@init) %>%
    set_param(metsulfuron@param) %>%
    set_param(c("threshold"=2)) %>%
    set_forcings(metsulfuron@forcings) %>%
    set_exposure(metsulfuron@exposure@series) -> sc
  expect_equal(solver(sc)$time, sc@times)

  # SETAC model
  focusd1 %>% set_times(0:10) -> sc
  expect_equal(solver(sc)$time, sc@times)
})
