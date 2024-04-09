
test_that("GUTS-RED sequence", {
  # continuous sim
  simulate(minnow_it, 0:4, hmax=.1, method="ode45") -> out.orig

  # sequential sim
  list(minnow_it %>% set_times(c(0,1,1.5)),
       minnow_it %>% set_times(c(1.5,2:4))) -> seq
  simulate(sequence(seq),hmax=0.1,method="ode45") -> out.ss
  # remove intermediate step at t=1.5
  out.ss <- out.ss[-3,]

  # results should be exactly the same when using a Runge-Kutta scheme
  expect_equal(out.ss[,1], out.orig[,1])
  expect_equal(out.ss[,2], out.orig[,2])
  expect_equal(out.ss[,3], out.orig[,3])
})

test_that("Lemna sequence", {
  tol=1e-5

  ##
  ## transfer occurs during simulation of a scenario
  metsulfuron %>%
    set_exposure(data.frame(t=0:14,c=0)) %>%
    set_transfer(interval=7, biomass=50) -> lemna
  simulate(lemna, method="lsoda") -> out.orig

  simulate(sequence(list(lemna)), method="lsoda") -> out.ss
  expect_equal(out.ss, out.orig)

  ##
  ## transfer occurs between sequences
  list(lemna %>% set_times(0:7),
       lemna %>% set_times(7:14)) -> seq
  simulate(sequence(seq), method="lsoda") -> out.ss
  expect_equal(out.ss, out.orig)

  ##
  ## deactivate the effect of any residues in the 2nd scenario
  set_exposure(lemna, no_exposure()) %>% simulate(times=0:7, method="lsoda") -> out.noex

  list(lemna %>% set_times(0:7),
       lemna %>% set_param(c(Emax=0)) %>% set_times(7:14)) -> seq
  simulate(sequence(seq), method="lsoda") -> out.ss

  # 1st scenario should give the same results as before
  expect_equal(out.ss[0:7,], out.orig[0:7,])
  # 2nd scenario should show uninhibited growth
  expect_equal(out.ss[9:15,"BM"], out.noex[2:8,"BM"], tolerance=tol)
  expect_equal(out.ss[9:15,"E"], out.noex[2:8,"E"], tolerance=tol)
  expect_equal(out.ss[9:15,"FrondNo"], out.noex[2:8,"FrondNo"], tolerance=tol)
  # but internal mass should still behave the same as before
  expect_equal(out.ss[9:15,"M_int"], out.orig[9:15,"M_int"], tolerance=tol)

  ## transfer in irregular intervals
  metsulfuron %>% set_transfer(times=c(5,10,12), biomass=50) -> lemna
  simulate(lemna, method="lsoda") -> out.orig

  # transfers during each sequence
  list(lemna %>% set_transfer(times=5) %>% set_times(0:7),
       lemna %>% set_transfer(times=c(10,12)) %>% set_times(7:14)) -> seq
  simulate(sequence(seq), method="lsoda") -> out.ss
  expect_equal(out.ss, out.orig, tolerance=tol, ignore_attr=TRUE)

  # transfer occurring at end of a sequence
  list(lemna %>% set_transfer(times=5) %>% set_times(0:5),
       lemna %>% set_transfer(times=c(10,12)) %>% set_times(5:14)) -> seq
  simulate(sequence(seq), method="lsoda") -> out.ss
  expect_equal(out.ss, out.orig, tolerance=tol, ignore_attr=TRUE)

})
