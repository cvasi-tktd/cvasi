
test_that("GUTS-RED sequence", {
  # continuous sim
  simulate(minnow_it, 0:4, hmax=.1, method="ode45") -> out.orig
  class(out.orig) <- c("cvasi_simulate", "data.frame")

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
  class(out.orig) <- c("cvasi_simulate", "data.frame")

  suppressWarnings(simulate(sequence(list(lemna)), method="lsoda") -> out.ss)
  expect_equal(out.ss, out.orig)

  ##
  ## transfer occurs between sequences
  list(lemna %>% set_times(0:7),
       lemna %>% set_times(7:14)) -> seq
  simulate(sequence(seq), method="lsoda") -> out.ss
  expect_equal(out.ss, out.orig)

  ##
  ## deactivate the effect of any residues in the 2nd scenario
  lemna %>%
    set_noexposure() %>%
    set_times(0:7) %>%
    simulate(method="lsoda") -> out.noex

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

test_that("output times", {
  # break included in output times
  sc <- minnow_it %>% set_times(0:6)
  sq <- sequence(list(sc, sc), breaks=3)
  rs1 <- simulate(sq)
  expect_equal(rs1$time, 0:6)

  # break included multiple times
  tms <- c(0:3, 3, 4:6)
  sc <- minnow_it %>% set_times(tms)
  sq <- sequence(list(sc, sc), breaks=3)
  rs1 <- simulate(sq)
  expect_equal(rs1$time, tms)

  # break not included
  sq <- sequence(list(sc, sc), breaks=3.1)
  rs2 <- simulate(sq)
  expect_equal(rs2$time, tms)
  expect_equal(rs2, rs1, tolerance=1e-5, ignore_attr=TRUE)
})

test_that("included outputs", {
  # single break
  sc1 <- minnow_it %>% set_times(0:3)
  sc2 <- minnow_it %>% set_times(3:6)
  sq <- sequence(list(sc1, sc2))
  rs <- simulate(sq)
  expect_equal(rs$time, 0:6)

  sq@inc_start <- c(TRUE, FALSE)
  sq@inc_end <- c(FALSE, FALSE)
  rs <- simulate(sq)
  expect_equal(rs$time, c(0, 1, 2, 4, 5))


  sq@inc_start <- c(TRUE, TRUE)
  sq@inc_end <- c(FALSE, FALSE)
  rs <- simulate(sq)
  expect_equal(rs$time, c(0, 1, 2, 3, 4, 5))

  sq@inc_start <- c(FALSE, FALSE)
  sq@inc_end <-  c(TRUE, TRUE)
  rs <- simulate(sq)
  expect_equal(rs$time, c(1, 2, 3, 4, 5, 6))

  # multiple breaks
  sc <- minnow_it %>% set_times(0:6)
  sq <- sequence(list(sc, sc, sc), breaks=c(2.1, 4))
  expect_equal(sq@inc_start, c(TRUE, FALSE, FALSE))
  expect_equal(sq@inc_end, c(FALSE, TRUE, TRUE))
  rs <- simulate(sq)
  expect_equal(rs$time, 0:6)

  sq@inc_start <- c(TRUE, TRUE, TRUE)
  rs <- simulate(sq)
  expect_equal(rs$time, c(0, 1, 2, 2.1, 3, 4, 4, 5, 6))

  # 1st scenario without output
  sc <- minnow_it %>% set_times(0:6)
  sq <- sequence(list(sc, sc), breaks=3) %>% set_times(3:6)
  rs <- simulate(sq)
  expect_equal(rs$time, 3:6)

  # 2ns scenario without output
  sc <- minnow_it %>% set_times(0:6)
  sq <- sequence(list(sc, sc), breaks=3) %>% set_times(0:3)
  rs <- simulate(sq)
  expect_equal(rs$time, 0:3)

  sq <- sq %>% set_times(0:2)
  rs <- simulate(sq)
  expect_equal(rs$time, 0:2)
})
