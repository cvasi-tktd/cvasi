test_that("set_transfer interval", {
  # no transfer
  Lemna_Schmitt() %>% set_transfer(interval=-1) -> sc
  expect_equal(sc@transfer.interval, -1)
  expect_equal(sc@transfer.times, numeric(0))

  ## transfer interval
  Lemna_Schmitt() %>% set_transfer(interval=5) -> sc
  expect_equal(sc@transfer.interval, 5)
  expect_equal(sc@transfer.times, numeric(0))

  ## transfer times
  Lemna_Schmitt() %>% set_transfer(times=seq(5)) -> sc
  expect_equal(sc@transfer.interval, -1)
  expect_equal(sc@transfer.times, seq(5))
  # auto-ordering of times
  Lemna_Schmitt() %>% set_transfer(times=c(1,5,2)) -> sc
  expect_equal(sc@transfer.times, c(1,2,5))

  # using interval and times in conjunction
  Lemna_Schmitt() %>% set_transfer(interval=-2, times=c(1)) -> sc
  expect_equal(sc@transfer.interval, -1)
  expect_equal(sc@transfer.times, c(1))
  Lemna_Schmitt() %>% set_transfer(interval=1, times=c()) -> sc
  expect_equal(sc@transfer.interval, 1)
  expect_equal(sc@transfer.times, numeric(0))

  # re-setting options
  Lemna_Schmitt() %>%
    set_transfer(interval=5) %>%
    set_transfer(times=seq(5)) -> sc
  expect_equal(sc@transfer.interval, -1)
  expect_equal(sc@transfer.times, seq(5))

  Lemna_Schmitt() %>%
    set_transfer(times=seq(5)) %>%
    set_transfer(interval=5) -> sc
  expect_equal(sc@transfer.interval, 5)
  expect_equal(sc@transfer.times, numeric(0))
})

test_that("set_transfer biomass", {
  ## biomass
  Lemna_Schmitt() %>% set_transfer(biomass=23) -> sc
  expect_equal(sc@transfer.biomass, 23)
  # one biomass amount for each transfer
  Lemna_Schmitt() %>% set_transfer(times=0:2, biomass=1:3) -> sc
  expect_equal(sc@transfer.biomass, 1:3)

  ## compartments
  Lemna_Schmitt() %>%
    set_transfer(scaled_comp=c("E","M_int")) -> sc
  expect_equal(sc@transfer.comp.scaled, c("E","M_int"))
  Lemna_Schmitt() %>%
    set_transfer(scaled_comp=c("E")) %>%
    set_transfer(scaled_comp=c("M_int")) -> sc
  expect_equal(sc@transfer.comp.scaled, c("M_int"))

  # setting all at once
  Lemna_Schmitt() %>%
    set_transfer(interval=-2, times=c(1,2), biomass=c(3,4), scaled_comp="E") -> sc
  expect_equal(sc@transfer.interval, -1)
  expect_equal(sc@transfer.times, c(1,2))
  expect_equal(sc@transfer.biomass, c(3,4))
  expect_equal(sc@transfer.comp.scaled, "E")
})

test_that("set_transfer invalid arguments", {
  # invalid inputs
  expect_error(set_transfer(sc, interval=NA))
  expect_error(set_transfer(sc, interval=2, times=2))
  expect_error(set_transfer(sc, times=c(2,NA,3)))
  expect_error(set_transfer(sc, times=1, biomass=1:3))
  expect_error(set_transfer(sc, interval=1, biomass=1:3))
})
