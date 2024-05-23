test_that("regular intervals", {
  metsulfuron %>%
    set_init(c(BM=1)) %>%
    set_noexposure() %>%
    set_transfer(interval=2, biomass=1) %>%
    simulate() -> rs

  expect_equal(rs[,-1], rs[c(1:3, rep(2:3, 6)),-1], tolerance=1e-6, ignore_attr=T)
})

test_that("custom time points", {
  metsulfuron %>%
    set_init(c(BM=1)) %>%
    set_noexposure() %>%
    set_transfer(times=c(2,4,6,8,10,12), biomass=1) %>%
    simulate() -> rs

  expect_equal(rs[,-1], rs[c(1:3, rep(2:3, 6)),-1], tolerance=1e-6, ignore_attr=T)
})

test_that("custom biomass", {
  metsulfuron %>%
    set_init(c(BM=1)) %>%
    set_noexposure() %>%
    set_transfer(times=c(2,4,6,8,10,12), biomass=c(1,2,1,2,1,2)) %>%
    simulate() -> rs

  expect_equal(rs[1:3,-1], rs[1:3,-1], tolerance=1e-6, ignore_attr=T)
  expect_equal(rs[2:3,-1], rs[4:5,-1], tolerance=1e-6, ignore_attr=T)
  expect_equal(rs[2:3,-1], rs[8:9,-1], tolerance=1e-6, ignore_attr=T)
  expect_equal(rs[2:3,-1], rs[12:13,-1], tolerance=1e-6, ignore_attr=T)
  expect_equal(rs[6:7,-1], rs[10:11,-1], tolerance=1e-6, ignore_attr=T)
  expect_gt(rs$BM[6], rs$BM[4]+1)
  expect_gt(rs$BM[7], rs$BM[5]+1)
})

test_that("non-standard biomass compartment name", {
  sc <- metsulfuron
  sc@transfer.comp.biomass <- "Foo"
  sc@init <- c(Foo=1, E=1, M_int=0)
  sc %>%
    set_transfer(times=c(2,3,6), biomass=c(1,1,1)) %>%
    simulate() -> rs

  expect_contains(names(rs), "Foo")
})

#test_that("multiple compartments", {
#
#})
