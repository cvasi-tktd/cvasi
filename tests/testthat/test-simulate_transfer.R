test_that("regular intervals, starting and ending on transfer", {
  sc <- metsulfuron %>%
    set_init(c(BM=1)) %>%
    set_times(0:10) %>%
    set_noexposure() %>%
    set_transfer(interval=5, biomass=1)

  ref <- sc %>%
    set_notransfer() %>%
    set_times(0:5) %>%
    simulate()

  rs <- simulate(sc)
  # first batch must equal to ref sim
  expect_equal(rs[1:6, ], ref, tolerance=1e-6, ignore_attr=TRUE)
  # second batch must also equal to ref sim, apart from the timestamp and sim start (t==0)
  expect_equal(rs[7:11, -1], ref[2:6, -1], tolerance=1e-6, ignore_attr=TRUE)

  # negative starting time
  rs <- sc %>%
    set_times(-5:5) %>%
    simulate()
  expect_equal(rs[1:6, -1], ref[, -1], tolerance=1e-6, ignore_attr=TRUE)
  expect_equal(rs[7:11, -1], ref[2:6, -1], tolerance=1e-6, ignore_attr=TRUE)
})

test_that("regular intervals, not starting on transfer", {
  sc <- metsulfuron %>%
    set_init(c(BM=1)) %>%
    set_times(1:10) %>%
    set_noexposure() %>%
    set_transfer(interval=5, biomass=1)

  ref <- sc %>%
    set_notransfer() %>%
    set_times(0:5) %>%
    simulate()

  rs <- simulate(sc)
  expect_equal(rs[1:5, -1], ref[1:5, -1], tolerance=1e-6, ignore_attr=TRUE)
  expect_equal(rs[6:10, -1], ref[2:6, -1], tolerance=1e-6, ignore_attr=TRUE)

  # negative starting time
  rs <- sc %>%
    set_times(-4:5) %>%
    simulate()
  expect_equal(rs[1:5, -1], ref[1:5, -1], tolerance=1e-6, ignore_attr=TRUE)
  expect_equal(rs[6:10, -1], ref[2:6, -1], tolerance=1e-6, ignore_attr=TRUE)
})

test_that("regular intervals, not ending on transfer", {
  sc <- metsulfuron %>%
    set_init(c(BM=1)) %>%
    set_times(0:9) %>%
    set_noexposure() %>%
    set_transfer(interval=5, biomass=1)

  ref <- sc %>%
    set_notransfer() %>%
    set_times(0:5) %>%
    simulate()

  rs <- simulate(sc)
  expect_equal(rs[1:6, ], ref[1:6, ], tolerance=1e-6, ignore_attr=TRUE)
  expect_equal(rs[7:10, -1], ref[2:5, -1], tolerance=1e-6, ignore_attr=TRUE)

  # negative starting time
  rs <- sc %>%
    set_times(-5:4) %>%
    simulate()
  expect_equal(rs[1:6, -1], ref[1:6, -1], tolerance=1e-6, ignore_attr=TRUE)
  expect_equal(rs[7:10, ], ref[2:5, ], tolerance=1e-6, ignore_attr=TRUE)
})

test_that("regular intervals, no transfers relevant", {
  # starts and ends transfer
  sc <- metsulfuron %>%
    set_init(c(BM=1)) %>%
    set_times(0:5) %>%
    set_noexposure() %>%
    set_transfer(interval=5, biomass=1)
  rs <- simulate(sc)
  ref <- sc %>%
    set_notransfer() %>%
    simulate()

  expect_equal(rs, ref, tolerance=1e-6)

  # negative starting time
  rs <- sc %>%
    set_times(-5:0) %>%
    simulate()
  expect_equal(rs[, -1], ref[, -1], tolerance=1e-6, ignore_attr=TRUE)

  # starts and ends somewhere in between transfers
  sc <- sc %>% set_times(1:4)
  rs <- sc %>% simulate()
  ref <- sc %>%
    set_notransfer() %>%
    simulate()

  expect_equal(rs, ref, tolerance=1e-6)
})


test_that("regular intervals with redundant time-points", {
  times <- c(0, 0, 1:5, 5, 6, 6, 7:10)

  sc <- metsulfuron %>%
    set_init(c(BM=1)) %>%
    set_noexposure() %>%
    set_times(times) %>%
    set_transfer(interval=5, biomass=1)
  rs <- simulate(sc)

  ref <- sc %>%
    set_notransfer() %>%
    set_times(times[times <= 5]) %>%
    simulate()

  expect_equal(rs$time, times)
  expect_equal(rs[1:8, ], ref, tolerance=1e-6)
  expect_equal(rs[9:14, -1], ref[c(3, 3:7), -1], tolerance=1e-6, ignore_attr=TRUE)
})

test_that("transfer not in output times", {
  sc <- metsulfuron %>%
    set_init(c(BM=1)) %>%
    set_noexposure() %>%
    set_times(c(1, 10)) %>%
    set_transfer(interval=5, biomass=1)
  rs <- simulate(sc)

  ref <- sc %>%
    set_notransfer() %>%
    set_times(0:5) %>%
    simulate()

  expect_equal(rs[, 1], c(1, 10))
  expect_equal(rs[1, -1], ref[1, -1], tolerance=1e-6, ignore_attr=TRUE)
  expect_equal(rs[2, -1], ref[6, -1], tolerance=1e-6, ignore_attr=TRUE)
})

test_that("custom time points", {
  sc <- metsulfuron %>%
    set_init(c(BM=1)) %>%
    set_noexposure() %>%
    set_transfer(times=c(3, 8), biomass=1)
  rs <- simulate(sc)
  ref <- sc %>%
    set_notransfer() %>%
    simulate()

  expect_equal(rs[1:4, -1], ref[1:4, -1], tolerance=1e-6) # first batch
  expect_equal(rs[5:9, -1], ref[2:6, -1], tolerance=1e-6, ignore_attr=TRUE) # second batch
  expect_equal(rs[10:15, -1], ref[2:7, -1], tolerance=1e-6, ignore_attr=TRUE) # third batch
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
