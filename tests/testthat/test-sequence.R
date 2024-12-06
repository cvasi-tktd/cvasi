test_that("sequence creation", {
  sc <- new("EffectScenario") %>% set_times(0:10)

  # single element
  expect_warning(sequence(list(sc)))
  # multiple elements
  expect_error(sequence(list(sc, sc)), "time overlap")
  sc2 <- sc %>% set_times(10:20)
  expect_equal(sequence(list(sc, sc2))@scenarios, list(sc, sc2))
})

test_that("breaks", {
  # scenarios overlap
  sc <- new("EffectScenario") %>% set_times(0:10)

  suppressMessages(l <- sequence(list(sc, sc), breaks=1)@scenarios)
  expect_equal(l[[1]]@times, c(0, 1))
  expect_equal(l[[2]]@times, 1:10)

  suppressMessages(l <- sequence(list(sc, sc), breaks=9)@scenarios)
  expect_equal(l[[1]]@times, 0:9)
  expect_equal(l[[2]]@times, 9:10)

  suppressMessages(l <- sequence(list(sc, sc), breaks=0.5)@scenarios)
  expect_equal(l[[1]]@times, c(0, 0.5))
  expect_equal(l[[2]]@times, c(0.5, 1:10))

  suppressMessages(l <- sequence(list(sc, sc), breaks=9.5)@scenarios)
  expect_equal(l[[1]]@times, c(0:9, 9.5))
  expect_equal(l[[2]]@times, c(9.5, 10))

  # scenarios have a gap
  sc1 <- new("EffectScenario") %>% set_times(0:4)
  sc2 <- new("EffectScenario") %>% set_times(6:10)

  suppressMessages(l <- sequence(list(sc1, sc2), breaks=3)@scenarios)
  expect_equal(l[[1]]@times, 0:3)
  expect_equal(l[[2]]@times, c(3, 6:10))

  suppressMessages(l <- sequence(list(sc1, sc2), breaks=4)@scenarios)
  expect_equal(l[[1]]@times, 0:4)
  expect_equal(l[[2]]@times, c(4, 6:10))

  suppressMessages(l <- sequence(list(sc1, sc2), breaks=5)@scenarios)
  expect_equal(l[[1]]@times, 0:5)
  expect_equal(l[[2]]@times, 5:10)

  suppressMessages(l <- sequence(list(sc1, sc2), breaks=6)@scenarios)
  expect_equal(l[[1]]@times, c(0:4, 6))
  expect_equal(l[[2]]@times, 6:10)

  suppressMessages(l <- sequence(list(sc1, sc2), breaks=6.5)@scenarios)
  expect_equal(l[[1]]@times, c(0:4, 6.5))
  expect_equal(l[[2]]@times, c(6.5, 7:10))
})

test_that("invalid arguments", {
  sc <- new("EffectScenario") %>% set_times(0:10)

  expect_error(sequence())
  expect_error(sequence("foo"))
  expect_error(sequence(list()))
  expect_warning(sequence(list(sc)))
  expect_error(sequence(list(sc, "foo")))
  expect_error(sequence(list(sc, sc), breaks="foo"))
  expect_error(sequence(list(sc, sc), breaks=c(1, 2)))
  suppressMessages(expect_error(sequence(list(sc, sc), breaks=c(0)), "too few"))
  suppressMessages(expect_error(sequence(list(sc, sc), breaks=c(20)), "too few"))
})

test_that("sequence_check", {
  sc <- new("EffectScenario")

  # no output times
  expect_error(sequence(list(sc, sc)), "no output times")
  # gap in output times
  sc1 <- sc %>% set_times(1:2)
  sc2 <- sc %>% set_times(4:5)
  expect_error(sequence(list(sc1, sc2)), "time gap")
  # output time overlap
  sc1 <- sc %>% set_times(1:2)
  sc2 <- sc %>% set_times(1:3)
  expect_error(sequence(list(sc1, sc2)), "time overlap")
  expect_error(sequence(list(sc, sc1)), "no output times")
})
