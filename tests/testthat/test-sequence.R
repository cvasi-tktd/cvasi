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

test_that("set new times", {
  sc1 <- new("EffectScenario") %>% set_times(0:3)
  sc2 <- new("EffectScenario") %>% set_times(3:6)

  # no breaks
  suppressWarnings(sq <- sequence(list(sc1)) %>% set_times(1:5))
  expect_equal(sq[[1]]@times, 1:5)

  # some breaks
  suppressMessages(sq <- sequence(list(sc1, sc2)) %>% set_times(1:5))
  expect_equal(sq[[1]]@times, 1:3)
  expect_equal(sq[[2]]@times, 3:5)

  expect_error(suppressMessages(sq %>% set_times(3:5)), "too few output times")
  expect_error(suppressMessages(sq %>% set_times(1:3)), "too few output times")
  expect_error(suppressMessages(sq %>% set_times(10:13)), "too few output times")}
)

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

test_that("breaks_from_sequence", {
  sc1 <- new("EffectScenario") %>% set_times(0:5)

  # no breaks
  suppressWarnings(sq <- sequence(list(sc1)))
  expect_equal(breaks_from_sequence(sq), numeric(0))
  # one break
  suppressMessages(sq <- sequence(list(sc1, sc1), breaks=3))
  expect_equal(breaks_from_sequence(sq), 3)
  # multiple breaks
  suppressMessages(sq <- sequence(list(sc1, sc1, sc1), breaks=c(2,3.1)))
  expect_equal(breaks_from_sequence(sq), c(2, 3.1))
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

test_that("array access", {
  sc1 <- new("EffectScenario") %>% set_times(0:3)
  sc2 <- new("EffectScenario") %>% set_times(3:6)
  sc3 <- new("EffectScenario") %>% set_times(3:9)
  sq <- sequence(list(sc1, sc2))

  # multiple elements
  expect_equal(sq[1], list(sc1))
  expect_equal(sq[2], list(sc2))
  expect_equal(sq[c(1, 2)], list(sc1, sc2))

  # single elements
  expect_equal(sq[[1]], sc1)
  expect_equal(sq[[2]], sc2)

  # assignments
  sq[[2]] <- sc3
  expect_equal(sq[[2]], sc3)

  # invalid arguments
  expect_error(sq[0], "out of bounds")
  expect_error(sq[10], "out of bounds")
  expect_error(sq[c(0, 42)], "out of bounds")

  expect_error(sq[[0]], "out of bounds")
  expect_error(sq[[10]], "out of bounds")
  expect_error(sq[[c(1, 2)]], "length one")

  expect_error(sq[[c(1, 2)]] <- sc3, "length one")
  expect_error(sq[[0]] <- sc3, "out of bounds")
  expect_error(sq[[10]] <- sc3, "out of bounds")
})
