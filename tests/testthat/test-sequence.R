test_that("sequence creation", {
  sc <- new("EffectScenario") %>% set_times(0:10)

  # single element
  expect_warning(sequence(list(sc)))
  # multiple elements
  expect_error(sequence(list(sc, sc)), "time overlap")
  sc2 <- sc %>% set_times(10:20)
  expect_equal(sequence(list(sc, sc2))@scenarios, list(sc, sc2))
})

test_that("split_sequence", {
  # scenarios overlap
  sc <- new("EffectScenario") %>% set_times(0:10)

  # single scenario, no changes
  suppressWarnings(sq <- sequence(list(sc)))
  expect_equal(sq[[1]]@times, 0:10)

  # two scenarios, both scenarios include the break
  sq <- sequence(list(sc, sc), breaks=5)
  expect_equal(sq[[1]]@times, 0:5)
  expect_equal(sq[[2]]@times, 5:10)
  expect_equal(sq@inc_start, c(TRUE, FALSE))
  expect_equal(sq@inc_end, c(TRUE, TRUE))

  # break before the first scenario
  sq <- sequence(list(sc, sc), breaks=-1)
  expect_equal(sq[[1]]@times, integer(0))
  expect_equal(sq[[2]]@times, 0:10)
  expect_equal(sq@inc_start, c(FALSE, TRUE))
  expect_equal(sq@inc_end, c(FALSE, TRUE))

  # break after the second scenario
  sq <- sequence(list(sc, sc), breaks=11)
  expect_equal(sq[[1]]@times, 0:10)
  expect_equal(sq[[2]]@times, integer(0))
  expect_equal(sq@inc_start, c(TRUE, FALSE))
  expect_equal(sq@inc_end, c(TRUE, FALSE))


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
  sq <- sequence(list(sc1, sc2)) %>% set_times(1:5)
  expect_equal(sq[[1]]@times, 1:3)
  expect_equal(sq[[2]]@times, 3:5)

  # no output for 1st scenario in sequence
  sq <- sequence(list(sc1, sc2)) %>% set_times(5:7)
  expect_equal(sq[[1]]@times, numeric(0))
  expect_equal(sq[[2]]@times, 5:7)

  # no output for 2nd scenario in sequence
  sq <- sequence(list(sc1, sc2)) %>% set_times(1:3)
  expect_equal(sq[[1]]@times, 1:3)
  expect_equal(sq[[2]]@times, numeric(0))
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

test_that("included outputs", {
  # include breaks if scenarios were split by user
  sc1 <- minnow_it %>% set_times(0:3)
  sc2 <- minnow_it %>% set_times(3:6)
  sq <- sequence(list(sc1, sc2))
  expect_equal(sq@inc_start, c(TRUE, FALSE))
  expect_equal(sq@inc_end, c(TRUE, TRUE))

  # remove breaks from output if it was automatically split
  sc1 <- minnow_it %>% set_times(0:2)
  sc2 <- minnow_it %>% set_times(3:6)
  sq <- sequence(list(sc1, sc2), breaks=3)
  expect_equal(sq@inc_start, c(TRUE, FALSE))
  expect_equal(sq@inc_end, c(FALSE, TRUE))

  # multiple breaks
  sc <- minnow_it %>% set_times(0:6)
  sq <- sequence(list(sc, sc, sc), breaks=c(2.1, 4))
  expect_equal(sq@inc_start, c(TRUE, FALSE, FALSE))
  expect_equal(sq@inc_end, c(FALSE, TRUE, TRUE))
})

test_that("sequence_check", {
  sc <- new("EffectScenario")

  # gap in output times
  sc1 <- sc %>% set_times(1:2)
  sc2 <- sc %>% set_times(4:5)
  expect_error(sequence(list(sc1, sc2)), "time gap")
  # output time overlap
  sc1 <- sc %>% set_times(1:2)
  sc2 <- sc %>% set_times(1:3)
  expect_error(sequence(list(sc1, sc2)), "time overlap")
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

test_that("mocking scenarios", {
  sq <- new("ScenarioSequence")
  sc <- new("EffectScenario", tag="foo", exposure=no_exposure()) %>% set_times(0:5)

  # slot not emulated yet -> fail
  expect_error(sq@tag)

  # mock from supplied scenario
  mock <- mock_as_scenario(sq, sc)
  expect_equal(mock@tag, sc@tag)

  # writing should fail
  expect_error(mock@tag <- "bar")
  # some slot should never be mocked
  expect_equal(mock@exposure, NA)

  # mock from first scenario in sequence
  sc1 <- sc %>% set_tag("baz")
  sq <- sequence(list(sc1, sc), breaks=3)
  mock <- mock_as_scenario(sq)
  expect_equal(mock@tag, sc1@tag)
  expect_equal(mock@times, sc@times)
})

test_that("mocked slots", {
  # newly created
  sc1 <- new("EffectScenario", tag="foo", times=0:5)
  sc2 <- sc1 %>% set_tag("bar")
  sq <- sequence(list(sc1, sc2), breaks=3)
  expect_equal(sq@tag, sc1@tag)

  # first scenario updated
  sq[[1]] <- sq[[1]] %>% set_tag("baz")
  expect_equal(sq[[1]]@tag, "baz")
  expect_equal(sq@tag, "baz")
})

test_that("with simulate()", {
  tol <- 1e-5

  # guts red it
  sc <- minnow_it
  suppressWarnings(sq <- sequence(list(sc)))
  expect_equal(simulate(sq), simulate(sc), tolerance=tol)

  # lemna
  sc <- metsulfuron
  suppressWarnings(sq <- sequence(list(sc)))
  expect_equal(simulate(sq), simulate(sc), tolerance=tol)

  # multiple elements in sequence
  suppressWarnings(sq <- sequence(list(sc, sc), breaks=3.1))
  expect_equal(simulate(sq), simulate(sc), ignore_attr=TRUE, tolerance=tol)
})

test_that("with fx()", {
  tol <- 1e-5

  # guts red it
  sc <- minnow_it
  suppressWarnings(sq <- sequence(list(sc)))
  expect_equal(fx(sq), fx(sc), tolerance=tol)

  # lemna
  sc <- metsulfuron
  suppressWarnings(sq <- sequence(list(sc)))
  expect_equal(fx(sq), fx(sc), tolerance=tol)
})

test_that("with effect()", {
  tol <- 1e-5

  # no moving windows
  sc <- minnow_it
  suppressWarnings(sq <- sequence(list(sc)))
  expect_equal(effect(sq, ep_only=TRUE), effect(sc, ep_only=TRUE), ignore_attr=TRUE, tolerance=tol)

  # with moving windows
  sc <- minnow_it %>% set_window(length=2, interval=2)
  suppressWarnings(sq <- sequence(list(sc)))
  expect_equal(effect(sq, ep_only=TRUE, max_only=FALSE),
               effect(sc, ep_only=TRUE, max_only=FALSE),
               ignore_attr=TRUE, tolerance=tol)

  sc <- minnow_it %>%
    set_exposure(data.frame(t=0:4, c=c(0, 0, 0, 1, 1))) %>%
    set_window(length=2, interval=2)
  sq <- sequence(list(sc, sc), breaks=2)
  expect_equal(effect(sq, ep_only=TRUE, max_only=FALSE),
               effect(sc, ep_only=TRUE, max_only=FALSE),
               ignore_attr=TRUE, tolerance=tol)
})

test_that("with epx()", {
  tol <- 1e-5

  # no moving windows
  sc <- minnow_it
  suppressWarnings(sq <- sequence(list(sc)))
  expect_equal(epx(sc, ep_only=TRUE),
               epx(sq, ep_only=TRUE),
               tolerance=tol, ignore_attr=TRUE)

  # with moving windows
  sc <- sc %>%
    set_exposure(data.frame(t=0:4, c=c(0, 0, 0, 1, 1))) %>%
    set_window(length=2, interval=1)
  suppressWarnings(sq <- sequence(list(sc)))
  expect_equal(epx(sc, ep_only=TRUE),
               epx(sq, ep_only=TRUE),
               tolerance=tol, ignore_attr=TRUE)
})

