test_that("sequence creation", {
  sc <- new("EffectScenario") %>% set_times(0:10)

  # single element
  expect_warning(sequence(list(sc)))
  # multiple elements
  expect_error(sequence(list(sc, sc)), "time overlap")
  sc2 <- sc %>% set_times(10:20)
  expect_equal(sequence(list(sc, sc2))@scenarios, list(sc, sc2))
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
