test_that("sequence creation", {
  # empty sequence
  expect_equal(sequence()@scenarios, list())
  # initialised sequence
  l <- list(minnow_it)
  expect_equal(sequence(l)@scenarios, l)

  # allow NULLs
  l <- list(minnow_it, NULL)
  expect_equal(sequence(l)@scenarios, l)
})
test_that("sequence accessors", {
  # set single elements in empty sequence
  seq <- sequence()
  scenario(seq, 1) <- minnow_it
  expect_equal(seq@scenarios, list(minnow_it))

  seq <- sequence()
  scenario(seq, 2) <- minnow_it
  expect_equal(seq@scenarios, list(NULL, minnow_it))
  # update existing element
  seq <- suppressWarnings(sequence(list(minnow_it, NULL)))
  scenario(seq, 1) <- NULL
  scenario(seq, 2) <- minnow_it
  expect_equal(seq@scenarios, list(NULL, minnow_it))

  # set all elements at once
  seq <- sequence()
  scenarios(seq) <- list(minnow_it, NULL)
  expect_equal(seq@scenarios, list(minnow_it, NULL))
})
test_that("invalid sequence elements", {
  # no output times
  expect_warning(sequence(GUTS_RED_IT()), "no output times")
  # gap in output times
  sc1 <- GUTS_RED_IT() %>% set_times(1:2)
  sc2 <- GUTS_RED_IT() %>% set_times(4:5)
  expect_warning(sequence(list(sc1, sc2)), "time gap")
  # output time overlap
  sc1 <- GUTS_RED_IT() %>% set_times(1:2)
  sc2 <- GUTS_RED_IT() %>% set_times(1:3)
  expect_warning(sequence(list(sc1, sc2)), "time overlap")

  # ignore nulls and missing values
  expect_no_warning(sequence(list(NULL, sc1)))
  expect_no_warning(sequence(list(sc1, NULL)))
  expect_no_warning(sequence(list(sc2, NULL, sc1)))
  expect_no_warning(sequence(list(sc1, NULL, sc2)))
  sc0 <- GUTS_RED_IT()
  expect_warning(sequence(list(sc0, sc1)), "no output times")
})
