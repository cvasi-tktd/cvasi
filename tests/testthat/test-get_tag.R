test_that("supported object types", {
  # empty argument
  expect_equal(get_tag(list()), list())
  # single scenario
  s1 <- new("EffectScenario") %>% set_tag("foo") %>% set_times(0:5)
  expect_equal(get_tag(s1), "foo")
  # multiple scenarios
  s2 <- new("EffectScenario") %>% set_tag("bar") %>% set_times(0:5)
  expect_equal(get_tag(list(s1, s2)), list("foo", "bar"))
  # scenario sequence
  suppressMessages(seq <- sequence(list(s1, s2), breaks=3))
  expect_equal(get_tag(seq), list("foo", "bar"))
  # parameter set
  ps <- parameter_set("xy", tag="baz")
  expect_equal(get_tag(ps), "baz")
})

test_that("unsupported object types", {
  # invalid arguments
  expect_error(get_tag(1))
})
