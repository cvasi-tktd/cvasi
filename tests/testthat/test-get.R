test_that("get_model", {
  s1 <- new("EffectScenario", name="foo")
  s2 <- new("EffectScenario", name="bar")
  p3 <- new("parameter_set", model="baz")

  # empty argument
  expect_equal(get_model(list()), list())
  # single argument
  expect_equal(get_model(s1), s1@name)
  expect_equal(get_model(p3), p3@model)
  # vectorized arguments
  expect_equal(get_model(c(s1,s2)), c(s1@name, s2@name))
  expect_equal(get_model(c(s1,p3)), c(s1@name, p3@model))
  # invalid arguments
  expect_error(get_model(assessment()))
})

test_that("get_tag", {
  s1 <- new("EffectScenario", tag="foo")
  s2 <- new("EffectScenario", tag="bar")
  p3 <- new("parameter_set", tag="baz")

  # empty argument
  expect_equal(get_tag(list()), list())
  # single argument
  expect_equal(get_tag(s1), s1@tag)
  expect_equal(get_tag(p3), p3@tag)
  # vectorized arguments
  expect_equal(get_tag(c(s1,s2)), c(s1@tag, s2@tag))
  expect_equal(get_tag(c(s1,p3)), c(s1@tag, p3@tag))
  # invalid arguments
  expect_error(get_tag(assessment()))
})
