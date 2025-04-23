test_that("get_model", {
  s1 <- new("EffectScenario", name="foo")
  s2 <- new("EffectScenario", name="bar")
  p3 <- new("ParameterSet", model="baz")

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

test_that("get_window", {
  sc <- new("EffectScenario") %>% set_window(length=-1, interval=-1)
  expect_equal(get_window(sc), list(length=-1, interval=-1))

  sc <- new("EffectScenario") %>% set_window(length=2, interval=3)
  l <- list(length=2, interval=3)
  expect_equal(get_window(sc), l)

  expect_equal(get_window(list(sc, sc)), list(l, l))
})
