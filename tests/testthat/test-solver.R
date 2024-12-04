test_that("no solver defined", {
  expect_error(solver(new("EffectScenario")), "solver missing")
})

test_that("custom solver", {
  setClass("test_model", contains="EffectScenario")
  setMethod("solver", "test_model", function(scenario, ...) return("foobar"))
  on.exit(removeMethod("solver", "test_model"))
  on.exit(removeClass("test_model"))

  rs <- new("test_model") %>% simulate()
  expect_equal(rs, "foobar", ignore_attr=TRUE)
})
