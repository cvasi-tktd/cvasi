test_that("set parameters", {
  nm <- "foo"
  tag <- "bar"
  p1 <- c("a"=1, "b"=2)
  p2 <- list("a"=1, "b"=2)

  # name
  expect_equal(parameter_set(nm)@model, nm)
  # tag
  expect_equal(parameter_set(nm, tag=tag)@tag, tag)
  # parameters
  expect_equal(parameter_set(nm, param=p1)@param, p2) # must be a list
  expect_equal(parameter_set(nm, param=p2)@param, p2)
})


test_that("invalid parameters", {
  # name: invalid type
  expect_error(parameter_set(list()))
  # name missing
  expect_error(parameter_set(tag="bar"))
  # tag: invalid type
  expect_error(parameter_set("foo", tag=list()))
  # param: invalid type
  expect_error(parameter_set("foo", param=data.frame()))
  expect_error(parameter_set("foo", param=list(data.frame())))
  expect_error(parameter_set("foo", param=list("a"=1, "b"=list())))
})
