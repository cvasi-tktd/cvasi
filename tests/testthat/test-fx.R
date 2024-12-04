test_that("default fx", {
  sc <- minnow_it %>% set_endpoints(c("D", "H"))
  sim <- unlist(tail(simulate(sc), n=1))

  # the return value of `fx_default()` is simply the value at the end of the
  # simulation

  # single endpoint
  sc <- sc %>% set_endpoints("D")
  expect_equal(fx_default(sc), sim["D"])
  # multiple endpoints
  sc <- sc %>% set_endpoints(c("D", "H"))
  expect_equal(fx_default(sc), sim[c("D", "H")])
  # endpoint not contained in simulation output
  sc <- sc %>% set_endpoints(c("D", "foobar"))
  expect_equal(fx_default(sc), c(sim[c("D")], "foobar"=NA_real_))
})

test_that("tail_nm", {
  # data.frame
  expect_equal(tail_nm(data.frame(a=0, b=1)), c(a=0, b=1))
  expect_equal(tail_nm(data.frame(a=0:3, b=1:4)), c(a=3, b=4))

  # matrix
  expect_equal(tail_nm(as.matrix(data.frame(a=0, b=1))), c(a=0, b=1))
  expect_equal(tail_nm(as.matrix(data.frame(a=0:3, b=1:4))), c(a=3, b=4))

  # other types
  expect_error(tail_nm(c(a=1, b=2)))
  expect_error(tail_nm(list(a=1, b=2)))
})
