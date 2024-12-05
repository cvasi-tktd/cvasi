test_that("simulate", {
  sc <- americamysis %>% set_param(list(p_M=NULL))
  expect_error(simulate(sc), "missing parameter")

  # optional output variables
  rs1 <- simulate(americamysis %>% set_nowindow)
})

test_that("effect", {
  sc <- americamysis %>% set_window(length=-1)
  ctrl <- sc %>% set_noexposure() %>% simulate()
  t1 <- sc %>% simulate()

  myeffect <- 1 - tail(t1$L, n=1)/tail(ctrl$L, n=1)
  expect_equal(effect(sc)$L[1], myeffect, tolerance=1e-5)
})
