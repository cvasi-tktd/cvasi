test_that("simulate_batch", {
  t1 <- data.frame(time=0:10, conc=0, trial="control")
  t2 <- data.frame(time=0:10, conc=1, trial="T1")
  treatments <- rbind(t1, t2)

  rs1 <- metsulfuron %>% set_exposure(t1[,c(1,2)]) %>% simulate()
  rs2 <- metsulfuron %>% set_exposure(t2[,c(1,2)]) %>% simulate()
  #lifecycle::expect_deprecated({
    rsb <- metsulfuron %>% simulate_batch(treatments)
  #})

  expect_equal(dplyr::select(rsb, !trial), rbind(rs1, rs2), ignore_attr=T, tolerance=1e-5)
  expect_equal(rsb$trial, rep(c("control","T1"), each=nrow(rs1)))
})

test_that("deprecated parameter", {
  expect_error({
    lifecycle::expect_deprecated({
      simulate_batch(metsulfuron, data.frame(), param_sample=23)
    })
  })
})
