
test_that("Myrio simulation", {
  nms <- intersect(Myrio()@param.req, names(focusd1@param))

  # set up a Myriophyllum scenario with Lemna parameters for testing purposes
  Myrio() %>%
    set_init(focusd1@init) %>%
    set_param(focusd1@param[nms]) %>%
    set_param(c("r_DW_TSL"=focusd1@param$r_DW_FN)) %>%
    set_exposure(focusd1@exposure@series) %>%
    set_times(0:20) -> sc

  focusd1 %>%
    set_param(c("k_photo_fixed"=1,"k_loss"=0)) %>%
    set_times(0:20) -> orig

  # equal results if all else is identical
  expect_equal(as.data.frame(solver(sc))$TSL, as.data.frame(solver(orig))$FrondNo)
  # check that TSL scales with r_DW_TSL
  r_DW_TSL <- focusd1@param$r_DW_FN * 10
  set_param(sc, c("r_DW_TSL"=r_DW_TSL)) -> sc
  expect_equal(as.data.frame(solver(sc))$TSL, as.data.frame(solver(orig))$FrondNo/10)

  # Myriophyllum  with logistic growth
  Myrio_log() %>%
    set_all(sc) %>%
    set_param(c("BM_L"=focusd1@param$BM_L,"r_DW_TSL"=focusd1@param$r_DW_FN)) %>%
    solver(nout=7) %>%
    as.data.frame() -> out_log
  # we should have almost reached the density limit
  expect_equal(tail(out_log$BM, 1), focusd1@param$BM_L, tolerance=0.1)
  # dBM should decrease continuously because initial state is above 50% of BM_L
  expect_true(all(out_log$dBM > 0))
  expect_true(all(out_log$BM == sort(out_log$BM)))

  # error if r_DW_TSL is missing
  sc@param$r_DW_TSL <- NULL
  expect_error(solver(sc), regexp="missing.*r_DW_TSL")
})
