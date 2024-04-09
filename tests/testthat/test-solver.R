test_that("GUTS-RED solver", {
  # IT model
  expect_equal(solver(minnow_it)$time, minnow_it@times)
  expect_equal(solver(minnow_it, times=0:2)$time, 0:2)
  # SD model
  expect_equal(solver(minnow_it)$time, minnow_it@times)
  expect_equal(solver(minnow_it, times=0:2)$time, 0:2)
})

test_that("Lemna solver", {
  # Schmitt model
  expect_equal(solver(metsulfuron)$time, metsulfuron@times)
  expect_equal(solver(metsulfuron, times=0:2)$time, 0:2)

  # Schmitt Threshold model
  Lemna_SchmittThold() %>%
    set_init(metsulfuron@init) %>%
    set_param(metsulfuron@param) %>%
    set_param(c("threshold"=2)) %>%
    set_forcings(metsulfuron@forcings) %>%
    set_exposure(metsulfuron@exposure@series) -> sc
  expect_equal(solver(sc)$time, sc@times)
  expect_equal(solver(sc, times=0:2)$time, 0:2)

  # SETAC model
  focusd1 %>% set_times(0:10) -> sc
  expect_equal(solver(sc)$time, sc@times)
  expect_equal(solver(sc, times=0:2)$time, 0:2)
})

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
  expect_equal(solver(sc)$TSL, solver(orig)$FrondNo)
  # check that TSL scales with r_DW_TSL
  r_DW_TSL <- focusd1@param$r_DW_FN * 10
  set_param(sc, c("r_DW_TSL"=r_DW_TSL)) -> sc
  expect_equal(solver(sc)$TSL, solver(orig)$FrondNo/10)

  # Myriophyllum  with logistic growth
  Myrio_log() %>%
    set_all(sc) %>%
    set_param(c("BM_L"=focusd1@param$BM_L,"r_DW_TSL"=focusd1@param$r_DW_FN)) %>%
    solver(nout=7) -> out_log
  # we should have almost reached the density limit
  expect_equal(tail(out_log$BM, 1), focusd1@param$BM_L, tolerance=0.1)
  # dBM should decrease continuously because initial state is above 50% of BM_L
  expect_true(all(out_log$dBM > 0))
  expect_true(all(out_log$BM == sort(out_log$BM)))

  # error if r_DW_TSL is missing
  sc@param$r_DW_TSL <- NULL
  expect_error(solver(sc), regexp="missing.*r_DW_TSL")
})


test_that("Custom solver", {
  my_ode <- function(t, state, params) {
    list(c(1)) # dummy ODE, constant derivative == linear growth
  }
  my_solver <- function(scenario, times, ...) {
    deSolve::ode(y=scenario@init, times=scenario@times, func=my_ode, ...)
  }

  setClass("test_model", contains="EffectScenario")
  setMethod("solver", "test_model", function(scenario, times, ...) my_solver(scenario, times, ...))
  on.exit(removeMethod("solver", "test_model"))
  on.exit(removeClass("test_model"))

  rs1 <- deSolve::ode(y=c(X=3), times=0:10, func=my_ode)
  rs2 <- new("test_model") %>%
    set_init(c(X=3)) %>%
    set_times(0:10) %>%
    simulate()

  expect_equal(rs2, rs2, tolerance=1e-6)
})

# TODO DEB* models
