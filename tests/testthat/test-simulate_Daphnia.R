# simulations to verify the model implementation
test_that("verify: constant exposure", {
  source(test_path("init-simulate_Daphnia.R"), local=TRUE)

  param <- tfs_param
  param$f <- 0.88

  tfs_result %>%
    dplyr::filter(set=="set3" & run=="control") %>%
    dplyr::pull(time) -> times

  for(i in seq(2, length(tfs_exposure$cst))) {
    exposure <- tfs_exposure$cst[,c(1,i)]

    DEB_Daphnia() %>%
      set_init(tfs_init) %>%
      set_param(param) %>%
      set_times(times) %>%
      set_exposure(exposure, reset_times = FALSE) %>%
      simulate() -> out

    runstr <- ifelse(i == 2, "control", paste0("CST_",(i-2)))
    tfs_result %>%
      dplyr::filter(set == "set3" & run == runstr) %>%
      dplyr::select(!c(set, run)) -> verify
    expect_equal(out, verify, tolerance=1e-3, ignore_attr=TRUE)
  }
})

test_that("verify: variable exposure 1", {
  source(test_path("init-simulate_Daphnia.R"), local=TRUE)

  tfs_result %>%
    dplyr::filter(set=="set1" & run=="control") %>%
    dplyr::pull(time) -> times

  for(i in seq(2, length(tfs_exposure$var1))) {
    exposure <- tfs_exposure$var1[,c(1,i)]

    DEB_Daphnia() %>%
      set_init(tfs_init) %>%
      set_param(tfs_param) %>%
      set_times(times) %>%
      set_exposure(exposure, reset_times = FALSE) %>%
      simulate() -> out

    runstr <- ifelse(i == 2, "control", paste0("VAR1_",(i-2)))
    tfs_result %>%
      dplyr::filter(set == "set1" & run == runstr) %>%
      dplyr::select(!c(set, run)) -> verify
    expect_equal(out, verify, tolerance=1e-3, ignore_attr=TRUE)
  }
})

test_that("verify: variable exposure 2", {
  source(test_path("init-simulate_Daphnia.R"), local=TRUE)

  tfs_result %>%
    dplyr::filter(set=="set2" & run=="control") %>%
    dplyr::pull(time) -> times

  for(i in seq(2, length(tfs_exposure$var1))) {
    exposure <- tfs_exposure$var2[,c(1,i)]

    DEB_Daphnia() %>%
      set_init(tfs_init) %>%
      set_param(tfs_param) %>%
      set_times(times) %>%
      set_exposure(exposure, reset_times = FALSE) %>%
      simulate(hmax=0.01) -> out

    runstr <- ifelse(i == 2, "control", paste0("VAR2_",(i-2)))
    tfs_result %>%
      dplyr::filter(set == "set2" & run == runstr) %>%
      dplyr::select(!c(set, run)) -> verify
    expect_equal(out, verify, tolerance=1e-3, ignore_attr=TRUE)
  }
})

# model behavior
test_that("brood-pouch delay", {
  source(test_path("init-simulate_Daphnia.R"), local=TRUE)

  # irregular time points
  dmagna %>%
    set_param(c(Tbp=3)) %>%
    simulate() -> res_bp
  dmagna %>%
    set_param(c(Tbp=0)) %>%
    simulate() -> res_no
  # last time point with zero reproduction should be roughly shifted by Tbp
  expect_equal(
    res_bp %>% dplyr::filter(R==0) %>% dplyr::pull(time) %>% max(),
    res_no %>% dplyr::filter(R==0) %>% dplyr::pull(time) %>% max() + 3,
    tolerance=0.1
  )
  expect_equal(res_bp$L, res_no$L)

  # regular time points
  dmagna %>%
    set_times(0:21) %>%
    set_param(c(Tbp=3)) %>%
    simulate() -> res_bp
  dmagna %>%
    set_times(0:21) %>%
    set_param(c(Tbp=0)) %>%
    simulate() -> res_no
  expect_equal(
    res_bp %>% dplyr::filter(R==0) %>% dplyr::pull(time) %>% max(),
    res_no %>% dplyr::filter(R==0) %>% dplyr::pull(time) %>% max() + 3
  )
  expect_equal(tail(res_bp$R, 19), head(res_no$R, 19))
  expect_equal(res_bp$L, res_no$L)
})
