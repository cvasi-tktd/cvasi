# These tests are very basic for now. Either they need to be extended or
# the plotting functions to be removed/amended.

test_that("plot_sd", {
  rs <- minnow_it %>%
    simulate() %>%
    dplyr::mutate(trial="foo")

  plot_sd(
    model_base = minnow_it,
    treatments = minnow_it@exposure@series %>% dplyr::mutate(trial="foo"),
    rs_mean = rs
  )
})

test_that("plot_ppc", {

  rs <- minnow_it %>%
    simulate() %>%
    dplyr::mutate(trial="foo")
  rr <- rs %>%
    dplyr::select(1, Y=2, X=2, 3)

  plot_ppc(rs_mean=rs,
           rs_range=rr,
           obs_mean=rs
  )
})

test_that("plot_epx", {
  rs <- metsulfuron %>%
          epx_mtw(level = 10, factor_cutoff = 1000)
  plot_epx(rs, metsulfuron@exposure@series)
})

test_that("plot_scenario", {
  lifecycle::expect_deprecated({
    plot_scenario(minnow_it)
  })
})
