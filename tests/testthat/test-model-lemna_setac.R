# converts a scenario object defined by the `lemna` package to a scenario
# of the `cvasi` package
lemna2scenario <- function(sc) {
  if(!is(sc, "lemna_scenario"))
    stop("argument is not a lemna scenario")

  forc <- sc$envir
  forc$conc <- NULL

  Lemna_SETAC() %>%
    set_init(sc$init) %>%
    set_param(sc$param) %>%
    set_exposure(sc$envir$conc) %>%
    set_times(sc$times) %>%
    set_forcings(forc)
}

# Make sure that simulation results of this package and 'lemna' are identical,
# the 'lemna' package vouches for correct model implementation
test_that("lemna::lemna, k_phot0_fixed=FALSE", {
  skip_on_os("mac") # macos numerics always deviate in some unforeseeable way
  skip_if_not_installed("lemna")

  origR <- lemna::lemna(lemna::focusd1, times=0:20, hmax=0.01, ode_mode="r")
  origC <- lemna::lemna(lemna::focusd1, times=0:20, hmax=0.01, ode_mode="c")

  lemna::focusd1 %>%
    lemna2scenario() %>%
    set_times(0:20) %>%
    simulate(hmax=0.01) -> out

  expect_equal(out, origR, ignore_attr=TRUE, tolerance=1e-3)
  expect_equal(out, origC, ignore_attr=TRUE, tolerance=1e-8)
})

test_that("lemna::lemna, k_phot0_fixed=TRUE", {
  skip_on_os("mac") # macos numerics always deviate in some unforeseeable way
  skip_if_not_installed("lemna")

  lemna_sc <- lemna::focusd1
  lemna_sc$param$k_photo_fixed <- 1
  origR <- lemna::lemna(lemna_sc, times=0:20, hmax=0.01, ode_mode="r")
  origC <- lemna::lemna(lemna_sc, times=0:20, hmax=0.01, ode_mode="c")

  lemna2scenario(lemna_sc) %>%
    set_times(0:20) %>%
    simulate(hmax=0.01) -> out

  expect_equal(out, origR, ignore_attr=TRUE, tolerance=1e-3)
  expect_equal(out, origC, ignore_attr=TRUE, tolerance=1e-8)
})
