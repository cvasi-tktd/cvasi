# Make sure that simulation results of this package and 'lemna' are identical,
# the 'lemna' package vouches for correct model implementation
test_that("lemna::lemna, k_phot0_fixed=FALSE", {
  skip_on_os("mac") # macos numerics always deviate in some unforeseeable way
  skip_if_not_installed("lemna")
  source(test_path("lemna_helpers.R"), local=TRUE)

  origR <- lemna::lemna(lemna::focusd1, times=0:20, hmax=0.01, ode_mode="r")
  origC <- lemna::lemna(lemna::focusd1, times=0:20, hmax=0.01, ode_mode="c")

  lemna::focusd1 %>%
    lemna2scenario() %>%
    simulate(times=0:20, hmax=0.01) -> out

  expect_equal(out, origR, ignore_attr=TRUE, tolerance=1e-3)
  expect_equal(out, origC, ignore_attr=TRUE, tolerance=1e-8)
})

test_that("lemna::lemna, k_phot0_fixed=TRUE", {
  skip_on_os("mac") # macos numerics always deviate in some unforeseeable way
  skip_if_not_installed("lemna")
  source(test_path("lemna_helpers.R"), local=TRUE)

  lemna_sc <- lemna::focusd1
  lemna_sc$param$k_photo_fixed <- 1
  origR <- lemna::lemna(lemna_sc, times=0:20, hmax=0.01, ode_mode="r")
  origC <- lemna::lemna(lemna_sc, times=0:20, hmax=0.01, ode_mode="c")

  lemna2scenario(lemna_sc) %>%
    simulate(times=0:20, hmax=0.01) -> out

  expect_equal(out, origR, ignore_attr=TRUE, tolerance=1e-3)
  expect_equal(out, origC, ignore_attr=TRUE, tolerance=1e-8)
})
