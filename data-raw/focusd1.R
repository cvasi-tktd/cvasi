

# Model parameters for the substance metsulfuron-methyl as reported by
# Schmitt et al. (2013) in combination with the modified dry-weight per frond
# as used by Hommen et al. (2015)
focus_param <- c(
  # variable environmental conditions
  k_photo_fixed = FALSE,
  # growth model
  BM_min = 0,
  BM_L = 176,
  # toxicodynamics
  b = 4.16,
  EC50_int = 0.3,
  E_max = 0.784,
  # toxicokinetics
  P = 0.0054,
  K_pw = 0.75,
  k_met = 0,
  r_DW_FN = 0.0004
)



# Create a lemna_scenario using time series of the FOCUS D1 Ditch scenario
# as reported by Hommen et al. (2015)
#
focus_envir <- list(
  tmp = read.delim("data-raw/D1_Temp_1.1.76-30.4.83.txt", header=FALSE, col.names=c("Time", "Temp")) %>%
    dplyr::filter(Time >= 2192 & Time <= 2557) %>%
    dplyr::mutate(Time = dplyr::row_number() - 1),
  irr = read.delim("data-raw/D1_Rad_1.1.76-30.4.83.txt", header=FALSE, col.names=c("Time", "Rad")) %>%
    dplyr::filter(Time >= 2192 & Time <= 2557) %>%
    dplyr::mutate(Time = dplyr::row_number() - 1),
  P = 0.3, # constant Phosphorus concentration of 0.3 mg L-1
  N = 0.6  # constant Nitrogen concentration of 0.6 mg L-1
)

# Simple exposure pattern representing a step-function
focus_conc <- read.delim("data-raw/D1_Conc.txt") %>%
  dplyr::filter(Time >= 2192 & Time <= 2557) %>%
  dplyr::mutate(Time = (dplyr::row_number() - 1) / 24)

# Create scenario
focusd1 <- Lemna_SETAC() %>%
  set_tag("focusd1") %>%
  set_init(c(BM=80, M_int=0)) %>%
  set_param(focus_param) %>%
  set_forcings(focus_envir) %>%
  set_exposure(focus_conc, reset_times=FALSE) %>%
  set_times(0:365)
focusd1@exposure@file <- "FOCUS D1 Ditch"

usethis::use_data(focusd1, overwrite = TRUE)
rm(focus_param,focus_conc,focus_envir,focusd1)
