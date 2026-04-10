# data
ringtest_b_pulse <- read.table(file = "data-raw/efsa_ringtest_b_pulse.txt",
                               header = TRUE) %>%
  dplyr::select(time, Nsurv, replicate, conc)

# write data
usethis::use_data(ringtest_b_pulse, overwrite=TRUE)

rm(ringtest_b_pulse)
