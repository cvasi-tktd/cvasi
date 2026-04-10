# data
ringtest_b_const <- read.table(file = "data-raw/efsa_ringtest_b_const.txt",
                               header = TRUE) %>%
  dplyr::select(time, Nsurv, replicate, conc)

# write data
usethis::use_data(ringtest_b_const, overwrite=TRUE)

rm(ringtest_b_const)
