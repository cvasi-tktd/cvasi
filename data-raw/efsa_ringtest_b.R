# data
ringtest_b <- read.table(file = "data-raw/efsa_ringtest_b.txt",
                               header = TRUE) %>%
  dplyr::select(time, Nsurv, replicate, conc)

# write data
usethis::use_data(ringtest_b, overwrite=TRUE)

rm(ringtest_b)
