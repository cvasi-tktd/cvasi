
##
## Derive compound parameters for Americamysis bahia model from AddMyPet
## database
## https://www.bio.vu.nl/thb/deb/deblab/add_my_pet/entries_web/Americamysis_bahia/Americamysis_bahia_res.html
##

# values derived from Americamysis' pars_init_*.m file
param <- jsonlite::fromJSON("data-raw/americamysis_param.json")
init <- jsonlite::fromJSON("data-raw/americamysis_init.json")

# medium exposure level from AddMyPet model
expsr <- data.frame(t=1:28,c=2.55)

# create scenario object
DEB_abj() %>%
  set_tag("americamysis") %>%
  set_init(init) %>%
  set_window(length=21, interval=1) %>%
  set_param(param) %>%
  set_exposure(expsr) -> americamysis

americamysis@exposure@file <- "constant exposure"
usethis::use_data(americamysis, overwrite=TRUE)

# save data object

rm(expsr, param, init, americamysis)
