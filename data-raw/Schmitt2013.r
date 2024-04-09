# Create rda file for Schmitt (2013) lemna effect data containing multiple
# treatments

#' Schmitt W., Bruns E., Dollinger M., and Sowig P., 2013: *Mechanistic TK/TD-model
#' simulating the effect of growth inhibitors on Lemna populations*. Ecol Model 255,
#' pp. 1-10. \doi{10.1016/j.ecolmodel.2013.01.017}

# ------------------------------------------------------------------------------
# Exposure data
Schmitt_exposure <- read.table(file = "data-raw/Schmitt2013.txt",
                             header = TRUE,
                             sep = "\t")

usethis::use_data(Schmitt2013, overwrite=TRUE)

rm(Schmitt_exposure)
