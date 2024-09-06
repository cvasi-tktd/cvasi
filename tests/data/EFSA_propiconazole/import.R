library(readxl)
library(dplyr)
library(ggplot2)

# Imports exposure series used for model validation in EFSA (2018), pp. 65,
# and saves them as an RDS object to be available for unit tests.
#
# EFSA Scientific Opinion on TKTD models, 2018. doi:10.2903/j.efsa.2018.5377

dir <- "tests/data/EFSA_propiconazole"

focus <- list()
# Apple scenarios
focus$R1pond <- read_xlsx(file.path(dir, "Propiconazole apple simulations.xlsx"), sheet="R1pond") %>%
  mutate(days=as.numeric(difftime(Time, Time[[1]], units="days"))) %>%
  select(days, conc_ugL=3)
focus$R2stream <- read_xlsx(file.path(dir, "Propiconazole apple simulations.xlsx"), sheet="R2stream") %>%
  mutate(days=as.numeric(difftime(Time, Time[[1]], units="days"))) %>%
  select(days, conc_ugL=3)

# Cereal scenarios
focus$D1ditch <- read_xlsx(file.path(dir, "Propiconazole spring cereals simulations.xlsx"), sheet="D1 ditch") %>%
  mutate(days=as.numeric(difftime(Time, Time[[1]], units="days"))) %>%
  select(days, conc_ugL=3)
focus$D1stream <- read_xlsx(file.path(dir, "Propiconazole spring cereals simulations.xlsx"), sheet="D1 stream") %>%
  mutate(days=as.numeric(difftime(Time, Time[[1]], units="days"))) %>%
  select(days, conc_ugL=3)
focus$D3ditch <- read_xlsx(file.path(dir, "Propiconazole spring cereals simulations.xlsx"), sheet="D3 dicth") %>%
  mutate(days=as.numeric(difftime(Time, Time[[1]], units="days"))) %>%
  select(days, conc_ugL=3)
focus$D4pond <- read_xlsx(file.path(dir, "Propiconazole spring cereals simulations.xlsx"), sheet="D4 pond") %>%
  mutate(days=as.numeric(difftime(Time, Time[[1]], units="days"))) %>%
  select(days, conc_ugL=3)
focus$D4stream <- read_xlsx(file.path(dir, "Propiconazole spring cereals simulations.xlsx"), sheet="D4 stream") %>%
  mutate(days=as.numeric(difftime(Time, Time[[1]], units="days"))) %>%
  select(days, conc_ugL=3)
focus$D5pond <- read_xlsx(file.path(dir, "Propiconazole spring cereals simulations.xlsx"), sheet="D5 pond") %>%
  mutate(days=as.numeric(difftime(Time, Time[[1]], units="days"))) %>%
  select(days, conc_ugL=3)
focus$D5stream <- read_xlsx(file.path(dir, "Propiconazole spring cereals simulations.xlsx"), sheet="D5 stream") %>%
  mutate(days=as.numeric(difftime(Time, Time[[1]], units="days"))) %>%
  select(days, conc_ugL=3)
focus$R4stream <- read_xlsx(file.path(dir, "Propiconazole spring cereals simulations.xlsx"), sheet="R4 stream") %>%
  mutate(days=as.numeric(difftime(Time, Time[[1]], units="days"))) %>%
  select(days, conc_ugL=3)

saveRDS(focus, file.path(dir, "focus.rds"))

rm(focus)
