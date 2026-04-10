# The original data is coming from
# 'https://openguts.info/download.html'
# Go to 'Download the example files list' link and click
# If you unzip it, go to example_files/input_data/ringtest_data
# This script is to manually change the original data format to long format

library(tidyverse)

# ------------------------------------------------------
# 1. Survival data (wide)
# ------------------------------------------------------
surv <- tibble(
  time = 0:4,
  Control = c(20,19,19,19,19),
  T1 = c(20,20,20,20,19),
  T2 = c(20,19,19,19,17),
  T3 = c(20,19,19,18,16),
  T4 = c(21,21,20,16,16),
  T5 = c(20,17,6,2,1),
  T6 = c(20,11,4,0,0),
  T7 = c(20,11,1,0,0)
)

# ------------------------------------------------------
# 2. Concentration data (wide)
# ------------------------------------------------------
conc <- tibble(
  time = 0,
  Control = 0,
  T1 = 8.05,
  T2 = 11.906,
  T3 = 13.8,
  T4 = 17.872,
  T5 = 24.186,
  T6 = 28.93,
  T7 = 35.924
)

# ------------------------------------------------------
# 3. Wide → Long
# ------------------------------------------------------
surv_long <- surv %>%
  pivot_longer(cols = -time, names_to = "replicate", values_to = "Nsurv")

conc_long <- conc %>%
  pivot_longer(cols = -time, names_to = "replicate", values_to = "conc")

# ------------------------------------------------------
# 4. full join
# ------------------------------------------------------
ringtest_b_const<- full_join(surv_long, conc_long, by = c("time", "replicate")) %>%
  mutate(scenario = "const") %>%
  select(replicate, scenario, conc, time, Nsurv) %>%
  arrange(time, replicate)


# ------------------------------------------------------
# 5. Filling NAs.
# ------------------------------------------------------
ringtest_b_const <- ringtest_b_const %>%
  group_by(replicate) %>%
  mutate(
    conc = ifelse(is.na(conc),
                  conc[time == 0][1],
                  conc)
  ) %>%
  ungroup()


ringtest_b_const <- as.data.frame(ringtest_b_const)

# check
ringtest_b_const
