# The original data is coming from
# 'https://openguts.info/download.html'
# Go to 'Download the example files list' link and click
# If you unzip it, go to example_files/input_data/ringtest_data
# This script is to manually change the original data format to long format

library(tidyverse)

# -------------------------------
# (Nsurv)
# -------------------------------
Nsurv <- tibble(
  time = 0:10,
  Control = c(60,59,58,58,57,57,56,56,56,55,54),
  close_pulses = c(70,50,49,49,45,45,45,42,38,37,36),
  wide_pulses = c(70,57,53,52,50,46,45,44,40,38,37),
  constant = c(70,70,69,69,68,66,65,64,60,55,54)
)

# -------------------------------
# (conc)
# -------------------------------
conc <- tibble(
  time = c(0,0.96,1,1.96,2.96,3,3.96,4,4.96,4.97,5.96,6.96,7,7.96,8,9,9.96,10),
  Control = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0),
  close_pulses = c(30.56,27.93,0,0.26,0.21,27.69,26.49,0,0.18,0.18,0.18,0.14,0.14,0,0.18,0,0,0),
  wide_pulses = c(28.98,27.66,0,0.27,0.26,0.26,0.26,0.26,0.25,0.25,0.03,0,26.98,26.28,0,0.12,0.12, 0.12),
  constant = c(4.93,4.69,4.69,4.58,4.58,4.58,4.54,4.54,4.58,4.71,4.71,4.6,4.6,4.59,4.59,4.46,4.51, 4.51)
)

# -------------------------------
#  Wide → Long
# -------------------------------
Nsurv_long <- Nsurv %>%
  pivot_longer(cols = -time, names_to = "replicate", values_to = "Nsurv")

conc_long <- conc %>%
  pivot_longer(cols = -time, names_to = "replicate", values_to = "conc")

# combine
ringtest_b_pulsed <- full_join(Nsurv_long, conc_long, by = c("time","replicate")) %>% mutate(scenario = "pulse")


# rearrange
ringtest_b_pulsed <- ringtest_b_pulsed %>% select(replicate, scenario, conc, time, Nsurv) %>% arrange(time, replicate)

ringtest_b_pulsed <- as.data.frame(ringtest_b_pulsed)
