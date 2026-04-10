# Please run two scripts: 'ringtest_dataB_const.R' and 'ringtest_dataB_pulsed.R' first and have 'ringtest_b_const' and 'ringtest_b_pulsed data frames in your environment. 

# add scenario to ringtest_B_const 
ringtest_b_const <- ringtest_b_const %>%
  mutate(scenario = "const")

# add scenario to ringtest_B_pulsed
ringtest_b_pulsed <- ringtest_b_pulsed %>%
  mutate(scenario = "pulse")

# merge two dataframes
ringtest_b <- bind_rows(ringtest_b_const, ringtest_b_pulsed)

# arrange with time
ringtest_b <- ringtest_b %>%
  arrange(scenario, time, replicate)

# check the result
ringtest_b
