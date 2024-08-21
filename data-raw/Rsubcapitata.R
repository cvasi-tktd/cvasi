# Create Algae_Weber scenario and CalibrationSet rda files
# then run simulation, and compare with digitized simulation of Weber
# eventually, create a test

# simulation with R subcapitata and isoproturon (Fig 33, p. 79 EFSA TKTD opinion)
# note: R. subcapitata in EFSA, but P. subcapitata (older name) in thesis

# ------------------------------------------------------------------------------
# Exposure data
weber_exposure <- read.table(file = "data-raw/Weber_Rsubcapitata_exposureA.txt",
                             header = TRUE,
                             sep = "\t")

# Define environmental forcings # Table D weber suppl.
sim_end <- 43
times <- seq(from = 0, to = sim_end, by = 0.1)
forc_I <- data.frame(times = times, I = 100)
forc_T <- data.frame(times = times, T_act = 24)

# scenario
Rsubcapitata <- Algae_TKTD() %>%
  set_tag("ReactorA") %>%
  # set_init(c(A = 90e4)) %>% # section 8.3.3.3 Weber thesis  # TODO seems very large
  set_param(c(
    # Table C in Weber suppl
    mu_max = 1.7820,  # max growth rate (d-1)
    m_max = 0.500,    # nat mortality (d-1)
    v_max = 0.0620,    # max P uptake (μg P/μg fresh wt/d)
    k_s = 0.0625,      # Half-sat. cnst extracellular P (mg P/L)
    Q_min = 0.0011,    # Min.intracellular P (μg P/μg fresh wt)
    Q_max = 0.0175,    # Max.intracellular P (μg P/μg fresh wt)
    # # Table D weber suppl.
    # R_0 = 0.36,        # Influx conc. of P (mg P/L)
    # D = 0.5,           # Dilution rate (1/d)
    # Table C in Weber suppl
    T_opt = 28,
    T_min = 0,
    T_max = 42,
    I_opt = 120,
    # TKTD opinion Fig 32 (also thesis table 8.4)
    EC_50 = 128,       # Effect conc. 50% inhib. growth rate (ug/L)
    # From thesis (table 8.4)
    b = 1.199,   # slope of concentration effect curve at EC_50 (-)
    # from ???
    # k = 1e-20)) %>%  # assume no degradation
    # k = 0.01650# thesis, p.37 mentions 42d half life. log(0.5)/42 = -0.01650
    kD = 100,
    dose_resp = 0)) %>% # logit
  set_exposure(weber_exposure) %>% # profile of external subst. conc.
  set_forcings(I = forc_I,
               T_act = forc_T) %>%
  set_times(seq(0, sim_end, 1))

usethis::use_data(Rsubcapitata, overwrite=TRUE)

rm(weber_exposure, sim_end, times, forc_I, forc_T)




