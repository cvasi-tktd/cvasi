##
## Lemna model fitted to metsulfuron-methyl exposure by Schmitt et al. (2013)
## doi.org/10.1016/j.ecolmodel.2013.01.017
##


## standard algae model parameters taken from file 'mm2.r' included
## in supporting material of Schmitt et al. (2013)
param_study <- list(
  #
  #      - Effect -
  Conc     = 1,   #  [any]      Concentration of toxicant (may also be a table)
  Emax     = .784,    #   maximum Effect
  EC50     = 0.3,     #  [same as conc. data]      Midpoint of effect curve
  b        = 4.16,    #  [-]         Slope of effect curve
  #
  #       - Toxicokinetics -
  P_up     = .0054,   # [cm/d]      Permeability for uptake
  AperBM   = 1000,   # [cm?/g_dw]   A_leaf / d_leaf = 1/d_leaf (for circular disc, d=0.05 cm) [Ref. HARLAN-022]
  Kbm      = .75,   # []          Biomass(fw):water partition coefficient
  P_Temp = F,       # Switch for temperature dependence of cuticle permeability
  MolWeight = 381,  # Molmass of molecule (determines Q10_permeability)



  #       - Fate of biomass -
  k_phot_fix  = F,   #  T/F          If True k_G_max is not changed by environmental factors
  k_phot_max  = 0.47, #  [1/d]       Maximum growth rate of biomass + kmort     [Ref. F 0191, Harlan-011]
  k_resp   = 0.05, #  [1/d]       Rate of mortality                 [Ref. Harlan-011, rough estimate]
  k_loss   = 0.0, #  [1/d]       Some rate of loss (e.g. Flow rate)
  #
  #      - Temperatur dependence -
  # k_phot
  Temp     = 12,   #  [°C]        Current temperature (may also be a table)
  Tmin     = 8.0  , #  [°C]        Minimum growth temperature      [Ref. F 0191, data re-evaluated  incl. kmort(T)]
  Tmax     = 40.5 , #  [°C]        Maximum growth temperature      [Ref. F 0191, data re-evaluated  incl. kmort(T)]
  Topt     = 26.7 , #  [°C]        Optimum growth temperature      [Ref. F 0191, data re-evaluated  incl. kmort(T)]
  # k_resp
  t_ref       = 25,   # temperature at which t_mort is effective
  Q10         = 2,
  #
  #      - Light dependence (linear dependence on global radiation (see Hodgeson 1969)
  Rad 	    = 15000 , #  [kJ/m²/d]  Radiation  (may also be given as table)
  k_0     	= 3 ,     #  [1/d]      Intercept of linear part
  a_k     	= 5E-5 ,  #  [(1/d)/(kJ/m?/d)]        Slope of linear part

  #      - Phosphorus dependence (Hill like dependence) -
  C_P      = 0.3,          #   [mg/L]       Phosporus concentration in water
  CP50     = 0.0043,        #   [mg/L]       P-conc. where growth rate is halfened   [Data from L??nd, 1983 evaluated with monod model]
  a_P      = 1,        #   []            Hill coefficient
  KiP      = 101,         #   [mg/L]       P-inhibition constant for very high P-conc.   [Ref. F 0191]

  #      - Nitrogen dependence (Hill like dependence) -
  C_N      = 0.6,         #   [mg/L]       Nitrogen concentration in water
  CN50     = 0.034,          #   [mg/L]       N-conc. where growth rate is halfened   [Data from L??nd, 1983 evaluated with monod model]
  a_N      = 1,        #   []            Hill coefficient
  KiN      = 604,         #   [mg/L]       n-inhibition constant for very high P-conc.   [Ref. F 0191]

  #      - Density dependence -
  BM50     = 176,    #  [g_dw/m?]             Cut off BM   [Ref. F 0191]

  #      - Others -
  mass_per_frond     = 0.0001,   #  [g_dw/frond]  Dryweight per frond [Ref. HARLAN-022]
  BMw2BMd     = 16.7   #  [g_fresh/g_dry]  Fresh- / dryweight [Ref. F 0191]
)

## extract forcing data from parameters to fit the package's format
forc_temp <- data.frame(t=0,tmp=param_study$Temp)
forc_rad  <- data.frame(t=0,rad=param_study$Rad)
param_study[c("Temp","Rad","Conc")] <- NULL # unset

## simple exposure pattern
## t 0..6  concentration 1 ug/L
## t 7..14 concentration 0 ug/L
exposure <- data.frame(time=0:14, conc=c(rep(1,7),rep(0,8)))

## an exposure time-series that generally recreates the pattern displayed
## in Figure 6 of Schmitt et al. (2013)
#af <- approxfun(x=c(0,80,81,100,140,250,300,365),
#                y=c(0, 0, 1, .3, .1,.01,.001, 0),
#                method="linear", rule=2)
#exposure <- data.frame(t=1:365, c=af(1:365))

## initial values as given in file 'mmc2.r' of Schmitt et al. (2013)
init <- c(
  BM       = 50,     # [g_dw/m?]   Dry Biomass dryweight per m2
  E        = 1,      # (0-1)      (Toxic) Effect = Factor on growth rate  (Range: 0 - 1, 1=no effect)
  M_int    = 0       # [?g]       Amount of toxicant in biomass
)

## create scenario object
Lemna_Schmitt() %>%
  set_tag("metsulfuron") %>%
  set_init(init) %>%
  set_param(param_study) %>%
  set_transfer(interval=-1) %>%
  set_exposure(exposure) %>%
  set_forcings(temp=forc_temp, rad=forc_rad) -> metsulfuron
metsulfuron@exposure@file <- "step function"
usethis::use_data(metsulfuron, overwrite=TRUE)

## testing
#lemna %>% simulate(transfer.interval=1000, method="lsoda")

#lemna %>% set_exposure(data.frame(t=0:7,c=c(1))) -> lemna2
#lemna2 %>% simulate(method="lsoda", hmax=.1)

#seq <- LemnaSequence(n=2)
#scenario(seq, 1) <- lemna %>% set_exposure(data.frame(t=0:7, c=1))
#scenario(seq, 2) <- lemna %>% set_exposure(data.frame(t=7:14,c=0))
#simulate(seq, method="lsoda", hmax=NULL)


#lemna %>% set_exposure(data.frame(t=0:14,c=0)) %>% simulate()

rm(param_study, forc_temp, forc_rad, exposure, init, metsulfuron)
