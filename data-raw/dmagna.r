
#
# model parameters
param <- list(
  L0 = 0.9676,
  Lp = 2.42,
  Lm = 4.614,
  rB = 0.1106,
  Rm = 16.69,
  f = 1,
  hb = 0.001608,
  Lf = 0,
  Lj = 0,
  Tlag = 0,
  kd = 2.060,
  zb = 5.201,
  bb = 0.04422,
  zs = 11.75,
  bs = 0.1759,
  FBV    = 0.02,
  KRV    = 1,
  kap    = 0.8,
  yP     = 0.8*0.8,
  Lm_ref = 4,
  len    = 2,
  Tbp    = 3,
  MoA = 1,
  FB = 0
)

#
# initial state
init <- c(D=0, L=param$L0, R=0, S=1)

#
# exposure
exposure <- data.frame(time=c(0, 0.99, 1, 6.99, 7, 7.99, 8, 21),
                       conc=c(16.7, 15.9, 0, 0, 16.1, 14.9, 0, 0))

#
# output times
times <- cumsum(c(0, rep(21.42/99,99)))

DEB_Daphnia() %>%
  set_tag("dmagna") %>%
  set_init(init) %>%
  set_param(param) %>%
  set_times(times) %>%
  set_exposure(exposure, reset_times = FALSE) -> dmagna
usethis::use_data(dmagna, overwrite=TRUE)

rm(param, init, exposure, times, dmagna)
