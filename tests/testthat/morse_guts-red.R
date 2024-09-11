#
# The code below contains an alternative model implementation of the GUTS-RED-IT
# and GUTS-RED-SD models which was published with the `morse` package, version
# 3.2.7 and before. The `morse` implementation is used as a means to compare model
# results numerically.
#


# wrapper functions extract LPx values from morse's return values in a format
# that is compatible with this package
morse_sd <- function(scenario) {
  param <- scenario@param
  exp <- scenario@exposure@series

  SurvSD_ode(
    Cw=exp[, 2],
    time=exp[, 1],
    kk=param[["kk"]],
    kd=param[["kd"]],
    z=param[["z"]],
    hb=param[["hb"]]
  ) %>%
    dplyr::select(time, D, H, S=dtheo)
}

morse_it <- function(scenario) {
  param <- scenario@param
  exp <- scenario@exposure@series

  SurvIT_ode(
    Cw=exp[, 2],
    time=exp[, 1],
    kd=param[["kd"]],
    hb=param[["hb"]],
    alpha=param[["alpha"]],
    beta=param[["beta"]]
  ) %>%
    dplyr::mutate(H=0) %>%
    dplyr::select(time, D, H, S=dtheo)
}

# Code below taken from morse package, version 3.2.7 (2020-11-11)
# https://github.com/pveber/morse/blob/c1637ee7811d4369942d9ac479fa681285f0ae5f/R/predict_ode.survFit.R
#
# Some parts have been commented out because they are not needed for our
# use case.
SurvSD_ode <- function(Cw, time, replicate=NULL, kk, kd, z, hb, mcmc_size = 1, interpolate_length = NULL, interpolate_method = "linear"){

  ## external signal with several rectangle impulses
  signal <- data.frame(times = time,
                       import = Cw)

  sigimp <- stats::approxfun(signal$times, signal$import, method = interpolate_method, rule = 2)

  if(!is.null(interpolate_length)){
    times <- seq(min(time), max(time), length = interpolate_length)
  } else{
    times <- signal$times
  }

  ## The parameters
  parms  <- list( kd = kd,
                  kk = kk,
                  z = z,
                  hb = hb,
                  mcmc_size = mcmc_size)

  ## Start values for steady state
  xstart <- c(D = rep(0, mcmc_size),
              H = rep(0, mcmc_size))

  ## Solve model
  out <- ode(y = xstart,
             times = times,
             func = model_SD,
             parms,
             input = sigimp)

  dtheo <- exp(- out[, grep("H", colnames(out))] )

  # Manage vector case
  #if(mcmc_size == 1){
  #  q50 = dtheo
  #  qinf95 = dtheo
  #  qsup95 = dtheo
  #} else{
  #  q50 = apply(dtheo, 1, quantile, probs = 0.5, na.rm = TRUE)
  #  qinf95 = apply(dtheo, 1, quantile, probs = 0.025, na.rm = TRUE)
  #  qsup95 = apply(dtheo, 1, quantile, probs = 0.975, na.rm = TRUE)
  #}

  #dtheo <- as.data.frame(dtheo) %>%
  #  mutate(time = out[, "time"],
  #         conc = out[, "signal"]
  #         replicate = rep(replicate, nrow(out)),
  #         q50 = q50,
  #         qinf95 = qinf95,
  #         qsup95 = qsup95
  #         )

  out <- as.data.frame(out)
  out$dtheo <- dtheo

  return(out)
}

model_SD <- function(t, State, parms, input)  {
  with(as.list(c(parms, State)), {

    conc_ext = input(t)

    D = State[1:mcmc_size]
    H = State[(mcmc_size+1):(2*mcmc_size)]

    dD <- kd * (conc_ext - D)     # internal concentration
    dH <- kk * pmax(D - z, 0) + hb # risk function

    res <- c(dD, dH)
    list(res, signal = conc_ext)
  })
}


# Survival function for "IT" model with external concentration changing with time
#
# @param Cw A scalar of external concentration
# @param time A vector of time
# @param kk a vector of parameter
# @param kd a vector of parameter
# @param z a vector of parameter
# @param hb a vector of parameter
#
#
# @return A matrix generate with coda.samples() function
#

SurvIT_ode <- function(Cw, time, replicate=NULL, kd, hb, alpha, beta, mcmc_size = 1, interpolate_length = NULL, interpolate_method = "linear"){

  ## external signal with several rectangle impulses
  signal <- data.frame(times = time,
                       import = Cw)

  sigimp <- stats::approxfun(signal$times, signal$import, method = interpolate_method, rule = 2)

  if(!is.null(interpolate_length)){
    times <- seq(min(time), max(time), length = interpolate_length)
  } else{
    times <- signal$times
  }

  ## The parameters
  parms  <- list( kd = kd,
                  alpha = alpha,
                  beta = beta,
                  mcmc_size = mcmc_size)

  ## Start values for steady state
  xstart <- c(D = rep(0, mcmc_size))

  ## Solve model
  out <- ode(y = xstart,
             times = times,
             func = model_IT,
             parms,
             input = sigimp)

  D <- out[, grep("D", colnames(out))]
  cumMax_D <- if(is.null(dim(D))) cummax(D) else apply(D, 2, cummax)
  thresholdIT <- t(1 / (1 + (t(cumMax_D) / parms$alpha)^(-parms$beta)))

  dtheo <- (1 - thresholdIT) * exp(times %*% t(-hb))

  # Manage vector case
  #if(mcmc_size == 1){
  #  q50 = dtheo
  #  qinf95 = dtheo
  #  qsup95 = dtheo
  #} else{
  #  q50 = apply(dtheo, 1, quantile, probs = 0.5, na.rm = TRUE)
  #  qinf95 = apply(dtheo, 1, quantile, probs = 0.025, na.rm = TRUE)
  #  qsup95 = apply(dtheo, 1, quantile, probs = 0.975, na.rm = TRUE)
  #}

  #dtheo <- as.data.frame(dtheo) %>%
  #  mutate(time = out[, "time"],
  #         conc = out[, "signal"]
  #         replicate = rep(replicate, nrow(out)),
  #         q50 = q50,
  #         qinf95 = qinf95,
  #         qsup95 = qsup95
  #         )

  out <- as.data.frame(out)
  out$dtheo <- as.numeric(dtheo)

  return(out)
}

model_IT <- function(t, State, parms, input) {
  with(as.list(c(parms, State)), {
    conc_ext <- input(t)

    D = State[1:mcmc_size]

    dD <- kd*(conc_ext - D)    # internal damage

    list(dD = dD, signal = conc_ext)
  })
}
