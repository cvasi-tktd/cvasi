########################
## Organizing man pages
########################

#' GUTS-RED models
#'
#' Reduced *General Unified Threshold models of Survival* (GUTS) with stochastic
#' death (*SD*) and individual tolerance (*IT*)
#'
#' The TKTD models *[GUTS-RED-SD][GUTS_RED_SD()]* and *[GUTS-RED-IT][GUTS_RED_IT()]*
#' were described by EFSA (2018).
#' GUTS-RED models assume a one-compartment model which directly links
#' external concentration to the scaled damage. The scaled damage is given in
#' units of concentration, equal to the units of measurement in the external
#' medium, e.g. ug/L. The damage dynamics is connected to an individual hazard
#' state variable, resulting in simulated mortality when an internal damage
#' threshold is exceeded. The death mechanisms stochastic death (*SD*) and
#' individual threshold (*IT*) are extreme cases of the *GUTS* theory.
#'
#' For *SD* models, the threshold parameter for lethal effects is fixed and
#' identical for all individuals of a group, meaning that the variance of the
#' threshold values is zero. Hence, the killing rate relates the probability of
#' a mortality event in proportion to the scaled damage. For *IT* models,
#' the thresholds for effects are distributed among individuals of a group.
#' Mortality of an individual follows immediately once the individual's tolerance
#' is exceeded. Meaning in model terms that the killing rate is set to
#' infinity (EFSA 2018).
#'
#' @section State variables:
#' The following list describes the default names and standard units of *GUTS-RED*
#' state variables:
#' * `D`, scaled damage (conc)
#' * `H`, cumulative hazard (-)
#'
#' The state variables are initialized with zero by default.
#'
#' @section SD model parameters:
#' * `kd`, dominant rate constant (time^-1)
#' * `hb`, background hazard rate (time^-1)
#' * `z`, threshold for effects (conc)
#' * `kk`, killing rate constant (time^-1)
#'
#' @section IT model parameters:
#' * `kd`, dominant rate constant (time^-1)
#' * `hb`, background hazard rate (time^-1)
#' * `alpha`, median of thresholds (conc)
#' * `beta`, shape parameter (-)
#'
#' @section Effects:
#' The effect endpoint `L` (lethality) is available for *GUTS-RED* models.
#' A value of zero (`0.0`) denotes *no effect* on organism survival. A value of
#' one (`1.0`) denotes a lethality rate of 100%, i.e. no survivors.
#'
#' The survival probability `S` is available in the return value of [simulate()].
#'
#' @references
#' EFSA PPR Panel (EFSA Panel on Plant Protection Products and their Residues),
#' Ockleford C, Adriaanse P, Berny P, et al., 2018: *Scientific Opinion on the
#' state of the art of Toxicokinetic/Toxicodynamic (TKTD) effect models for
#' regulatory risk assessment of pesticides for aquatic organisms*. EFSA Journal 2018;
#' 16(8):5377, 188 pp. \doi{10.2903/j.efsa.2018.5377}
#'
#' @name GUTS-RED-models
#' @family GUTS-RED models
#' @family models
NULL


########################
## Class definitions
########################

#' @export
setClass("GutsRedSd", contains="EffectScenario")

#' @export
setClass("GutsRedIt", contains="EffectScenario")


########################
## Constructors
########################

#' GUTS-RED-IT scenario
#'
#' Reduced *General Unified Threshold models of Survival* (GUTS) with
#' individual tolerance (*IT*).
#'
#' @inheritSection GUTS-RED-models State variables
#' @inheritSection GUTS-RED-models IT model parameters
#' @inheritSection GUTS-RED-models Effects
#' @section Simulation output:
#' The return value of [simulate()] will contain values for the state variables,
#' as well as an additional column `S` which represents the survival probability
#' for each time point. `S` is calculated as described in EFSA (2018) as
#' *S = (1- F(t))*. The background hazard rate `hb` is already considered in state
#' variable `H` and therefore does not occur as an additional term to derive `S`.
#'
#' @section Solver settings:
#' The arguments to ODE solver [deSolve::ode()] control how model equations
#' are numerically integrated. The settings influence stability of the numerical
#' integration scheme as well as numerical precision of model outputs. Generally, the
#' default settings as defined by *deSolve* are used, but all *deSolve* settings
#' can be modified in *cvasi* workflows by the user, if needed. Please refer
#' to e.g. [simulate()] on how to pass arguments to *deSolve* in *cvasi*
#' workflows.
#'
#' @inherit GUTS-RED-models references
#'
#' @param param optional named `list` or `vector` with model parameters
#' @param init optional named numeric `vector` to use as initial state
#' @return an S4 object of type [GutsRedIt-class]
#'
#' @export
#' @family GUTS-RED models
#' @aliases GutsRedIt-class GUTS-RED-IT
#' @importFrom methods new
GUTS_RED_IT <- function(param, init) {
  new("GutsRedIt",
      name = "GUTS-RED-IT",
      param.req = c("kd", "hb", "alpha", "beta"),
      endpoints = c("L"),
      init = c(D=0, H=0),
      control.req=FALSE
  ) -> o
  if(!missing(param))
    o <- set_param(o, param)
  if(!missing(init))
    o <- set_init(o, init)

  o
}


#' GUTS-RED-SD scenario
#'
#' Reduced *General Unified Threshold models of Survival* (GUTS) with stochastic
#' death (*SD*).
#'
#' @inheritSection GUTS-RED-models State variables
#' @inheritSection GUTS-RED-models SD model parameters
#' @inheritSection GUTS-RED-models Effects
#' @inheritParams GUTS_RED_IT
#' @section Simulation output:
#' The return value of [simulate()] will contain values for the state variables,
#' as well as an additional column `S` which represents the survival probability
#' for each time point. `S` is calculated as described in EFSA (2018) as
#' *S = exp(-H)*. The background hazard rate `hb` is already considered in state
#' variable `H` and therefore does not occur as an additional term to derive `S`.
#'
#' @inheritSection GUTS_RED_IT Solver settings
#' @inherit GUTS-RED-models references
#' @return an S4 object of type [GutsRedSd-class]
#'
#' @export
#' @family GUTS-RED models
#' @aliases GutsRedSd-class GUTS-RED-SD
#' @importFrom methods new
GUTS_RED_SD <- function(param, init) {
  new("GutsRedSd",
      name = "GUTS-RED-SD",
      param.req = c("kd", "hb", "z", "kk"),
      endpoints = c("L"),
      init = c(D=0, H=0),
      control.req=FALSE
  ) -> o
  if(!missing(param))
    o <- set_param(o, param)
  if(!missing(init))
    o <- set_init(o, init)

  o
}


########################
## Simulation
########################

# @param scenario Scenario object
# @param hmax numeric, maximum step length in time, see [deSolve::ode()]
# @param method string, numerical solver used by [deSolve::ode()]
# @param ... additional arguments passed to [deSolve::ode()]
#' @importFrom deSolve ode
solver_gutsredsd <- function(scenario, method="lsoda", hmax=1, ...) {
  params <- scenario@param
  if(is.list(params))
    params <- unlist(params)

  # check if all required parameters are present
  params_missing <- is.na(params[scenario@param.req])
  if(any(params_missing)) {
    stop("Missing parameters: ", paste(scenario@param.req[params_missing], collapse=", "))
  }

  # make sure that parameters are present and in required order
  params <- params[c("kd", "hb", "z", "kk")]

  df <- ode2df(ode(y=scenario@init, times=scenario@times, parms=params, dllname="cvasi",
                    initfunc="gutsredsd_init", func="gutsredsd_func", initforc="gutsredsd_forc",
                    forcings=scenario@exposure@series, outnames=c("Cw"),
                    method=method, hmax=hmax, ...))
  # Derive survival probability, see EFSA Scientific Opinion on TKTD models, p. 33
  # doi:10.2903/j.efsa.2018.5377
  df$S <- exp(-df$H) # background hazard rate included in H (if enabled)
  df
}
#' @include solver.R
#' @describeIn solver Numerically integrates GUTS-RED-SD models
setMethod("solver", "GutsRedSd", function(scenario, ...) solver_gutsredsd(scenario, ...))

# @param scenario Scenario object
# @param method string, numerical solver used by [deSolve::ode()]
# @param hmax numeric, maximum step length in time, see [deSolve::ode()]
# @param ... additional arguments passed to [deSolve::ode()]
#' @importFrom deSolve ode
solver_gutsredit <- function(scenario, method="lsoda", hmax=1, ...) {
  params <- scenario@param
  if(is.list(params))
    params <- unlist(params)

  # check if all required parameters are present
  params_missing <- is.na(params[scenario@param.req])
  if(any(params_missing)) {
    stop("Missing parameters: ", paste(scenario@param.req[params_missing], collapse=", "))
  }

  # make sure that parameters are present and in required order
  odeparams <- params[c("kd","hb")]

  # F := cumulative maximum of D(t)
  F_init <- scenario@init[["D"]]
  df <- ode2df(ode(y=c(scenario@init, "F"=F_init), times=scenario@times, parms=odeparams, dllname="cvasi",
                initfunc="gutsredit_init", func="gutsredit_func", initforc="gutsredit_forc",
                forcings=scenario@exposure@series, outnames=c("Cw"),
                method=method, hmax=hmax, ...))

  # Derive survival probability, EFSA Scientific Opinion on TKTD models, p. 33
  # doi:10.2903/j.efsa.2018.5377; column `F` approximates `cummax(df$D)`
  FS <- (1 / (1 + (df$F / params["alpha"])^(-params["beta"])))
  df$S <- (1 - FS) * exp(-df$H)
  df$F <- NULL
  df
}
#' @describeIn solver Numerically integrates GUTS-RED-IT models
setMethod("solver", "GutsRedIt", function(scenario, ...) solver_gutsredit(scenario, ...) )


########################
## Effects
########################

# Calculate effect of GUTS-RED-SD scenario
fx_gutsredsd <- function(scenario, ...) {
  # we save the control run if we just set the background mortality to zero
  # as it would cancel out, anyways
  if(scenario@param$hb > 0)
    scenario@param$hb <- 0

  res <- simulate(scenario, ...)
  c("L"=1 - tail(res$S, n=1))
}

# Calculate effect of GUTS-RED-IT scenario
fx_gutsredit <- function(scenario, ...) {
  # we avoid the control run if we just set the background mortality to zero
  # as it would cancel out anyways
  if(scenario@param$hb > 0) {
    scenario <- set_param(scenario, c(hb=0))
  }

  res <- simulate(scenario, ...)
  c("L"=1 - tail(res$S, n=1))
}

#' @include fx.R
#' @describeIn fx Calculates lethality of [GUTS-RED-SD] scenarios
setMethod("fx", "GutsRedSd", function(scenario, ...) fx_gutsredsd(scenario, ...))
#' @describeIn fx Calculates lethality of [GUTS-RED-IT] scenarios
setMethod("fx", "GutsRedIt", function(scenario, ...) fx_gutsredit(scenario, ...))
