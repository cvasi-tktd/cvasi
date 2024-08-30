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
#' In addition, the surival rate over time can be derived for all output time
#' points using [survival()].
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
#' @family scenarios
NULL

#' @export
setClass("GutsRedSd", contains="EffectScenario")

#' @export
setClass("GutsRedIt", contains="EffectScenario")


#' GUTS-RED-IT scenario
#'
#' Reduced *General Unified Threshold models of Survival* (GUTS) with
#' individual tolerance (*IT*).
#'
#' @inheritSection GUTS-RED-models State variables
#' @inheritSection GUTS-RED-models IT model parameters
#' @inheritSection GUTS-RED-models Effects
#' @inherit GUTS-RED-models references
#'
#' @param param optional named `list` or `vector` with model parameters
#' @param init optional named numeric `vector` to use as initial state
#' @return an S4 object of type [GutsRedIt-class]
#'
#' @export
#' @family GUTS-RED models
#' @aliases GutsRedIt-class
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
#' @inherit GUTS-RED-models references
#' @inheritParams GUTS_RED_IT
#' @return an S4 object of type [GutsRedSd-class]
#'
#' @export
#' @family GUTS-RED models
#' @aliases GutsRedSd-class
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
