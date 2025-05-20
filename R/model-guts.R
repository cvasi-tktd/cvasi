
########################
## Class definitions
########################
#' @export
setClass("Guts", contains="EffectScenario",
         slots=list("dose_metric" = "character", "scaled_ci" = "logical"),
         prototype=list("dose_metric" = "D", "scaled_ci" = FALSE)
)

#' @export
setClass("GutsSd", contains="Guts")

#' @export
setClass("GutsIt", contains="Guts")


########################
## Constructors
########################

#' GUTS-SD scenario
#'
#' Full *General Unified Threshold models of Survival* (GUTS) with stochastic
#' death (*SD*). The model was defined by Jager et al. (2011). It is
#' compatible with the *Full GUTS* model as described by EFSA (2018), but
#' some parameter names may differ.
#'
#' @section State variables:
#' The following list describes the default names and standard units of *GUTS*
#' state variables:
#' * `Ci`, (scaled) internal concentration (conc)
#' * `D`, (scaled) damage (*)
#' * `H`, cumulative hazard (-)
#'
#' The state variables are initialized with zero by default.
#'
#' @section SD model parameters:
#' The set of parameters and their names follows the definition by Jager et al
#' (2011). The actual number of required parameters depends on the selected model
#' variant, i.e. if the internal concentration is scaled or not, as well as the
#' selected dose metric. The full set of parameters is as follows
#'
#' * `ki`, accumulation rate into body (time^-1)
#' * `ke`, elimination rate (time^-1)
#' * `Kiw`, scaling constant for external concentration (*)
#' * `kr`, damage recovery rate (time^-1)
#' * `kk`, killing rate constant (time^-1)
#' * `hb`, background hazard rate (time^-1)
#' * `z`, threshold for effects (*)
#'
#'
#' @section Effects:
#' The effect endpoint `L` (lethality) is available for *GUTS* models.
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
#' Jager T., Albert C., Preuss T.G., and Ashauer R., 2011: General Unified Threshold
#' Model of Survival - a Toxicokinetic-Toxicodynamic Framework for Ecotoxicology.
#' Environ. Sci. Technol. 45(7), pp. 2529-2540. \doi{10.1021/es103092a}
#'
#'
#' @param scaled_ci `logical`, if `TRUE`, the model will use the scaled internal concentration
#'   `Ci*`. Otherwise the internal concentration `Ci` is not scaled. Default value
#'    is `FALSE`.
#' @param dose_metric `character`, set to `'D'` to use scaled damage, `'Ci'` to
#'   use internal concentration, or `'Cw'` to use the external concentration as dose
#'   metric. Default value is `'D'`.
#' @return an S4 object of type `GutsSd-class`
#' @export
#' @family GUTS models
#' @aliases GutsSd-class GUTS-SD
#' @seealso [GUTS-RED-SD]
#' @importFrom methods new
GUTS_SD <- function(scaled_ci=FALSE, dose_metric=c("D", "Ci", "Cw")) {
  # Check arguments
  if(!is.logical(scaled_ci)) {
    stop("Argument `scaled_ci` must be a logical value")
  }
  dose_metric <- match.arg(dose_metric)
  # Plausibility check
  if(scaled_ci & dose_metric == "Cw") {
    stop("Internal concentration `Ci` cannot be scaled, if dose metric M=Cw")
  }

  init <- c(Ci=NA_real_, D=NA_real_, H=0)
  preq <- c("kk", "hb", "z")

  # Parameters required for (scaled) damage ODE
  if(dose_metric == "D") {
    preq <- c("kr", preq)
    init["D"] <- 0
  }

  # Parameters required for internal concentration ODE
  if(scaled_ci) { # scaled Ci
    preq <- c("ke", "Kiw", preq)
    init["Ci"] <- 0
  } else if(dose_metric != "Cw") { # Any other case, unless Ci disabled
    preq <- c("ki", "ke", preq)
    init["Ci"] <- 0
  }

  param <- as.list(setNames(rep(NA_real_, length(preq)), preq))
  if(scaled_ci) {
    param["Kiw"] <- 1
  }

  new("GutsSd",
      name = paste0("GUTS-SD (", ifelse(scaled_ci, "scaled Ci, ", ""), "M=", dose_metric, ")"),
      scaled_ci = scaled_ci,
      dose_metric = dose_metric,
      param.req = preq,
      param = param,
      endpoints = c("L"),
      init = init,
      control.req=FALSE
  )
}

# TODO currently only implements exponentially distributed individual thresholds z
#   but other types such as log-normal or log-logistic may be necessary,
#   cf Jager et al (2011), Table 3
#' GUTS-IT scenario
#'
#' Full *General Unified Threshold models of Survival* (GUTS) with Individual Tolerance
#  Distribution (*IT*). The model was defined by Jager et al. (2011). It is
#' compatible with the *Full GUTS* model as described by EFSA (2018), but
#' some parameter names may differ.
#'
#' @section State variables:
#' The following list describes the default names and standard units of *GUTS*
#' state variables:
#' * `Ci`, (scaled) internal concentration (conc)
#' * `D`, (scaled) damage (*)
#' * `H`, cumulative hazard (-)
#'
#' The state variables are initialized with zero by default.
#'
#' @section IT model parameters:
#' The set of parameters and their names follows the definition by Jager et al
#' (2011). The actual number of required parameters depends on the selected model
#' variant, i.e. if the internal concentration is scaled or not, as well as the
#' selected dose metric. The full set of parameters is as follows
#'
#' * `ki`, accumulation rate into body (time^-1)
#' * `ke`, elimination rate (time^-1)
#' * `Kiw`, scaling constant for external concentration (*)
#' * `kr`, damage recovery rate (time^-1)
#' * `hb`, background hazard rate (time^-1)
#' * `alpha`, median of thresholds (conc)
#' * `beta`, shape parameter (-)
#'
#' @section Effects:
#' The effect endpoint `L` (lethality) is available for *GUTS* models.
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
#' Jager T., Albert C., Preuss T.G., and Ashauer R., 2011: General Unified Threshold
#' Model of Survival - a Toxicokinetic-Toxicodynamic Framework for Ecotoxicology.
#' Environ. Sci. Technol. 45(7), pp. 2529-2540. \doi{10.1021/es103092a}
#'
#'
#' @param scaled_ci `logical`, if `TRUE`, the model will use the scaled internal concentration
#'   `Ci*`. Otherwise the internal concentration `Ci` is not scaled. Default value
#'    is `FALSE`.
#' @param dose_metric `character`, set to `'D'` to use scaled damage, `'Ci'` to
#'   use internal concentration, or `'Cw'` to use the external concentration as dose
#'   metric. Default value is `'D'`.
#' @return an S4 object of type `GutsIt-class`
# @export
#' @family GUTS models
#' @aliases GutsIT-class GUTS-IT
#' @seealso [GUTS-RED-IT]
#' @importFrom methods new
GUTS_IT <- function(scaled_ci=FALSE, dose_metric=c("D", "Ci", "Cw")) {
  # Check arguments
  if(!is.logical(scaled_ci)) {
    stop("Argument `scaled_ci` must be a logical value")
  }
  dose_metric <- match.arg(dose_metric)
  # Plausibility check
  if(scaled_ci & dose_metric == "Cw") {
    stop("Internal concentration `Ci` cannot be scaled, if dose metric M=Cw")
  }

  init <- c(Ci=NA_real_, D=NA_real_, H=0)
  preq <- c("hb", "alpha", "beta")

  # Parameters required for (scaled) damage ODE
  if(dose_metric == "D") {
    preq <- c("kr", preq)
    init["Ci"] <- 0
    init["D"] <- 0
  }

  # Parameters required for internal concentration ODE
  if(scaled_ci) { # scaled Ci
    preq <- c("ke", "Kiw", preq)
    init["Ci"] <- 0
  } else if(dose_metric != "Cw") { # Any other case, unless Ci disabled
    preq <- c("ki", "ke", preq)
    init["Ci"] <- 0
  }

  param <- as.list(setNames(rep(NA_real_, length(preq)), preq))
  if(scaled_ci) {
    param["Kiw"] <- 1
  }

  new("GutsIt",
      name = paste0("GUTS-IT (", ifelse(scaled_ci, "scaled Ci, ", ""), "M=", dose_metric, ")"),
      scaled_ci = scaled_ci,
      dose_metric = dose_metric,
      param.req = preq,
      param = param,
      endpoints = c("L"),
      init = init,
      control.req=FALSE
  )
}

########################
## Simulation
########################

# @param scenario Scenario object
# @param hmax numeric, maximum step length in time, see [deSolve::ode()]
# @param method string, numerical solver used by [deSolve::ode()]
# @param ... additional arguments passed to [deSolve::ode()]
#' @importFrom deSolve ode
solver_gutssd <- function(scenario, method="ode45", hmax=0.1, ...) {
  params <- scenario@param
  if(is.list(params))
    params <- unlist(params)
  init <- scenario@init

  # Convert parameters for scaled Ci, if necessary
  if(scenario@scaled_ci) {
    params["ki"] = params["ke"] * params["Kiw"]
  }

  # Set dose_metric switch value
  if(scenario@dose_metric == "D") {
    dm_val <- 0
  } else if(scenario@dose_metric == "Ci") {
    dm_val <- 1
    init[["D"]] <- NA_real_
  } else if(scenario@dose_metric == "Cw") {
    dm_val <- 2
    init[["Ci"]] <- NA_real_
    init[["D"]] <- NA_real_
  } else {
    stop("Slot `dose_metric` has unsupported value")
  }
  params["dose_metric"] <- dm_val

  # check if all required parameters are present
  params_missing <- is.na(params[scenario@param.req])
  if(any(params_missing)) {
    stop("Missing parameters: ", paste(scenario@param.req[params_missing], collapse=", "))
  }

  # make sure that parameters are present and in required order
  param_order <- c("ki", "ke", "kr", "kk", "hb", "z", "dose_metric")
  # not all parameters expected by the model's C code may be present, make sure
  # that all parameters are present and initialized with `NA`
  params <- setNames(params[param_order], param_order)

  df <- ode2df(ode(y=init, times=scenario@times, parms=params, dllname="cvasi",
                   initfunc="gutssd_init", func="gutssd_func", initforc="gutssd_forc",
                   forcings=scenario@exposure@series, outnames=c("Cw", "M"),
                   method=method, hmax=hmax, ...))

  # Derive survival probability, refs:
  # Jager et al (2011) DOI: 10.1021/es103092a
  # EFSA Scientific Opinion, p. 33, DOI: 10.2903/j.efsa.2018.5377
  df$S <- exp(-df$H) # background hazard rate `hb` included in state variable `H` (if enabled)
  df
}

solver_gutsit <- function(scenario, method="ode45", hmax=0.1, nout=0, ...) {
  params <- scenario@param
  if(is.list(params))
    params <- unlist(params)
  init <- scenario@init

  # Convert parameters for scaled Ci, if necessary
  if(scenario@scaled_ci) {
    params["ki"] = params["ke"] * params["Kiw"]
  }

  # Set dose_metric switch value
  if(scenario@dose_metric == "D") {
    dm_val <- 0
    init[["CMax"]] <- init[["D"]]
  } else if(scenario@dose_metric == "Ci") {
    dm_val <- 1
    init[["CMax"]] <- init[["Ci"]]
    init[["D"]] <- NA_real_
  } else if(scenario@dose_metric == "Cw") {
    dm_val <- 2
    init[["CMax"]] <- 0
    init[["Ci"]] <- NA_real_
    init[["D"]] <- NA_real_
    # activate Cw as additional output of the solver
    nout <- max(nout, 1)
  } else {
    stop("Slot `dose_metric` has unsupported value")
  }
  params["dose_metric"] <- dm_val

  # check if all required parameters are present
  params_missing <- is.na(params[scenario@param.req])
  if(any(params_missing)) {
    stop("Missing parameters: ", paste(scenario@param.req[params_missing], collapse=", "))
  }

  # make sure that parameters are present and in required order
  param_order <- c("ki", "ke", "kr", "hb", "dose_metric")
  # not all parameters expected by the model's C code may be present, make sure
  # that all parameters are present and initialized with `NA`
  params_ode <- setNames(params[param_order], param_order)

  df <- ode2df(ode(y=init, times=scenario@times, parms=params_ode, dllname="cvasi",
                   initfunc="gutsit_init", func="gutsit_func", initforc="gutsit_forc",
                   forcings=scenario@exposure@series, outnames=c("Cw", "M"), nout=nout,
                   method=method, hmax=hmax, ...))

  # `CMax` approximates `cummax(M)` but may underestimate its value in case of
  # M = Cw, because Cw has no known derivative. Therefor we apply a *parallel*
  # maximum on `M` and `CMax`. However, if there too few output time points this
  # might not improve the result.
  if(scenario@dose_metric == "Cw") {
    df$CMax <- pmax(df$CMax, df$Cw)
  }
  # None of the dose metrics can, by definition, become negative. However, the
  # numerical schemes may sometimes return negative values due to interpolation.
  df$CMax <- pmax(0, df$CMax)

  # Derive survival probability, refs:
  # Jager et al (2011) DOI: 10.1021/es103092a
  # EFSA Scientific Opinion, p. 33, DOI: 10.2903/j.efsa.2018.5377
  FS <- (1 / (1 + (df$CMax / params[["alpha"]])^(-params[["beta"]])))
  df$S <- (1 - FS) * exp(-df$H)
  df$CMax <- NULL
  df
}

#' @include solver.R
#' @describeIn solver Numerically integrates GUTS-SD models
setMethod("solver", "GutsSd", function(scenario, ...) solver_gutssd(scenario, ...))

#' @include solver.R
#' @describeIn solver Numerically integrates GUTS-IT models
setMethod("solver", "GutsIt", function(scenario, ...) solver_gutsit(scenario, ...))

#' @include fx.R
#' @describeIn fx Calculates lethality of [GUTS-SD] scenarios
setMethod("fx", "GutsSd", function(scenario, ...) fx_gutsredsd(scenario, ...))

#' @include fx.R
#' @describeIn fx Calculates lethality of [GUTS-IT] scenarios
setMethod("fx", "GutsIt", function(scenario, ...) fx_gutsredit(scenario, ...))
