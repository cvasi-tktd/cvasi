########################
## Organizing man pages
########################

#' Myriophyllum models
#'
#' Supported models:
#' * [Myrio()], with exponential growth
#' * [Myrio_log()], with logistic growth
#'
#' @name Myriophyllum-models
#' @seealso [Lemna-models], [Transferable]
#' @family Myriophyllum models
#' @family models
#' @aliases Myriophyllum-class
NULL


########################
## Class definitions
########################

#' @include class-Transferable.R
#' @export
setClass("Myriophyllum", contains=c("Transferable","EffectScenario"))

# Myriophyllum model with exp. growth
#' @export
setClass("MyrioExp", contains="Myriophyllum")
# for backwards compatibility
#' @export
setClass("MyrioExpScenario", contains="MyrioExp")

# Myriophyllum model with log. growth
#' @export
setClass("MyrioLog", contains="Myriophyllum")
# for backwards compatibility
#' @export
setClass("MyrioLogScenario", contains="MyrioLog")


########################
## Constructors
########################

#' Myriophyllum model with exponential growth
#'
#' The *Myriophyllum* model is derived from the *Lemna* TKTD model by
#' Klein *et al.* (2021). The Myriophyllum model is mathematically equivalent
#' to the Tier 2C version of the *Lemna* model by Klein *et al.* (2021),
#' cf. [Lemna_SETAC()]. Recommended settings for Tier 2C are `k_photo_fixed=TRUE`
#' and `k_resp=0` (Klein *et al.* 2021).
#' In particular, the growth model is a simple exponential growth model,
#' which is considered to be the typical situation for a laboratory macrophyte
#' study. Instead of frond numbers as for *Lemna*, the biomass is also returned as
#' total shoot length (`TSL`) in simulation results.
#' Consequently, the model has the additional parameter `r_DW_TSL`
#' (dry weight per total shoot length ratio) instead of `r_DW_FN` (dry weight
#' per frond number ratio).
#'
#' @section State variables:
#' The model has two state variables:
#' - `BM`, Biomass (g dw m-2 for field studies or mg dw for lab)
#' - `M_int`, Mass of toxicant in plant population (mass per m2, e.g. ug m-2)
#'
#' @section Model parameters:
#' - Growth model
#'   - `k_photo_max`, Maximum photosynthesis rate (d-1), default: `0.47`
#'
#' - Concentration response (Toxicodynamics)
#'   - `EC50_int`, Internal concentration resulting in 50% effect (ug L-1)
#'   - `E_max`, Maximum inhibition (-), default: `1`
#'   - `b`, Slope parameter (-)
#'
#' - Internal concentration (Toxicokinetics)
#'   - `P`, Permeability (cm d-1)
#'   - `r_A_DW`, Area per dry-weight ratio (cm2 g-1), default: `1000`
#'   - `r_FW_DW`, Fresh weight per dry weight ratio (-), default: `16.7`
#'   - `r_FW_V`, Fresh weight density (g cm-3), default: `1`
#'   - `r_DW_TSL`, Dry weight per total shoot length ratio  (g (field) or mg (lab) dw cm-1)
#'   - `K_pw`, Partitioning coefficient plant:water (-), default: `1`
#'   - `k_met`, Metabolisation rate (d-1), default: `0`
#'
#' @section Environmental factors:
#'
#' None.
#'
#' @section Parameter boundaries:
#' Default values for parameter boundaries are set for all parameters by expert
#' judgement, for calibration purposes. Values can be modified using [set_bounds()].
#'
#' @section Simulation output:
#' Simulation results will contain the state variables
#' It is possible to amend the output of [simulate()] with additional model
#' quantities that are not state variables, for e.g. debugging purposes or to
#' analyze model behavior. To enable or disable additional outputs, use the
#' optional argument `nout` of [simulate()]. As an example, set `nout=2` to
#' enable reporting of the acceleration factor (`MV`) and the mobilization flux
#' (`pC`). Set `nout=0` to disable additional outputs (default).
#'
#' The available output levels are as follows:
#' - `nout` >= 1: `C_int`, internal concentration (mass per volume)
#' - `nout` >= 2: `TSL`, total shoot length (?)
#' - `nout` >= 3: `f_photo`, photosynthesis dependency function (-)
#' - Growth and TK/TD
#'   - `nout` >= 4: `C_int_unb`, unbound internal concentration (mass per volume)
#'   - `nout` >= 5: `C_ext`, external concentration (mass per volume)
#' - Derivatives
#'   - `nout` >= 6: `dBM`, biomass derivative (g dw m-2 d-1)
#'   - `nout` >= 7: `dM_int`, mass of toxicant in plants derivative (mass per m2 d-1)
#'
#' @inheritSection Lemna_SETAC Effects
#' @inheritSection Transferable Biomass transfer
#' @references
#' Klein J., Cedergreen N., Heine S., Reichenberger S., Rendal C.,
#' Schmitt W., Hommen U., 2021: *Refined description of the Lemna TKTD growth model
#' based on Schmitt et al. (2013) - equation system and default parameters*.
#' Report of the working group *Lemna* of the SETAC Europe Interest Group Effect
#' Modeling. Version 1, uploaded on 22. Sept. 2021.
#' https://www.setac.org/group/effect-modeling.html
#'
#' @return an S4 object of type [MyrioExpScenario-class]
#' @seealso [Macrophyte-models], [Transferable], [Scenarios]
#' @family Myriophyllum models
#' @family macrophyte models
#' @aliases MyrioExp-class MyrioExpScenario-class
#' @export
Myrio <- function() {
  new("MyrioExp",
      name="Myriophyllum",
      param.req=c("k_photo_max", "E_max", "EC50_int", "b", "P", "r_A_DW",
                  "r_FW_DW", "r_FW_V", "r_DW_TSL", "K_pw", "k_met"),
      # default values as defined by Klein et al. (2021)
      param=list(k_photo_max=0.47, E_max=1,
                 r_A_DW=1000, r_FW_DW=16.7, r_FW_V=1,
                 K_pw=1, k_met=0),
      # boundary presets defined by expert judgement
      param.bounds=list(k_photo_max=c(0, 1), E_max=c(0, 1), EC50_int=c(0, 1e6),
                        b=c(0.1, 20), P=c(0, 100)),
      endpoints=c("BM", "r"),
      control.req=TRUE,
      init=c(BM=0, M_int=0),
      transfer.comp.biomass="BM",
      transfer.comp.scaled="M_int"
  )
}

#' Myriophyllum model with logistic growth
#'
#' The *Myriophyllum* model is derived from the *Lemna* TKTD model
#' by Klein *et al.* (2021).
#' `Myrio_log()` modifies the [Myrio()] model to feature logistic growth, i.e.
#' control growth is described by the differential equation
#' `d BM/dt = k_photo_max*BM*(1 - BM/BM_L)` where `BM_L` is the carrying capacity.
#'
#' @inheritSection Myrio State variables
#' @section Model parameters:
#' - Growth model
#'   - `k_photo_max`, Maximum photosynthesis rate (d-1), default: `0.47`
#'   - `BM_L`, Carrying capacity (g dw m-2)
#'
#' - Concentration response (Toxicodynamics)
#'   - `EC50_int`, Internal concentration resulting in 50% effect (ug L-1)
#'   - `E_max`, Maximum inhibition (-), default: `1`
#'   - `b`, Slope parameter (-)
#'
#' - Internal concentration (Toxicokinetics)
#'   - `P`, Permeability (cm d-1)
#'   - `r_A_DW`, Area per dry-weight ratio (cm2 g-1), default: `1000`
#'   - `r_FW_DW`, Fresh weight per dry weight ratio (-), default: `16.7`
#'   - `r_FW_V`, Fresh weight density (g cm-3), default: `1`
#'   - `r_DW_TSL`, Dry weight per total shoot length ratio  (?)
#'   - `K_pw`, Partitioning coefficient plant:water (-), default: `1`
#'   - `k_met`, Metabolisation rate (d-1), default: `0`
#'
#' @inheritSection Myrio Environmental factors
#' @inheritSection Myrio Simulation output
#' @inheritSection Myrio Effects
#' @inheritSection Transferable Biomass transfer
#' @inheritSection Myrio Parameter boundaries
#' @inherit Myrio references
#'
#' @return an S4 object of type [MyrioLogScenario-class]
#' @seealso [Transferable], [Scenarios]
#' @family Myriophyllum models
#' @family macrophyte models
#' @aliases MyrioLog-class MyrioLogScenario-class
#' @export
Myrio_log <- function() {
  new("MyrioLog",
      name="Myriophyllum",
      param.req=c("k_photo_max", "BM_L", "E_max", "EC50_int", "b", "P",
                  "r_A_DW", "r_FW_DW", "r_FW_V", "r_DW_TSL", "K_pw", "k_met"),
      # default values as defined by Klein et al. (2021)
      param=list(k_photo_max=0.47, E_max=1, r_A_DW=1000,
                 r_FW_DW=16.7, r_FW_V=1, K_pw=1, k_met=0),
      # boundary presets defined by expert judgement
      param.bounds=list(k_photo_max=c(0, 1), BM_L=c(0, 1e6), E_max=c(0, 1),
                        EC50_int=c(0, 1e6), b=c(0.1, 20), P=c(0, 100)),
      endpoints=c("BM", "r"),
      control.req=TRUE,
      init=c(BM=0, M_int=0),
      transfer.comp.biomass="BM",
      transfer.comp.scaled="M_int"
  )
}


########################
## Simulation
########################

# Solver for MyrioExp scenarios
solver_myrioexp <- function(scenario, ...) {
  # Constant identifying exponential growth model
  scenario@param$growthno <- 1
  # Dummy value, only used for logistic growth
  scenario@param$BM_L <- NA_real_

  solver_myrio(scenario, ...)
}
#' @include solver.R
#' @describeIn solver Numerically integrates `MyrioExp` models
setMethod("solver", "MyrioExp", function(scenario, times, ...) solver_myrioexp(scenario, times, ...))

# Solver for MyrioLog scenarios
solver_myriolog <- function(scenario, ...) {
  # Constant identifying logistic growth model
  scenario@param$growthno <- 2
  solver_myrio(scenario, ...)
}
#' @describeIn solver Numerically integrates `MyrioLog` models
setMethod("solver", "MyrioLog", function(scenario, times, ...) solver_myriolog(scenario, times, ...))

# Numerically solve Myriophyllum scenarios
#
# @param scenario
# @param times
# @param ... additional parameters passed on to [deSolve::ode()]
# @return data.frame
solver_myrio <- function(scenario, times, approx=c("linear","constant"),
                         f=0, nout=2, method="lsoda", hmax=0.1, ...) {
  approx <- match.arg(approx)
  if(missing(times))
    times <- scenario@times
  if(length(times) < 2)
    stop("output times vector is not an interval")

  params <- scenario@param
  # make sure that parameters are present and in required order
  params_order <- c("k_photo_max", "growthno", "BM_L", "E_max", "EC50_int", "b",
                    "P", "r_A_DW", "r_FW_DW", "r_FW_V", "r_DW_TSL", "K_pw",
                    "k_met")
  if(is.list(params))
    params <- unlist(params)

  # check for missing parameters
  params_missing <- setdiff(scenario@param.req, names(params))
  if(length(params_missing)>0)
    stop(paste("missing parameters:", paste(params_missing, collapse=",")))

  # reorder parameters for deSolve
  params <- params[params_order]
  forcings <- list(scenario@exposure@series)
  fcontrol <- list(method=approx, rule=2, f=f, ties="ordered")

  # set names of additional output variables
  outnames <- c("C_int", "TSL", "f_photo", "C_int_unb", "C_ext", "dBM", "dM_int")

  as.data.frame(ode(y=scenario@init, times=times, parms=params, dllname="cvasi",
                    initfunc="myrio_init", func="myrio_func", initforc="myrio_forc",
                    forcings=forcings, fcontrol=fcontrol, nout=nout, method=method,
                    hmax=hmax, outnames=outnames, ...))
}


########################
## Effects
########################

#' @include fx.R model-lemna_setac.R
#' @describeIn fx Effect at end of simulation of [Myriophyllum-models]
setMethod("fx", "Myriophyllum", function(scenario, ...) fx_lemna(scenario, ...))
