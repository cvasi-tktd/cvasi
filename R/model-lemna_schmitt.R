########################
## Class definition
########################

# Lemna model class (Schmitt et al. 2013)
#' @export
#' @include model-lemna_setac.R
setClass("LemnaSchmitt", contains="Lemna")
# for backwards compatibility
#' @export
setClass("LemnaSchmittScenario", contains="LemnaSchmitt")


########################
## Constructor
########################

#' Lemna model (Schmitt et al. 2013)
#'
#' The model is a mechanistic combined toxicokinetic-toxicodynamic (TK/TD) and
#' growth model for the aquatic macrophytes *Lemna spp.*
#' The model simulates the development of *Lemna* biomass under laboratory and
#' environmental conditions and was developed by Schmitt *et al.* (2013). Growth
#' of the *Lemna* population is simulated on basis of photosynthesis and respiration
#' rates which are functions of environmental conditions.
#' The toxicodynamic sub-model describes the effects of growth-inhibiting
#' substances by a respective reduction in the photosynthesis rate based on
#' internal concentrations. This is the historical version of the Lemna model.
#' For current uses, we recommend the Lemna (SETAC) model, which is a more recent
#' version of the Schmitt model.
#'
#' Constructors to ease creation of scenarios based on the *Lemna* model by
#' Schmitt *et al.* (2013).
#' A variant of this *Lemna* model, `Lemna_SchmittThold()`, provides an additional
#' cumulative exposure threshold parameter. The Lemna biomass stops growing
#' if the integral of exposure over time exceeds the threshold. The integral
#' of exposure is internally accounted for by an additional state variable
#' `AUC` (Area Under Curve).
#'
#' @section State variables:
#' The following list describes the default names and standard units of the model's
#' state variables:
#' * BM, g_dw/m2, dry weight biomass per square meter
#' * E, -, effect \[0,1\]
#' * M_int, ug, internal toxicant mass
#' * AUC, ug/L, cumulative exposure (**only** for `LemnaThreshold` model)
#'
#' Biomass (BM) and internal toxicant mass (M_int) are initialized to zero by
#' default. See [set_init()] on how to set the initial states.
#'
#' @section Model parameters:
#' The following model parameters are required:
#' * Fate and biomass
#'   * `k_phot_fix`, logical, TRUE then k_phot_max is not changed by environmental factors, else FALSE
#'   * `k_phot_max`, 1/d, maximum photosynthesis rate
#'   * `k_resp`, 1/d, respiration rate
#'   * `k_loss`, 1/d, rate of loss (e.g. flow rate)
#'   * `mass_per_frond`, g_dw/frond, dry weight per frond
#'   * `BMw2BMd`, g_fw/g_dw, Fresh weight/dry weight
#' * Effect
#'   * `Emax`, -, maximum effect \[0,1\]
#'   * `EC50`, ug/L, midpoint of effect curve
#'   * `b`, -, slope of effect curve
#' * Toxicokinetics
#'   * `P_up`, cm/d, Permeability for uptake
#'   * `AperBM`, cm2/g_dw, A_leaf / d_leaf = 1/d_leaf (for circular disc, d=0.05 cm)
#'   * `Kbm`, -, Biomass(fw) : water partition coefficient
#'   * `P_Temp`, logical, TRUE to enable temperature dependence of cuticle permeability, else FALSE
#'   * `MolWeight`, g/mol, Molmass of molecule (determines Q10_permeability)
#' * Temperature dependence
#'   * `Tmin`, deg C, minimum temperature for growth
#'   * `Tmax`, deg C, maximum temperature for growth
#'   * `Topt`, deg C, optimal temperature for growth
#'   * `t_ref`, deg C, reference temperature for respiration rate
#'   * `Q10`, -, temperature dependence factor for respiration rate
#' * Light dependence
#'   * `k_0`, 1/d, light dependence: intercept of linear part
#'   * `a_k`, (1/d)/(kJ/m2.d), light dependence: slope of linear part
#' * Phosphorus dependence (Hill like dep.)
#'   * `C_P`, mg/L, phosphorus concentration in water
#'   * `CP50`, mg/L, phosphorus conc. where growth rate is halfed
#'   * `a_p`, -, Hill coefficient
#'   * `KiP`, mg/L, p-inhibition constant for very high p-conc.
#' * Nitrogen dependence (Hill like dep.)
#'   * `C_N`, mg/L, nitrogen concentration in water
#'   * `CN50`, mg/L, n-conc. where growth rate is halfed
#'   * `a_N`, -, Hill coefficient
#'   * `KiN`, mg/L, n-inhibition constant for very high p-conc.
#' * Density dependence
#'   * `BM50`, g_dw/m2, cut off BM
#'
#' The `Lemna_SchmittThold` model requires the following additional parameter:
#' * `threshold`, ug/L, cumulative exposure threshold
#'
#' @section Forcings:
#' Besides exposure, the Lemna model requires two environmental properties as
#' time-series input: global radiation (`rad`, kJ/m2.d) and temperature (`temp`, deg C).
#' Forcings time-series are represented by `data.frame` objects consisting of two
#' columns. The first for time and the second for the environmental factor in question.
#'
#' Entries of the `data.frame` need to be ordered chronologically. A time-series
#' can consist of only a single row; in this case it will represent constant
#' environmental conditions. See [scenarios] for more details.
#'
#' @section Effects:
#' Supported effect endpoints include *BM* (biomass) and *r* (average
#' growth rate during simulation). The effect on biomass is calculated from
#' the last state of a simulation. Be aware that endpoint *r* is incompatible
#' with frond transfers.
#'
#' @section Parameter boundaries:
#' Default values for parameter boundaries are set for all parameters by expert
#' judgement, for calibration purposes. Values can be access from the object, and
#' defaults overwritten.
#'
#' @section Simulation output:
#' Simulation results will contain two additional columns besides state variables:
#' * `C_int`, ug/L, internal concentration of toxicant
#' * `FrondNo`, -, number of fronds
#'
#' It is possible to amend the output of [simulate()] with additional model
#' quantities that are not state variables, for e.g. debugging purposes or to
#' analyze model behavior. To enable or disable additional outputs, use the
#' optional argument `nout` of [simulate()], see examples below. `nout=1`
#' enables reporting of internal concentration (C_int), `nout=14` enables all
#' additional outputs, and `nout=0` will disable additional outputs.
#'
#' The available output levels are as follows:
#' * `nout >= 1`: `C_int`, internal concentration (ug/L)
#' * `nout >= 2`: `FrondNo`, number of fronds (-)
#' * `nout >= 3`: `C_int_u`, unbound internal concentration (ug/l)
#' * Growth and TK/TD
#'   * `nout >= 4`: `BM_fresh`, fresh weight biomass (g_fw/m2)
#'   * `nout >= 5`: `k_photo_eff`, current photosynthesis rate (1/d)
#'   * `nout >= 6`: `k_resp_eff`, current respiration rate (1/d)
#'   * `nout >= 7`: `f_Eff`, toxic effect factor (-)
#'   * `nout >= 8`: `P_up_eff`, current permeability for uptake (cm/d)
#' * Environmental variables
#'   * `nout >= 9`: `actConc`, current toxicant concentration in surrounding medium (ug/L)
#'   * `nout >= 10`: `actTemp`, current environmental temperature (deg C)
#'   * `nout >= 11`: `actRad`, current environmental radiation (kJ/m2.d)
#' * Derivatives
#'   * `nout >= 12`: `d BM/dt`, current change in state variable BM
#'   * `nout >= 13`: `d E/dt`, current change in effect
#'   * `nout >= 14`: `d M_int/dt`, current change in internal toxicant mass
#'
#' @inheritSection Transferable Biomass transfer
#'
#' @param param optional named `list` or `vector` of model parameters
#' @param init optional named numeric `vector` of initial state values
#'
#' @return an S4 object of type [LemnaSchmitt-class]
#' @seealso [Lemna-models], [Macrophyte-models], [Transferable], [Scenarios]
#' @references
#' Schmitt W., Bruns E., Dollinger M., and Sowig P., 2013:
#' *Mechanistic TK/TD-model simulating the effect of growth inhibitors on
#' Lemna populations*. Ecol Model 255, pp. 1-10. \doi{10.1016/j.ecolmodel.2013.01.017}
#'
#' @family Lemna models
#' @family macrophyte models
#' @aliases LemnaSchmitt-class LemnaSchmittScenario-class
#' @export
Lemna_Schmitt <- function(param, init) {
  new("LemnaSchmitt",
      name="Lemna_Schmitt",
      param.req=c("Emax", "EC50", "b", "P_up", "AperBM", "Kbm", "P_Temp",
                  "MolWeight", "k_phot_fix", "k_phot_max", "k_resp", "k_loss",
                  "Tmin", "Tmax", "Topt", "t_ref", "Q10", "k_0", "a_k", "C_P",
                  "CP50", "a_P", "KiP", "C_N", "CN50", "a_N", "KiN", "BM50",
                  "mass_per_frond", "BMw2BMd"),
      # default values as defined by Schmitt et al. (2013)
      param=list(Emax=1, AperBM=1000, Kbm=1, P_Temp=FALSE,
                 MolWeight=390.4, k_phot_fix=TRUE, k_phot_max=0.47, k_resp=0.05, k_loss=0.0,
                 Tmin=8.0, Tmax=40.5, Topt=26.7, t_ref=25, Q10=2, k_0=3, a_k=5E-5, C_P=0.3,
                 CP50=0.0043, a_P=1, KiP=101, C_N=0.6, CN50=0.034, a_N=1, KiN=604, BM50=176,
                 mass_per_frond=0.0001, BMw2BMd=16.7),
      # boundary presets defined by expert judgement
      param.bounds=list(Emax=c(0, 1),  EC50=c(0, 1e6), b=c(0.1, 20), P_up=c(0, 100),
                        k_phot_max=c(0,1)),
      endpoints=c("BM","r"),
      forcings.req=c("temp", "rad"),
      control.req=TRUE,
      init=c(BM=0.0012, E=1, M_int=0),
      transfer.interval=-1,
      transfer.biomass=0.0012,
      transfer.comp.biomass="BM",
      transfer.comp.scaled="M_int"
  ) -> o
  if(!missing(param))
    o <- set_param(o, param)
  if(!missing(init))
    o <- set_init(o, init)

  o
}

#' @describeIn Lemna_Schmitt model variant with cumulative exposure threshold
#' @export
Lemna_SchmittThold <- function(param, init) {
  o <- Lemna_Schmitt()
  o@name <- "Lemna_SchmittThold"
  o@param.req <- c(o@param.req, "threshold")
  o@init <- c(o@init, AUC=0)

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
# @param approx string, interpolation method of exposure series, see [stats::approxfun()]
# @param f if `approx="constant"`, a number between 0 and 1 inclusive, see [stats::approxfun()]
# @param nout `numeric`, number of additional output variables, `nout=1` appends
#   the internal concentration `C_int`, the maximum number is 13
# @param method string, numerical solver used by [deSolve::ode()]
# @param hmax numeric, maximum step length in time, see [deSolve::ode()]
# @param ... additional arguments passed to [deSolve::ode()]
#' @importFrom deSolve ode
solver_lemna_schmitt <- function(scenario, approx=c("linear","constant"),
                                 f=1, nout=2, method="ode45", hmax=0.1, ...) {
  params <- scenario@param
  if(is.list(params))
    params <- unlist(params)
  approx <- match.arg(approx)

  # make sure that parameters are present and in required order
  params.req <- c("Emax","EC50","b","P_up","AperBM","Kbm","P_Temp","MolWeight",
                  "k_phot_fix","k_phot_max","k_resp","k_loss","Tmin","Tmax",
                  "Topt","t_ref","Q10","k_0","a_k","C_P","CP50","a_P","KiP",
                  "C_N","CN50","a_N","KiN","BM50","mass_per_frond","BMw2BMd")
  # additional output variables
  outnames <- c("C_int","FrondNo","C_int_u","BM_fresh","k_phot_eff","k_resp_eff","f_Eff",
                "P_up_eff","actConc","actTemp","actRad","dBMdt","dEdt","dM_intdt")

  # check for missing parameters
  params.missing <- setdiff(params.req, names(params))
  if(length(params.missing)>0)
    stop(paste("missing parameters:",paste(params.missing,sep=",",collapse=",")))

  # make sure exposure AUC parameter exists
  # magic value: -1 denotes that threshold is not considered
  if(!("threshold" %in% names(params))) {
    params["threshold"] <- -1
  }
  # disable threshold for scenarios were effects were disabled
  if(params["Emax"] <= 0 | params["threshold"] <= 0) {
    params["threshold"] <- -1
  }

  # reorder parameters for deSolve
  params <- params[c(params.req, "threshold")]
  # check if any parameter is negative apart from threshold
  if(any(head(params,-1)<0))
    stop(paste("parameter out of bounds: ",paste(names(params)[which(head(params,-1)<0)], collapse=",")))

  # create forcings list
  # TODO check if it can be activated without issues
  #if(params["k_phot_fix"]==FALSE)
  forcings <- list(scenario@exposure@series, scenario@forcings$temp, scenario@forcings$rad)
  #else  # temp and radiation are not required by model if k_phot_fix==TRUE
  #  forcings <- list(exposure, data.frame(t=0,temp=-1), data.frame(t=0,rad=-1))

  # run solver
  as.data.frame(ode(y=scenario@init, times=scenario@times, parms=params, dllname="cvasi",
                    initfunc="lemna_schmitt_init", func="lemna_schmitt_func", initforc="lemna_schmitt_forc",
                    forcings=forcings, fcontrol=list(method=approx, rule=2, f=f, ties="ordered"),
                    nout=nout, outnames=outnames, method=method, hmax=hmax, ...))
}
#' @include solver.R
#' @describeIn solver Numerically integrates Lemna_Schmitt models
setMethod("solver", "LemnaSchmitt", function(scenario, ...) solver_lemna_schmitt(scenario, ...) )


########################
## Effects
########################

# effects are calculated them same way as for `LemnaSetac`, see
# model-lemna_setac.R for details
