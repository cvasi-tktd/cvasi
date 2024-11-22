########################
## Class definition
########################

# Lemna model class (Klein et al. 2021)
#' @export
setClass("LemnaSetac", contains="Lemna")
# for backwards compatibility
#' @export
setClass("LemnaSetacScenario", contains="LemnaSetac")


########################
## Constructor
########################

#' Lemna model (Klein et al. 2021)
#'
#' The model was described and published by the SETAC Europe Interest Group
#' Effect Modeling (Klein et al. 2022). It is based on the *Lemna* model
#' by Schmitt (2013). The model is a mechanistic combined
#' toxicokinetic-toxicodynamic (TK/TD) and growth model for the aquatic
#' macrophytes *Lemna spp.*. The model simulates the development of Lemna biomass
#' under laboratory and environmental conditions. Growth of the Lemna population
#' is simulated on basis of photosynthesis and respiration rates which are
#' functions of environmental conditions. The toxicodynamic sub-model describes
#' the effects of growth-inhibiting substances by a respective reduction in the
#' photosynthesis rate based on internal concentrations.
#'
#' @section State variables:
#' The model has two state variables:
#' - `BM`, Biomass (g dw m-2)
#' - `M_int`, Mass of toxicant in plant population (mass per m2, e.g. ug m-2)
#'
#' @section Model parameters:
#' - Growth model
#'   - `k_photo_fixed`, Model switch for unlimited growth conditions (TRUE/FALSE)
#'   - `k_photo_max`, Maximum photosynthesis rate (d-1)
#'   - `k_loss`, Reference loss rate (d-1)
#'   - `BM_threshold`, Lower biomass abundance threshold,  (g dw m-2)
#'   - `BM_min`, Reservoir for biomass recovery,  (g dw m-2)
#'
#' - Temperature response of photosynthesis
#'   - `T_opt`, Optimum growth temperature (°C)
#'   - `T_min`, Minimum growth temperature (°C)
#'   - `T_max`, Maximum growth temperature (°C)
#'
#' - Temperature response of biomass loss rate
#'   - `Q10`, Temperature coefficient (-)
#'   - `T_ref`, Reference temperature for response=1 (°C)
#'
#' - Irradiance reponse of photosynthesis
#'   - `alpha`, Slope of irradiance response (m2 d kJ-1)
#'   - `beta`, Intercept of irradiance response (-)
#'
#' - Nutrient response of photosynthesis
#'   - `N_50`, Half-saturation constant of Nitrogen (mg N L-1)
#'   - `P_50`, Half-saturation constant of Phosphorus (mg P L-1)
#'
#' - Density dependence of photosynthesis
#'   - `BM_L`, Carrying capacity (g dw m-2)
#'
#' - Concentration response (Toxicodynamics)
#'   - `EC50_int`, Internal concentration resulting in 50% effect (ug L-1)
#'   - `E_max`, Maximum inhibition (-)
#'   - `b`, Slope parameter (-)
#'
#' - Internal concentration (Toxicokinetics)
#'   - `P`, Permeability (cm d-1)
#'   - `r_A_DW`, Area per dry-weight ratio (cm2 g-1)
#'   - `r_FW_DW`, Fresh weight per dry weight ratio (-)
#'   - `r_FW_V`, Fresh weight density (g cm-3)
#'   - `r_DW_FN`, Dry weight per frond ratio  (g dw)
#'   - `K_pw`, Partitioning coefficient plant:water (-)
#'   - `k_met`, Metabolisation rate (d-1)
#'
#' @section Forcings:
#' Besides exposure, the model requires four environmental properties as
#' time-series input:
#' - `tmp`, temperature (°C)
#' - `irr`, irradiance (kJ m-2 d-1)
#' - `P`, Phosphorus concentration (mg P L-1)
#' - `N`, Nitrogen concentration (mg N L-1)
#'
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
#' with biomass transfers.
#'
#' @section Simulation output:
#' For reasons of convenience, the return value contains by default two additional
#' variables derived from simulation results: the internal concentration `C_int`
#' as well as the number of fronds `FrondNo`. These can be disabled by setting
#' the argument `nout = 0`.
#'
#' The available output levels are as follows:
#' - `nout >= 1`: `C_int`, internal concentration (mass per volume)
#' - `nout >= 2`: `FrondNo`, frond number (-)
#' - Response functions
#'   - `nout >= 3`: `f_loss`, respiration dependency function (-)
#'   - `nout >= 4`: `f_photo`, photosynthesis dependency function (-)
#'   - `nout >= 5`: `fT_photo`, temperature response of photosynthesis (-)
#'   - `nout >= 6`: `fI_photo`, irradiance response of photosynthesis (-)
#'   - `nout >= 7`: `fP_photo`, phosphorus response of photosynthesis (-)
#'   - `nout >= 8`: `fN_photo`, nitrogen response of photosynthesis (-)
#'   - `nout >= 9`: `fBM_photo`, density response of photosynthesis (-)
#'   - `nout >= 10`: `fCint_photo`, concentration response of photosynthesis (-)
#' - Environmental variables
#'   - `nout >= 11`: `C_int_unb`, unbound internal concentration (mass per volume)
#'   - `nout >= 12`: `C_ext`, external concentration (mass per volume)
#'   - `nout >= 13`: `Tmp`, temperature (deg C)
#'   - `nout >= 14`: `Irr`, irradiance (kJ m-2 d-1)
#'   - `nout >= 15`: `Phs`, Phosphorus concentration (mg P L-1)
#'   - `nout >= 16`: `Ntr`, Nitrogen concentration (mg N L-1)
#' - Derivatives
#'   - `nout >= 17`: `dBM`, biomass derivative (g dw m-2 d-1)
#'   - `nout >= 18`: `dM_int`, mass of toxicant in plants derivative (mass per m2 d-1)
#'
#' @inheritSection Transferable Biomass transfer
#'
#' @references
#' Klein J., Cedergreen N., Heine S., Reichenberger S., Rendal C.,
#' Schmitt W., Hommen U., 2021: *Refined description of the Lemna TKTD growth model
#' based on Schmitt et al. (2013) - equation system and default parameters*.
#' Report of the working group *Lemna* of the SETAC Europe Interest Group Effect
#' Modeling. Version 1, uploaded on 22. Sept. 2021.
#' https://www.setac.org/group/effect-modeling.html
#'
#'
#' Schmitt W., Bruns E., Dollinger M., and Sowig P., 2013:
#' *Mechanistic TK/TD-model simulating the effect of growth inhibitors on
#' Lemna populations*. Ecol Model 255, pp. 1-10. \doi{10.1016/j.ecolmodel.2013.01.017}
#'
#' @return an S4 object of type [LemnaSetac-class]
#' @seealso [Lemna-models], [Macrophyte-models], [Transferable], [Scenarios]
#' @family Lemna models
#' @family macrophyte models
#' @aliases LemnaSetacScenario-class LemnaSetac-class
#' @export
Lemna_SETAC <- function() {
  new("LemnaSetac",
      name="Lemna_SETAC",
      # Recommended parameter values according to Klein et al. (2021)
      param=list(
        k_photo_fixed=TRUE, # model switch, if TRUE then f_loss = 1 and f_photo = fCint_photo()
        k_photo_max = 0.47,    # max photosynthesis rate (d-1)
        k_loss = 0.05,         # reference loss rate (d-1)
        BM_min = 5e-4,         # threshold density for setting dBM/dt to zero (g dw m-2)

        # response parameters
        T_opt = 26.7,   # optimum growth temperature (deg C)
        T_min = 8,      # minimum growth temperature (deg C)
        T_max = 40.5,   # maximum growth temperature (deg C)
        Q10 = 2,        # temperature coefficient (-)
        T_ref = 25,     # ref temperature for response=1 (°C)
        alpha = 5e-5,   # slope of irradiance response of photosynthesis (m2 d kJ-1)
        beta = 0.025,   # intercept of irradiance response of photosynthesis (-)
        N_50 = 0.034,   # half-saturation constant of Nitrogen response (mg N L-1)
        P_50 = 0.0043,  # half-saturation constant of Phosphorus response (mg P L-1)
        BM_L = 177,     # carrying capacity (g dw m-2)

        # toxicodynamic parameters
        E_max = 1,      # maximum inhibition (-), substance specific
        EC50_int = NA,  # int. conc. resulting in 50% effect (ug L-1), substance specific
        b = NA,         # slope parameter (-), substance specific

        # toxicokinetic parameters
        P = NA,         # permeability (cm d-1), substance specific
        r_A_DW = 1000,  # area per dry-weight ratio (cm2 g-1)
        r_FW_DW = 16.7, # fresh weight per dry weight ratio (-)
        r_FW_V = 1,     # fresh weight density (g cm-3)
        r_DW_FN = 1e-4, # dry weight per frond ratio (g dw)
        K_pw = 1,       # partitioning coefficient plant:water (-), generally substance specific
        k_met = 0),
      # parameters required for the model to run
      param.req=c("k_photo_fixed", "k_photo_max", "k_loss", "BM_min", "T_opt",
                  "T_min", "T_max", "Q10", "T_ref", "alpha", "beta", "N_50",
                  "P_50", "BM_L", "E_max", "EC50_int", "b", "P", "r_A_DW",
                  "r_FW_DW", "r_FW_V", "r_DW_FN", "K_pw", "k_met"),
      # boundary presets defined by expert judgement
      param.bounds=list(k_photo_max=c(0, 1), EC50_int=c(0, 1e6), b=c(0.1, 20),
                        P=c(0, 100)),
      endpoints=c("BM", "r"),
      forcings.req=c("tmp", "irr", "P", "N"),
      forcings=list(tmp=data.frame(time=0, tmp=12),
                    irr=data.frame(time=0, irr=15000),
                    P=data.frame(time=0, P=0.3),
                    N=data.frame(time=0, N=0.6)
      ),
      control.req=TRUE,
      init=c(BM=0.0012, M_int=0),
      transfer.comp.biomass="BM",
      transfer.comp.scaled="M_int"
  )
}


########################
## Simulation
########################

# Numerically integrate Lemna_SETAC scenarios
#
# @param scenario an EffectScenario object
# @param times optional output times, will override scenario's properties
# @param approx how to interpolate between data points in forcing series, see [deSolve::ode()]
# @param rule how to handle data points outside of time-series, see [deSolve::forcings]
# @param f how to approximate data points for `constant` interpolation, see [deSolve::forcings]
# @param nout number of additional output variables, see [deSolve::ode()]
# @param method numerical integration method, see [deSolve::ode()]
# @param hmax numeric, set max step length in time, defaults to `0.1`
# @param ... additional parameters passed on to [deSolve::ode()]
#
# @return data.frame
#' @importFrom deSolve ode
solver_lemna_setac <- function(scenario, times, approx = c("linear","constant"),
                               f=0, nout=2, method="lsoda", hmax=0.1, ...) {
  if(missing(times))
    times <- scenario@times
  if(length(times) < 2)
    stop("output times vector is not an interval")

  params <- scenario@param
  if(is.list(params)) params <- unlist(params)
  approx <- match.arg(approx)

  # check for missing parameters
  params.missing <- setdiff(scenario@param.req, names(params))
  if(length(params.missing)>0)
    stop(paste("missing parameters:", paste(params.missing, sep=",", collapse=", ")))

  # reorder parameters for deSolve
  params <- params[scenario@param.req]
  # check if any parameter is negative
  if(any(params < 0, na.rm=TRUE))
    stop(paste("parameter out of bounds:",paste(names(params)[which(params < 0)], collapse=", ")))
  if(any(is.na(params)))
    stop(paste("parameter missing:",paste(names(params)[which(is.na(params))], collapse=", ")))

  envir <- scenario@forcings
  # Environmental factors are irrelevant in case of unlimited growth, removing any
  # superfluous time-series data will reduce simulation overhead/runtime
  if(params["k_photo_fixed"]) {
    forcings <- list(scenario@exposure@series,
                     data.frame(time=0, tmp=0),
                     data.frame(time=0, irr=0),
                     data.frame(time=0, P=0),
                     data.frame(time=0, N=0))
  } else {
    forcings <- list(scenario@exposure@series, envir$tmp, envir$irr, envir$P, envir$N)
  }

  # set names of additional output variables
  outnames <- c("C_int", "FrondNo", "f_loss", "f_photo", "fT_photo", "fI_photo",
                "fP_photo", "fN_photo", "fBM_photo", "fCint_photo", "C_int_unb",
                "C_ext", "Tmp", "Irr", "Phs", "Ntr", "dBM", "dM_int")
  # run deSolve
  as.data.frame(ode(y=scenario@init, times=times, parms=params, dllname="cvasi",
                    initfunc="lemna_setac_init", func="lemna_setac_func", initforc="lemna_setac_forc",
                    forcings=forcings, fcontrol=list(method=approx, rule=2, f=f, ties="ordered"),
                    nout=nout, outnames=outnames, method=method, hmax=hmax, ...))
}
#' @include solver.R
#' @describeIn solver Numerically integrates Lemna_SETAC models
setMethod("solver", "LemnaSetac", function(scenario, times, ...) solver_lemna_setac(scenario, times, ...) )


########################
## Effects
########################

# Calculate effect of Lemna scenario
fx_lemna <- function(scenario, ...) {
  efx_r <- "r" %in% scenario@endpoints
  # TODO move to a validate_scenario function, this takes precious time on every effect() call
  if(efx_r & has_transfer(scenario))
    stop("endpoint r is incompatible with biomass transfers")

  out <- simulate(scenario, ...)

  efx <- c("BM"=tail(out$BM, 1))
  if(efx_r) # we skip the log() operation if we can
    efx["r"] <- log(tail(out$BM,1) / out$BM[1]) / (tail(out[,1],1) - out[1,1])

  efx
}

#' @include fx.R
#' @describeIn fx Effect at end of simulation of [Lemna-models]
setMethod("fx", "Lemna", function(scenario, ...) fx_lemna(scenario, ...))
