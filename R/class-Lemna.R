#' Macrophyte models
#'
#' Population models of standard test macrophytes, such as *Lemna spp.*
#'
#' Available macrophyte models:
#' - [Lemna][Lemna-models]
#' - [Myriophyllum][Myrio()]
#'
#' @inheritSection Transferable-class Biomass transfer
#'
#' @name Macrophyte-models
#' @family macrophytes
#' @seealso \linkS4class{EffectScenario}
NULL

#' Lemna models
#'
#' Overview of supported *Lemna* models
#'
#' - [Lemna_Schmitt()] by Schmitt *et al.* (2013)
#' - [Lemna_SETAC()] by Klein *et al.* (2021)
#'
#' @inheritSection Transferable-class Biomass transfer
#' @name Lemna-models
#' @seealso [Macrophyte-models]
NULL

#' Generic Lemna class
#'
#' Only used for technical purposes
#'
#' @include class-Transferable.R
#' @seealso \linkS4class{LemnaSchmittScenario}, \linkS4class{LemnaSetacScenario}
#' @export
setClass("Lemna", contains=c("Transferable","EffectScenario"))

#' Lemna model class (Schmitt et al. 2013)
#'
#' Please refer to [Lemna_Schmitt()] for details about the model. Class slots are
#' documented in \linkS4class{EffectScenario}.
#' @export
setClass("LemnaSchmittScenario", contains="Lemna")

#' Lemna model class (Klein et al. 2021)
#'
#' Please refer to [Lemna_SETAC()] for details about the model. Class slots are
#' documented in \linkS4class{EffectScenario}.
#' @export
setClass("LemnaSetacScenario", contains="Lemna")

#' Lemna model (Schmitt et al. 2013)
#'
#' The model is a mechanistic combined toxicokinetic-toxicodynamic (TK/TD) and
#' growth model for the aquatic macrophytes *Lemna spp.*
#' The model simulates the development of Lemna biomass under laboratory and
#' environmental conditions and was developed by Schmitt et al. (2013). Growth
#' of the Lemna population is simulated on basis of photosynthesis and respiration
#' rates which are functions of environmental conditions.
#' The toxicodynamic sub-model describes the effects of growth-inhibiting
#' substances by a respective reduction in the photosynthesis rate based on
#' internal concentrations.
#'
#' Constructors to ease creation of scenarios based on the *Lemna* model by
#' Schmitt *et al.* (2013).
#' A variant of this Lemna model, `Lemna_SchmittThold()`, provides an additional
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
#'   * k_phot_fix, logical, TRUE then k_phot_max is not changed by environmental factors, else FALSE
#'   * k_phot_max, 1/d, maximum photosynthesis rate
#'   * k_resp, 1/d, respiration rate
#'   * k_loss, 1/d, rate of loss (e.g. flow rate)
#'   * mass_per_frond, g_dw/frond, dry weight per frond
#'   * BMw2BMd, g_fw/g_dw, Fresh weight/dry weight
#' * Effect
#'   * Emax, -, maximum effect \[0,1\]
#'   * EC50, ug/L, midpoint of effect curve
#'   * b, -, slope of effect curve
#' * Toxicokinetics
#'   * P_up, cm/d, Permeability for uptake
#'   * AperBM, cm2/g_dw, A_leaf / d_leaf = 1/d_leaf (for circular disc, d=0.05 cm)
#'   * Kbm, -, Biomass(fw) : water partition coefficient
#'   * P_Temp, logical, TRUE to enable temperature dependence of cuticle permeability, else FALSE
#'   * MolWeight, g/mol, Molmass of molecule (determines Q10_permeability)
#' * Temperature dependence
#'   * Tmin, deg C, minimum temperature for growth
#'   * Tmax, deg C, maximum temperature for growth
#'   * Topt, deg C, optimal temperature for growth
#'   * t_ref, deg C, reference temperature for respiration rate
#'   * Q10, -, temperature dependence factor for respiration rate
#' * Light dependence
#'   * k_0, 1/d, light dependence: intercept of linear part
#'   * a_k, (1/d)/(kJ/m2.d), light dependence: slope of linear part
#' * Phosphorus dependence (Hill like dep.)
#'   * C_P, mg/L, phosphorus concentration in water
#'   * CP50, mg/L, phosphorus conc. where growth rate is halfed
#'   * a_p, -, Hill coefficient
#'   * KiP, mg/L, p-inhibition constant for very high p-conc.
#' * Nitrogen dependence (Hill like dep.)
#'   * C_N, mg/L, nitrogen concentration in water
#'   * CN50, mg/L, n-conc. where growth rate is halfed
#'   * a_N, -, Hill coefficient
#'   * KiN, mg/L, n-inhibition constant for very high p-conc.
#' * Density dependence
#'   * BM50, g_dw/m2, cut off BM
#'
#' The `Lemna_SchmittThold` model requires the following additional parameter:
#' * threshold, ug/L, cumulative exposure threshold
#'
#' @section Forcings:
#' Besides exposure, the Lemna model requires two environmental properties as
#' time-series input: global radiation (`rad`, kJ/m2.d) and temperature (`temp`, deg C).
#' Forcings time-series are represented by `data.frame` objects consisting of two
#' columns. The first for time and the second for the environmental factor in question.
#'
#' Entries of the `data.frame` need to be ordered chronologically. A time-series
#' can consist of only a single row; in this case it will represent constant
#' environmental conditions. See \linkS4class{EffectScenario} for more details.
#'
#' @section Effects:
#' Supported effect endpoints include *BM* (biomass) and *r* (average
#' growth rate during simulation). The effect on biomass is calculated from
#' the last state of a simulation. Be aware that endpoint *r* is incompatible
#' with frond transfers.
#'
#' @section Simulation output:
#' Simulation results will contain two additional columns besides state variables:
#' * C_int, ug/L, internal concentration of toxicant
#' * FrondNo, -, number of fronds
#'
#' It is possible to amend the output of [simulate()] with additional model
#' quantities that are not state variables, for e.g. debugging purposes or to
#' analyze model behavior. To enable or disable additional outputs, use the
#' optional argument `nout` of [simulate()], see examples below. `nout=1`
#' enables reporting of internal concentration (C_int), `nout=14` enables all
#' additional outputs, and `nout=0` will disable additional outputs.
#'
#' The available output levels are as follows:
#' * `nout=1`
#'   * `C_int`, internal concentration (ug/L)
#' * `nout=2`
#'   * `FrondNo`, number of fronds (-)
#' * `nout=3`
#'   * `C_int_u`, unbound internal concentration (ug/l)
#' * `nout=8`, growth and TK/TD
#'   * `BM_fresh`, fresh weight biomass (g_fw/m2)
#'   * `k_photo_eff`, current photosynthesis rate (1/d)
#'   * `k_resp_eff`, current respiration rate (1/d)
#'   * `f_Eff`, toxic effect factor (-)
#'   * `P_up_eff`, current permeability for uptake (cm/d)
#' * `nout=11`, environmental factors
#'   * `actConc`, current toxicant concentration in surrounding medium (ug/L)
#'   * `actTemp`, current environmental temperature (deg C)
#'   * `actRad`, current environmental radiation (kJ/m2.d)
#' * `nout=14`, derivatives
#'   * `d BM/dt`, current change in state variable BM
#'   * `d E/dt`, current change in effect
#'   * `d M_int/dt`, current change in internal toxicant mass
#'
#' @inheritSection Transferable-class Biomass transfer
#'
#' @param param optional named `list` or `vector` of model parameters
#' @param init optional named numeric `vector` of initial state values
#'
#' @seealso [Lemna-models], [Macrophyte-models], \linkS4class{Transferable}, \linkS4class{EffectScenario}
#' @references
#' Schmitt W., Bruns E., Dollinger M., and Sowig P., 2013:
#' *Mechanistic TK/TD-model simulating the effect of growth inhibitors on
#' Lemna populations*. Ecol Model 255, pp. 1-10. \doi{10.1016/j.ecolmodel.2013.01.017}
#'
#' @family Lemna models
#' @family macrophytes
#' @export
Lemna_Schmitt <- function(param, init) {
  new("LemnaSchmittScenario",
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
      endpoints=c("BM","r"),
      forcings.req=c("temp","rad"),
      control.req=TRUE,
      init=c(BM=0,E=1,M_int=0),
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

#' Lemna model (Klein et al. 2021)
#'
#' The model was described and published by the SETAC Europe Interest Group
#' Effect Modeling (Klein et al. 2022). The *Lemna* model based on the *Lemna*
#' model by Schmitt (2013). The model is a mechanistic combined
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
#' environmental conditions. See \linkS4class{EffectScenario} for more details.
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
#' - `nout >= 1`
#'    - `C_int`, internal concentration (mass per volume)
#' - `nout >= 2`
#'    - `FrondNo`, frond number (-)
#' - `nout >= 4`
#'   - `f_loss`, respiration dependency function (-)
#'   - `f_photo`, photosynthesis dependency function (-)
#' - `nout >= 10`
#'   - `fT_photo`, temperature response of photosynthesis (-)
#'   - `fI_photo`, irradiance response of photosynthesis (-)
#'   - `fP_photo`, phosphorus response of photosynthesis (-)
#'   - `fN_photo`, nitrogen response of photosynthesis (-)
#'   - `fBM_photo`, density response of photosynthesis (-)
#'   - `fCint_photo`, concentration response of photosynthesis (-)
#' - `nout >= 16`
#'   - `C_int_unb`, unbound internal concentration (mass per volume)
#'   - `C_ext`, external concentration (mass per volume)
#'   - `Tmp`, temperature (deg C)
#'   - `Irr`, irradiance (kJ m-2 d-1)
#'   - `Phs`, Phosphorus concentration (mg P L-1)
#'   - `Ntr`, Nitrogen concentration (mg N L-1)
#' - `nout >= 18`
#'   - `dBM`, biomass derivative (g dw m-2 d-1)
#'   - `dM_int`, mass of toxicant in plants derivative (mass per m2 d-1)
#'
#' @inheritSection Transferable-class Biomass transfer
#'
#' @references
#' Klein J., Cedergreen N., Heine S., Reichenberger S., Rendal C.,
#' Schmitt W., Hommen U., 2021: *Refined description of the Lemna TKTD growth model
#' based on Schmitt et al. (2013) – equation system and default parameters*.
#' Report of the working group *Lemna* of the SETAC Europe Interest Group Effect
#' Modeling. Version 1, uploaded on 22. Sept. 2021.
#' https://www.setac.org/group/SEIGEffectModeling
#' @seealso [Lemna-models], [Macrophyte-models], \linkS4class{Transferable}, \linkS4class{EffectScenario}
#' @family Lemna models
#' @family macrophytes
#' @export
Lemna_SETAC <- function() {
  if(!requireNamespace("lemna", quietly=TRUE))
    warning("required package 'lemna' is not installed")

  new("LemnaSetacScenario",
      name="Lemna_SETAC",
      param.req=names(lemna::param_defaults()),
      param=lemna::param_defaults()[!is.na(lemna::param_defaults())],
      endpoints=c("BM","r"),
      forcings.req=c("tmp","irr","P","N"),
      forcings=list(tmp=data.frame(time=0,tmp=NA_real_),
                 irr=data.frame(time=0,irr=NA_real_),
                 P=data.frame(time=0,P=NA_real_),
                 N=data.frame(time=0,N=NA_real_)
                 ),
      control.req=TRUE,
      init=c(BM=0,M_int=0),
      transfer.comp.biomass="BM",
      transfer.comp.scaled="M_int"
  )
}

