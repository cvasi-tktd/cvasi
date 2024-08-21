#' Algae models
#'
#' Overview of supported *Algae* models
#'
#' - [Algae_Weber()] by Weber *et al.* (2012)
#' - [Algae_TKTD()] based on Weber *et al.* (2012), but with scaled damage
#' - [Algae_Simple()] Simple growth model without additional forcing variables
#'
#' @inheritSection Transferable Biomass transfer
#'
#' @references
#' Weber D, Schaeffer D, Dorgerloh M, Bruns E, Goerlitz G, Hammel K, Preuss TG
#' and Ratte HT, 2012. Combination of a higher-tier flow-through system and
#' population modeling to assess the effects of time-variable exposure of
#' isoproturon on the green algae Desmodesmus subspictatus and
#' Pseudokirchneriella subcapitata. Environmental Toxicology and
#' Chemistry, 31, 899-908. \doi{10.1002/etc.1765}
#' EFSA Panel on Plant Protection Products and their Residues, 2018. Scientific
#' opinion on the state of the art of Toxicokinetic/Toxicodynamic (TKTD) effect
#' models for regulatory risk assessment of pesticides for aquatic organisms.
#' EFSA journal 16:5377 \doi{10.2903/j.efsa.2018.5377}
#'
#' @name Algae-models
#' @family algae models
#' @family scenarios
#' @seealso [Lemna-models], [Transferable]
#' @include class-Transferable.R
#' @aliases Algae-class
NULL

# Generic Algae class
#' @export
setClass("Algae", contains = c("Transferable", "EffectScenario"))

# Algae model with exponential growth and forcings (P, I)
#' @export
setClass("AlgaeWeberScenario", contains = "Algae")

# Algae model with exponential growth and scaled internal damage
#' @export
setClass("AlgaeTKTDScenario", contains = "Algae")

# Algae model with exponential growth without forcings
#' @export
setClass("AlgaeSimpleScenario", contains = "Algae")

#' Algae model with exponential growth and forcings (I, T)
#'
#'The model is a mechanistic combined toxicokinetic-toxicodynamic (TK/TD) and
#'growth model for algae. The model simulates the development of algal biomass
#'under laboratory and environmental conditions and was developed by
#'Weber et al. (2012) as cited in EFSA TKTD opinion (2018). The growth of the
#'algae population is simulated on the basis of growth rates, which are
#'dependent on environmental conditions (radiation, temperature and phosphorus).
#'The toxicodynamic sub-model describes the effects of growth-inhibiting
#'substances through a corresponding reduction in the photosynthesis rate on
#'the basis of internal concentrations. (the implementation of Weber et al.
#'(2012) is followed where units differ with EFSA)
#'
#' @section State variables:
#' The model has four state variables:
#' - `A`, Biomass (ug fresh wt/mL, cells/mL *10^4)
#' - `Q`, Mass of phosphorous internal (mg P/L, or ug P/mL)
#' - `P`, Mass of phosphorous external (mg P/L, or ug P/mL)
#' - `C`, external substance concentration (ug/L)
#'
#' @section Model parameters:
#' - Growth model
#'   - `mu_max`, Maximum growth rate (d-1)
#'   - `Q_min`, Minimum intracellular P (ug P/ug fresh wt)
#'   - `Q_max`, Maximum intracellular P (ug P/ug fresh wt)
#'   - `v_max`, Maximum P-uptake rate at non-limited growth (ug P/ug fresh wt/d)
#'   - `k_s`,   Half-saturation constant for extracellular P (mg P/L)
#'   - `m_max`, Natural mortality rate (1/d)
#'   - `I_opt`, Optimum light intensity for growth (uE/m²/s)
#'   - `T_opt`, Optimum temperature for growth (°C)
#'   - `T_max`, Maximum temperature for growth (°C)
#'   - `T_min`, Minimum temperature for growth (°C)
#'   - `D`, Dilution rate (1/d)
#'   - `R_0`, Influx concentration of P (mg P/L)
#'
#' - Concentration response (Toxicodynamics)
#'   - `EC_50`, Effect concentration of 50% inhibition of growth rate (ug/L)
#'   - `b`, slope of concentration effect curve at EC_50 (-)
#'
#' - External concentration (Toxicokinetics)
#'   - `k`, Degradation rate of toxicant in aquatic environments (d-1)
#'
#' @section Forcings:
#' Besides exposure events (C_in), the *Algae* model requires three environmental
#' properties as time-series input: Irradiance (`I`, uE/m²/s)
#' and temperature (`T_act`, deg C).
#' Forcings time-series are represented by `data.frame` objects
#' consisting of two columns. The first for time and the second for the
#' environmental factor in question. The input format for all forcings is a
#' list of the data frames.
#'
#' @section Simulation output:
#' Simulation results will contain the state variables Biomass (`A`), mass of
#' internal phosphorous (`Q`), mass of external phosphorous (`P`) and the external
#' concentration (`C`). The derivatives are also available as additional output.
#'
#' - `nout >= 4`
#'    - `dA`, biomass derivative (µg)
#'    - `dQ`, internal phosphorous derivative (mg P/ug fresh wt)
#'    - `dP`, external phosphorous derivative (mg P L-1)
#'    - `dC`, external concentration derivative (ug L-1)
#'
#' @section Parameter boundaries:
#' Default values for parameter boundaries are set for all parameters by expert
#' judgement, for calibration purposes. Values can be access from the object, and
#' defaults overwritten.
#'
#' @references
#' Weber D, Schaeffer D, Dorgerloh M, Bruns E, Goerlitz G, Hammel K, Preuss TG
#' and Ratte HT, 2012. Combination of a higher-tier flow-through system and
#' population modeling to assess the effects of time-variable exposure of
#' isoproturon on the green algae Desmodesmus subspictatus and
#' Pseudokirchneriella subcapitata. Environmental Toxicology and
#' Chemistry, 31, 899-908. \doi{10.1002/etc.1765}
#'
#' EFSA PPR Panel (EFSA Panel on Plant Protection Products and their Residues),
#' Ockleford C, Adriaanse P, Berny P, Brock T, Duquesne S, Grilli S,
#' Hernandez-Jerez AF, Bennekou SH,Klein M, Kuhl T, Laskowski R, Machera K,
#' Pelkonen O, Pieper S, Smith RH, Stemmer M, Sundh I, Tiktak A,Topping CJ,
#' Wolterink G, Cedergreen N, Charles S, Focks A, Reed M, Arena M, Ippolito A,
#' Byers H andTeodorovic I, 2018. Scientific Opinion on the state of the art of
#' Toxicokinetic/Toxicodynamic (TKTD)effect models for regulatory risk assessment
#' of pesticides for aquatic organisms. EFSA Journal, 16(8), 5377.
#' \doi{10.2903/j.efsa.2018.5377}
#'
#' @return an S4 object of type [AlgaeWeberScenario-class]
#' @seealso [Scenarios], [Transferable]
#' @family algae models
#' @export
#' @aliases AlgaeWeberScenario-class
Algae_Weber <- function() {
  new("AlgaeWeberScenario",
    name = "Algae_Weber",
    param.req = c("mu_max", "m_max", "v_max", "k_s",
      "Q_min", "Q_max", "R_0", "D",
      "T_opt", "T_min", "T_max", "I_opt",
      "EC_50", "b", "k"
    ),
    # default values as defined by Weber et al. (2012)
    param = list(mu_max = 1.7380, m_max = 0.0500, v_max = 0.0520,
                 k_s = 0.0680,
                 Q_min = 0.0011, Q_max = 0.0144, R_0 = 0.36, D = 0.5,
                 T_opt = 27, T_min = 0, T_max = 35, I_opt = 120
    ),
    param.low = list(mu_max = 0, m_max = 0, v_max = 0,
                 k_s = 0,
                 Q_min = 0, Q_max = 0, R_0 = 0, D = 0,
                 T_opt = 4, T_min = 0, T_max = 10, I_opt = 20
    ),
    param.up = list(mu_max = 3, m_max = 0.5, v_max = 1,
                 k_s = 1,
                 Q_min = 1, Q_max = 1, R_0 = 50, D = 3,
                 T_opt = 40, T_min = 20, T_max = 50, I_opt = 300
    ),
    endpoints = c("A", "r"),
    # growth as endpoint mu_max * f_T * f_I * f_Q * f_C?
    forcings.req=c("T_act","I","C_in"),
    control.req = TRUE,
    init = c(A = 1, Q = 0.01, P = 0.18, C = 0),
    transfer.interval = -1,
    transfer.biomass = 1,
    transfer.comp.biomass = "A",
    transfer.comp.scaled = "Q"
  )
}

#' Algae model with exponential growth, forcings (P, I) and scaled damage
#'
#'The model is a mechanistic combined toxicokinetic-toxicodynamic (TK/TD) and
#'growth model for algae. The model simulates the development of algal biomass
#'under laboratory and environmental conditions. The growth of the algae
#' population is simulated on the basis of growth rates, which are dependent on
#' environmental conditions (radiation, temperature and phosphorus).
#' The model is a variant of the [Algae_Weber()] model (Weber 2012) as cited
#' in EFSA TKTD opinion (2018). This Algae model, [Algae_TKTD()], provides an
#' additional possibility (probit) to simulate the dose-response curve and
#' considers a scaled internal damage instead of the external concentration.
#'
#' @section State variables:
#' The model has four state variables:
#' - `A`, Biomass (ug fresh wt/mL, cells/mL *10^4)
#' - `Q`, Mass of phosphorous internal (ug P/ug fresh wt)
#' - `P`, Mass of phosphorous external (ug P/L)
#' - `Dw`, Damage concentration (ug/L)
#'
#' @section Model parameters:
#' - Growth model
#'   - `mu_max`, Maximum growth rate (d-1)
#'   - `Q_min`, Minimum intracellular P (ug P/ug fresh wt)
#'   - `Q_max`, Maximum intracellular P (ug P/ug fresh wt)
#'   - `v_max`, Maximum P-uptake rate at non-limited growth (ug P/ug fresh wt/d)
#'   - `k_s`,   Half-saturation constant for extracellular P (mg P/L)
#'   - `m_max`, Natural mortality rate (1/d)
#'   - `I_opt`, Optimum light intensity for growth (uE/m²/s)
#'   - `T_opt`, Optimum temperature for growth (°C)
#'   - `T_max`, Maximum temperature for growth (°C)
#'   - `T_min`, Minimum temperature for growth (°C)
#'
#' - Concentration response (Toxicodynamics)
#'   - `EC_50`, Effect concentration of 50% inhibition of growth rate (ug L-1)
#'   - `b`, slope of concentration effect curve at EC_50 (-)
#'   - `dose_resp`, shape of the dose response curve (0 = logit, 1 = probit)
#'
#' - External concentration (Toxicokinetics)
#'   - `kD`, dominant rate constant (d-1)
#'
#' @section Forcings:
#' Besides exposure events (Cw), the *Algae* model requires two environmental
#' properties as time-series input: Irradiance (`I`, uE/m²/s) and
#' temperature (`T_act`, deg C).
#' Forcings time-series are represented by `data.frame` objects
#' consisting of two columns. The first for time and the second for the
#' environmental factor in question. The input format for all forcings is a
#' list of the data frames.
#'
#' @section Simulation output:
#' Simulation results will contain the state variables Biomass (`A`), mass of
#' internal phosphorous (`Q`), mass of external phosphorous (`P`) and the damage
#' concentration (`Dw`). The derivatives are also available as additional output.
#'
#' - `nout >= 4`
#'    - `dA`, biomass derivative (µg)
#'    - `dQ`, internal phosphorous derivative (mg P/ug fresh wt)
#'    - `dP`, external phosphorous derivative (mg P L-1)
#'    - `dDw`, damage concentration derivative (ug L-1)
#'
#' @references
#' Weber D, Schaeffer D, Dorgerloh M, Bruns E, Goerlitz G, Hammel K, Preuss TG
#' and Ratte HT, 2012. Combination of a higher-tier flow-through system and
#' population modeling to assess the effects of time-variable exposure of
#' isoproturon on the green algae Desmodesmus subspictatus and
#' Pseudokirchneriella subcapitata. Environmental Toxicology and
#' Chemistry, 31, 899-908. \doi{10.1002/etc.1765}
#'
#' @return an S4 object of type [AlgaeTKTDScenario-class]
#' @seealso [Scenarios], [Transferable]
#' @family algae models
#' @export
#' @aliases AlgaeTKTDScenario-class
Algae_TKTD <- function() {
  new("AlgaeTKTDScenario",
      name = "Algae_TKTD",
      param.req = c("mu_max", "m_max", "v_max", "k_s",
                    "Q_min", "Q_max",
                    "T_opt", "T_min", "T_max", "I_opt",
                    "EC_50", "b", "k", "kD", "dose_resp"
      ),
      # default values as defined by Weber et al. (2012)
      param = list(mu_max = 1.7380, m_max = 0.0500, v_max = 0.0520,
                   k_s = 0.0680,
                   Q_min = 0.0011, Q_max = 0.0144,
                   T_opt = 27, T_min = 0, T_max = 35, I_opt = 120,
                   dose_resp = 0
      ),
      param.low = list(mu_max = 0, m_max = 0, v_max = 0,
                       k_s = 0,
                       Q_min = 0, Q_max = 0,
                       T_opt = 4, T_min = 0, T_max = 10, I_opt = 20
      ),
      param.up = list(mu_max = 3, m_max = 0.5, v_max = 1,
                      k_s = 1,
                      Q_min = 1, Q_max = 1,
                      T_opt = 40, T_min = 20, T_max = 50, I_opt = 300
      ),
      endpoints = c("A", "r"),
      # growth as endpoint mu_max * f_T * f_I * f_Q * f_C?
      forcings.req=c("T_act","I","C_in"),
      control.req = TRUE,
      init = c(A = 1, Q = 0.01, P = 0.18, Dw = 0),
      transfer.interval = -1,
      transfer.biomass = 1,
      transfer.comp.biomass = "A",
      transfer.comp.scaled = "Q"
  )
}

#' Algae model with exponential growth but without additional forcings
#'
#' The model is a mechanistic combined toxicokinetic-toxicodynamic (TK/TD) and
#' growth model for algae. It follows the concept of a simplified algae model
#' described in Rendal et al. (2023). The model simulates the development of
#' algal biomass. The growth of the algae population is simulated on the basis
#' of growth rates, which are, in contrast to the Weber model, independent on
#' environmental conditions which are usually optimal in laboratory effect studies.
#' The toxicodynamic sub-model describes the effects of growth-inhibiting
#' substances through a corresponding reduction in the photosynthesis rate on the
#' basis of either external or internal concentrations (depending on user choice of 'scaled' parameter setting).
#'
#' @section State variables:
#' The model has two state variables:
#' - `A`, Biomass (ug fresh wt/mL, cells/mL *10^4)
#' - `Dw`, only used if scaled = 1
#'
#' @section Model parameters:
#' - Growth model
#'   - `mu_max`, Maximum growth rate (d-1)
#'
#' - Concentration response (Toxicodynamics)
#'   - `EC_50`, Effect concentration of 50% inhibition of growth rate (ug L-1)
#'   - `b`, slope of concentration effect curve at EC_50 (-)
#'   - `dose_response`, shape of the dose response curve (0 = logit, 1 = probit)
#'
#' - External concentration (Toxicokinetics)
#'   - `kD`, dominant rate constant of toxicant in aquatic environments (d-1)
#'   - `scaled`, 0 = no internal scaled damage / 1 = yes (-)
#'
#' @section Forcings:
#' Simplified model without additional forcings for e.g. irradiation or temperature
#' as implemented in `Algae_Weber`. A constant growth over time is assumed.
#' In case that growth is time dependent, a forcing variable (f_growth) can be set.
#' Forcing time-series are represented by `data.frame` objects consisting of two
#' columns. The first for time and the second for a scaling factor of mu_max.
#' The input format for all forcings is a list of the data frames. If f_growth is
#' not set, a default scaling factor of 1 is used.
#'
#' @section Parameter boundaries:
#' Upper and lower parameter boundaries are set by default for each parameter. This,
#' to avoid extreme values during calibration (particularly likelihood profiling)
#'
#' @section Simulation output:
#' Simulation results will contain the state variables biomass (`A`) and in case
#' of scaled damage the damage concentration (`Dw`). The derivatives are also
#' available as additional output.
#'
#' - `nout >= 2`
#'    - `dA`, biomass derivative (µg)
#'    - `dDw`, damage concentration derivative (ug L-1)
#'
#'
#' @references
#' Weber D, Schaeffer D, Dorgerloh M, Bruns E, Goerlitz G, Hammel K, Preuss TG
#' and Ratte HT, 2012. Combination of a higher-tier flow-through system and
#' population modeling to assess the effects of time-variable exposure of
#' isoproturon on the green algae Desmodesmus subspictatus and
#' Pseudokirchneriella subcapitata. Environmental Toxicology and
#' Chemistry, 31, 899-908. \doi{10.1002/etc.1765}
#'
#' @return an S4 object of type [AlgaeSimpleScenario-class]
#' @seealso [Scenarios], [Transferable]
#' @family algae models
#' @export
#' @aliases AlgaeSimpleScenario-class
Algae_Simple <- function() {
  new("AlgaeSimpleScenario",
      name = "Algae_Simple",
      param.req = c("mu_max",
                    "EC_50", "b", "kD",
                    "dose_response", "scaled"
      ),
      # default values as defined by Weber et al. (2012)
      param = list(mu_max = 1.7380, const_growth = TRUE,
                   dose_response = 0, scaled = 0
      ),
      param.low = list(mu_max = 0
      ),
      param.up = list(mu_max = 4
      ),
      endpoints = c("A", "r"),
      forcings.req = c("f_growth"),
      forcings = list(f_growth = data.frame(time = 0, f_growth = 1)),
      control.req = TRUE,
      init = c(A = 1, Dw = 0),
      transfer.interval = -1,
      transfer.biomass = 1,
      transfer.comp.biomass = "A"
  )
}
