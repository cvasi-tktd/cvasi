#' Algae models
#'
#' Overview of supported *Algae* models
#'
#' - [Algae_Weber()] by Weber *et al.* (2012)
#' - [Algae_TKTD()] based on Weber *et al.* (2012), but with scaled damage
#' - [Algae_Simple()] Simple growth model without additional forcing variables
#'
#' @inheritSection Transferable-class Biomass transfer
#' @name Algae-models
NULL

#' Generic Algae class
#'
#' Only used for technical purposes
#' @include class-Transferable.R
#' @seealso \linkS4class{AlgaeWeberScenario}, \linkS4class{AlgaeTKTDScenario}
#' @export
setClass("Algae", contains = c("Transferable", "EffectScenario"))

#' Algae model with exponential growth and forcings (P, I)
#'
#' Please refer to [Algae_Weber()] for details about the model. Class slots are
#' documented in \linkS4class{EffectScenario}.
#' @export
setClass("AlgaeWeberScenario", contains = "Algae")

#' Algae model with exponential growth and scaled internal damage
#'
#' Please refer to [Algae_TKTD()] for details about the model. Class slots are
#' documented in \linkS4class{EffectScenario}.
#' @export
setClass("AlgaeTKTDScenario", contains = "Algae")

#' Algae model with exponential growth without forcings
#'
#' Please refer to [Algae_Simple()] for details about the model. Class slots are
#' documented in \linkS4class{EffectScenario}.

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
#'the basis of internal concentrations.
#'
#' @section State variables:
#' The model has four state variables:
#' - `A`, Biomass (µg fresh wt)
#' - `Q`, Mass of phosphorous internal (mg P/μg fresh wt)
#' - `P`, Mass of phosphorous external (mg P/L)
#' - `C`, external substance concentration (ug/L)
#'
#' @section Model parameters:
#' - Growth model
#'   - `mu_max`, Maximum growth rate (d-1)
#'   - `Q_min`, Minimum intracellular P (mg P/μg fresh wt)
#'   - `Q_max`, Maximum intracellular P (mg P/μg fresh wt)
#'   - `v_max`, Maximum P-uptake rate at non-limited growth (mg P/μg fresh wt/d)
#'   - `k_s`,   Half-saturation constant for extracellular P (mg P/L)
#'   - `m_max`, Natural mortality rate (1/d)
#'   - `I_opt`, Optimum light intensity for growth (μE/m²/s)
#'   - `T_opt`, Optimum temperature for growth (°C)
#'   - `T_max`, Maximum temperature for growth (°C)
#'   - `T_min`, Minimum temperature for growth (°C)
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
#' properties as time-series input: Irradiance (`I`, μE/m²/s)
#' and temperature (`T_act`, deg C).
#' Forcings time-series are represented by `data.frame` objects
#' consisting of two columns. The first for time and the second for the
#' environmental factor in question. The input format for all forcings is a
#' list of the data frames.
#'
#' @section Simulation output:
#'
#' Simulation results will contain state variables.
#'
#' @references
#'Weber D, Schaeffer D, Dorgerloh M, Bruns E, Goerlitz G, Hammel K, Preuss TG
#'and Ratte HT, 2012. Combination of a higher-tier flow-through system and
#'population modeling to assess the effects of time-variable exposure of
#'isoproturon on the green algae Desmodesmus subspictatus and
#'Pseudokirchneriella subcapitata. Environmental Toxicology and
#'Chemistry, 31, 899–908. https://doi.org/10.1002/etc.1765
#'
#'EFSA PPR Panel (EFSA Panel on Plant Protection Products and their Residues),
#'Ockleford C, Adriaanse P, Berny P, Brock T, Duquesne S, Grilli S,
#'Hernandez-Jerez AF, Bennekou SH,Klein M, Kuhl T, Laskowski R, Machera K,
#'Pelkonen O, Pieper S, Smith RH, Stemmer M, Sundh I, Tiktak A,Topping CJ,
#'Wolterink G, Cedergreen N, Charles S, Focks A, Reed M, Arena M, Ippolito A,
#'Byers H andTeodorovic I, 2018. Scientific Opinion on the state of the art of
#'Toxicokinetic/Toxicodynamic (TKTD)effect models for regulatory risk assessment
#'of pesticides for aquatic organisms. EFSA Journal, 16(8), 5377.
#'https://doi.org/10.2903/j.efsa.2018.5377
#'
#' \linkS4class{Transferable},
#' \linkS4class{EffectScenario}
#' @export
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
    T_opt = 27, T_min = 0, T_max = 35, I_opt = 120,
    EC_50 = 115, b = 1.268, k = 0.2
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
#' Constructor to ease creation of *Algae* ‘EffectScenario’s. Model is a variant
#' of the `Algae_Weber` model (Weber 2012) as cited in EFSA TKTD opinion (2018).
#' This Algae model, `Algae_TKTD()`, a) provides an additional
#' possibility to simulate the dose-response curve (probit) and b) considers an
#' scaled internal damage instead of the external concentration.
#'
#' @section State variables:
#' The model has four state variables:
#' - `A`, Biomass (µg fresh wt)
#' - `Q`, Mass of phosphorous internal (mg P/μg fresh wt)
#' - `P`, Mass of phosphorous external (mg P/L)
#' - `Dw`, Damage concentration (ug/L)
#'
#' @section Model parameters:
#' - Growth model
#'   - `mu_max`, Maximum growth rate (d-1)
#'   - `Q_min`, Minimum intracellular P (mg P/μg fresh wt)
#'   - `Q_max`, Maximum intracellular P (mg P/μg fresh wt)
#'   - `v_max`, Maximum P-uptake rate at non-limited growth (mg P/μg fresh wt/d)
#'   - `k_s`,   Half-saturation constant for extracellular P (mg P/L)
#'   - `m_max`, Natural mortality rate (1/d)
#'   - `I_opt`, Optimum light intensity for growth (μE/m²/s)
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
#'   - `kD`, dominant rate constant (used in `Algae_TKTD()`only) (d-1)
#'
#' @section Forcings:
#' Besides exposure events (Cw), the *Algae* model requires two environmental
#' properties as time-series input: Irradiance (`I`, μE/m²/s) and
#' temperature (`T_act`, deg C).
#' Forcings time-series are represented by `data.frame` objects
#' consisting of two columns. The first for time and the second for the
#' environmental factor in question. The input format for all forcings is a
#' list of the data frames.
#'
#' @section Simulation output:
#'
#' Simulation results will contain state variables.
#'
#' @references
#'Weber D, Schaeffer D, Dorgerloh M, Bruns E, Goerlitz G, Hammel K, Preuss TG
#'and Ratte HT, 2012. Combination of a higher-tier flow-through system and
#'population modeling to assess the effects of time-variable exposure of
#'isoproturon on the green algae Desmodesmus subspictatus and
#'Pseudokirchneriella subcapitata. Environmental Toxicology and
#'Chemistry, 31, 899–908.
#'

#' \linkS4class{Transferable},
#' \linkS4class{EffectScenario}

#' @export
Algae_TKTD <- function() {
  new("AlgaeTKTDScenario",
      name = "Algae_TKTD",
      param.req = c("mu_max", "m_max", "v_max", "k_s",
                    "Q_min", "Q_max",
                    "T_opt", "T_min", "T_max", "I_opt",
                    "EC_50", "b", "k", "kD", "dose_resp", "scaled"
      ),
      # default values as defined by Weber et al. (2012)
      param = list(mu_max = 1.7380, m_max = 0.0500, v_max = 0.0520,
                   k_s = 0.0680,
                   Q_min = 0.0011, Q_max = 0.0144,
                   T_opt = 27, T_min = 0, T_max = 35, I_opt = 120,
                   EC_50 = 115, b = 1.268, k = 0.2, kD = 0.1, dose_resp = 0
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
#' Constructor to ease creation of *Algae* ‘EffectScenario’s. Model is derived
#' from the *Algae* model by Weber (2012) as cited in EFSA TKTD opinion (2018).
#'
#' @section State variables:
#' The model has two state variables:
#' - `A`, Biomass
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
#' simplified model without additional forcings except the external concentration
#' and a forcing variable f_growth if the growth rates are time-dependent. If
#' not time dependent, f_growth should be set to 1.
#'
#' @section Simulation output:
#'
#' Simulation results will contain state variables.
#'
#' @references
#'Weber D, Schaeffer D, Dorgerloh M, Bruns E, Goerlitz G, Hammel K, Preuss TG
#'and Ratte HT, 2012. Combination of a higher-tier flow-through system and
#'population modeling to assess the effects of time-variable exposure of
#'isoproturon on the green algae Desmodesmus subspictatus and
#'Pseudokirchneriella subcapitata. Environmental Toxicology and
#'Chemistry, 31, 899–908.
#'

#' \linkS4class{Transferable},
#' \linkS4class{EffectScenario}

#' @export
Algae_Simple <- function() {
  new("AlgaeSimpleScenario",
      name = "Algae_Simple",
      param.req = c("mu_max", "EC_50", "b", "kD", "dose_response", "scaled"
      ),
      # default values as defined by Weber et al. (2012)
      param = list(mu_max = 1.7380,
                   EC_50 = 115, b = 1.268, kD = 0.1,
                   dose_response = 0, scaled = 0
      ),
      endpoints = c("A", "r"),
      forcings.req=c("C_in", "f_growth"),
      control.req = TRUE,
      init = c(A = 1, Dw = 0),
      transfer.interval = -1,
      transfer.biomass = 1,
      transfer.comp.biomass = "A"
  )
}
