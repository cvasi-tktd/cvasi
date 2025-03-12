########################
## Organizing man pages
########################

#' Algae models
#'
#' Overview of supported *Algae* models
#'
#' - [Algae_Weber()] by Weber *et al.* (2012)
#' - [Algae_TKTD()] based on Weber *et al.* (2012), but with scaled damage
#' - [Algae_Simple()] simplified growth model without additional forcing variables
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
#'
#' EFSA Panel on Plant Protection Products and their Residues, 2018. Scientific
#' opinion on the state of the art of Toxicokinetic/Toxicodynamic (TKTD) effect
#' models for regulatory risk assessment of pesticides for aquatic organisms.
#' EFSA journal 16:5377 \doi{10.2903/j.efsa.2018.5377}
#'
#' @name Algae-models
#' @family algae models
#' @family models
#' @seealso [Lemna-models], [Transferable]
#' @include class-Transferable.R
#' @aliases Algae-class
NULL


########################
## Class definitions
########################

# Generic Algae class
#' @export
setClass("Algae", contains = c("Transferable", "EffectScenario"))

# Algae model with exponential growth and forcings (P, I)
#' @export
setClass("AlgaeWeber", contains = "Algae")
# for backwards compatibility
#' @export
setClass("AlgaeWeberScenario", contains = "AlgaeWeber")

# Algae model with exponential growth and scaled internal damage
#' @export
setClass("AlgaeTKTD", contains = "Algae")
# for backwards compatibility
#' @export
setClass("AlgaeTKTDScenario", contains = "AlgaeTKTD")

# Algae model with exponential growth without forcings
#' @export
setClass("AlgaeSimple", contains = "Algae")
# for backwards compatibility
#' @export
setClass("AlgaeSimpleScenario", contains = "AlgaeSimple")


########################
## Constructor
########################

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
#' concentration (`C`).
#'
#' It is possible to amend the output of [simulate()] with additional model
#' quantities that are not state variables, for e.g. debugging purposes or to
#' analyze model behavior. To enable or disable additional outputs, use the
#' optional argument `nout` of [simulate()]. As an example, set `nout=2` to
#' enable reporting of model derivatives `dA` and `dQ`. Set `nout=0` to disable
#' additional outputs (default).
#'
#' The available output levels are as follows:
#'
#' - Derivatives
#'    - `nout >= 1`: `dA`, biomass derivative (µg)
#'    - `nout >= 2`: `dQ`, internal phosphorous derivative (mg P/ug fresh wt)
#'    - `nout >= 3`: `dP`, external phosphorous derivative (mg P L-1)
#'    - `nout >= 4`: `dC`, external concentration derivative (ug L-1)
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
#' Some default settings of *deSolve* were adapted for this model by expert
#' judgement to enable precise, but also computationally efficient, simulations
#' for most model parameters. These settings can be modified by the user,
#' if needed:
#'
#' - `hmax = 0.1`<br>
#'    Maximum step length in time suitable for most simulations.
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
#' @return an S4 object of type [AlgaeWeber-class]
#' @seealso [Scenarios], [Transferable]
#' @family algae models
#' @export
#' @aliases AlgaeWeber-class AlgaeWeberScenario-class
Algae_Weber <- function() {
  new("AlgaeWeber",
      name = "Algae_Weber",
      param.req = c("mu_max", "m_max", "v_max", "k_s", "Q_min", "Q_max", "R_0", "D",
                    "T_opt", "T_min", "T_max", "I_opt", "EC_50", "b", "k"
      ),
      # default values as defined by Weber et al. (2012)
      param = list(mu_max = 1.7380, m_max = 0.0500, v_max = 0.0520, k_s = 0.0680,
                   Q_min = 0.0011, Q_max = 0.0144, R_0 = 0.36, D = 0.5,
                   T_opt = 27, T_min = 0, T_max = 35, I_opt = 120
      ),
      # boundary presets defined by expert judgement
      param.bounds = list(mu_max=c(0, 4),  EC_50=c(0, 1e6), b=c(0.1, 20)),
      endpoints = c("A", "r"),
      forcings.req=c("T_act", "I"),
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
#' concentration (`Dw`).
#'
#' It is possible to amend the output of [simulate()] with additional model
#' quantities that are not state variables, for e.g. debugging purposes or to
#' analyze model behavior. To enable or disable additional outputs, use the
#' optional argument `nout` of [simulate()]. As an example, set `nout=2` to
#' enable reporting of model derivatives `dA` and `dQ`. Set `nout=0` to disable
#' additional outputs (default).
#'
#' The available output levels are as follows:
#' - Derivatives
#'    - `nout >= 1`: `dA`, biomass derivative (µg)
#'    - `nout >= 2`: `dQ`, internal phosphorous derivative (mg P/ug fresh wt)
#'    - `nout >= 3`: `dP`, external phosphorous derivative (mg P L-1)
#'    - `nout >= 4`: `dDw`, damage concentration derivative (ug L-1)
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
#' Some default settings of *deSolve* were adapted for this model by expert
#' judgement to enable precise, but also computationally efficient, simulations
#' for most model parameters. These settings can be modified by the user,
#' if needed:
#'
#' - `hmax = 0.1`<br>
#'    Maximum step length in time suitable for most simulations.
#'
#' @references
#' Weber D, Schaeffer D, Dorgerloh M, Bruns E, Goerlitz G, Hammel K, Preuss TG
#' and Ratte HT, 2012. Combination of a higher-tier flow-through system and
#' population modeling to assess the effects of time-variable exposure of
#' isoproturon on the green algae Desmodesmus subspictatus and
#' Pseudokirchneriella subcapitata. Environmental Toxicology and
#' Chemistry, 31, 899-908. \doi{10.1002/etc.1765}
#'
#' @return an S4 object of type [AlgaeTKTD-class]
#' @seealso [Scenarios], [Transferable]
#' @family algae models
#' @export
#' @aliases AlgaeTKTD-class AlgaeTKTDScenario-class
Algae_TKTD <- function() {
  new("AlgaeTKTD",
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
      # boundary presets defined by expert judgement
      param.bounds = list(mu_max=c(0, 4),  EC_50=c(0, 1e6), b=c(0.1, 20), kD=c(0, 10)),
      endpoints = c("A", "r"),
      forcings.req=c("T_act", "I"),
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
#' Simulation results will contain the state variables biomass (`A`) and
#' scaled damage concentration (`Dw`).
#'
#' It is possible to amend the output of [simulate()] with additional model
#' quantities that are not state variables, for e.g. debugging purposes or to
#' analyze model behavior. To enable or disable additional outputs, use the
#' optional argument `nout` of [simulate()]. As an example, set `nout=2` to
#' enable reporting of external concentration (`Cw`) and growth scaling factor
#' (`f_growth`). Set `nout=0` to disable additional outputs (default).
#'
#' The available output levels are as follows:
#' - `nout >= 1`: `Cw` external concentration (ug L-1)
#' - `nout >= 2`: `f_growth` growth scaling factor (-)
#' - `nout >= 3`: `dA`, biomass derivative (µg)
#' - `nout >= 4`: `dDw`, damage concentration derivative (ug L-1)
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
#' Some default settings of *deSolve* were adapted for this model by expert
#' judgement to enable precise, but also computationally efficient, simulations
#' for most model parameters. These settings can be modified by the user,
#' if needed:
#'
#' - `hmax = 0.01`<br>
#'    Maximum step length in time suitable for most simulations.
#'
#' @references
#' Weber D, Schaeffer D, Dorgerloh M, Bruns E, Goerlitz G, Hammel K, Preuss TG
#' and Ratte HT, 2012. Combination of a higher-tier flow-through system and
#' population modeling to assess the effects of time-variable exposure of
#' isoproturon on the green algae Desmodesmus subspictatus and
#' Pseudokirchneriella subcapitata. Environmental Toxicology and
#' Chemistry, 31, 899-908. \doi{10.1002/etc.1765}
#'
#' @return an S4 object of type [AlgaeSimple-class]
#' @seealso [Scenarios], [Transferable]
#' @family algae models
#' @export
#' @aliases AlgaeSimple-class AlgaeSimpleScenario-class
Algae_Simple <- function() {
  new("AlgaeSimple",
      name = "Algae_Simple",
      param.req = c("mu_max", "EC_50", "b", "kD", "dose_response", "scaled"),
      # default values as defined by Weber et al. (2012)
      param = list(mu_max = 1.7380, dose_response = 0, scaled = 0),
      # boundary presets defined by expert judgement
      param.bounds = list(mu_max=c(0, 4), EC_50=c(0, 1e6), b=c(0.1, 20), kD=c(0, 10)),
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


########################
## Simulation
########################

# Solver function for Algae_Weber models
# @param scenario Scenario object
# @param method string, numerical solver used by [deSolve::ode()]
# @param hmax numeric, maximum step length in time, see [deSolve::ode()]
# @param ... additional arguments passed to [deSolve::ode()]
#' @importFrom deSolve ode
solver_algae_weber <- function(scenario, method = "lsoda", hmax = 0.1, ...) {
  params.req = c("mu_max", "m_max", "v_max", "k_s", "Q_min", "Q_max", "R_0", "D",
                 "T_opt", "T_min", "T_max", "I_opt", "EC_50", "b", "k"
  )

  params <- scenario@param
  if(is.list(params))
    params <- unlist(params)

  # reorder parameters for deSolve
  params <- params[params.req]

  # create forcings list
  forcings <- list(
    scenario@exposure@series,
    scenario@forcings$I,
    scenario@forcings$T_act
  )

  # set names of additional output variables
  outnames <- c("dA", "dQ", "dP", "dC")

  # run solver
  as.data.frame(ode(y = scenario@init, times=scenario@times, initfunc = "algae_init",
                    func = "algae_func", initforc = "algae_forc", parms = params,
                    forcings = forcings,
                    dllname = "cvasi", method = method, hmax = hmax, outnames = outnames,
                    ...))
}

#' @include solver.R
#' @describeIn solver numerically integrates Algae_Weber models
setMethod("solver", "AlgaeWeber", solver_algae_weber)

# Solver function for Algae_TKTD models
# @param scenario Scenario object
# @param method string, numerical solver used by [deSolve::ode()]
# @param hmax numeric, maximum step length in time, see [deSolve::ode()]
# @param ... additional arguments passed to [deSolve::ode()]
#' @importFrom deSolve ode
solver_algae_tktd <- function(scenario, method = "lsoda", hmax = 0.1, ...) {
  params.req = c("mu_max", "m_max", "v_max", "k_s",
                 "Q_min", "Q_max",
                 "T_opt", "T_min", "T_max", "I_opt",
                 "EC_50", "b", "kD", "dose_resp"
  )

  params <- scenario@param
  if(is.list(params))
    params <- unlist(params)

  # reorder parameters for deSolve
  params <- params[params.req]

  # create forcings list
  forcings <- list(
    scenario@exposure@series,
    scenario@forcings$I,
    scenario@forcings$T_act
  )

  # set names of additional output variables
  outnames <- c("dA", "dQ", "dP", "dDw")

  # run solver
  as.data.frame(ode(y = scenario@init, times=scenario@times, initfunc = "algae_TKTD_init",
                    func = "algae_TKTD_func", initforc = "algae_TKTD_forc",
                    parms = params, forcings = forcings,
                    dllname = "cvasi", method = method, hmax = hmax, outnames = outnames,
                    ...))
}

#' @describeIn solver numerically integrates Algae_TKTD models
setMethod("solver", "AlgaeTKTD", solver_algae_tktd)


# Solver function for Algae_Weber models
# @param scenario Scenario object
# @param method string, numerical solver used by [deSolve::ode()]
# @param hmax numeric, maximum step length in time, see [deSolve::ode()]
# @param ... additional arguments passed to [deSolve::ode()]
#' @importFrom deSolve ode
solver_algae_simple <- function(scenario, method = "lsoda", hmax = 0.1, ...) {
  params <- scenario@param
  if(is.list(params))
    params <- unlist(params)

  # create forcings list
  forcings <- list(scenario@exposure@series, scenario@forcings$f_growth)

  #required for C code
  params.req = c("mu_max", "EC_50", "b", "kD", "scaled", "dose_response")
  # reorder parameters for deSolve
  params <- params[params.req]

  # set names of additional output variables
  outnames <- c("Cw", "f_growth", "dA", "dDw")

  # run solver
  as.data.frame(ode(y = scenario@init, times=scenario@times, initfunc = "algae_simple_init",
                    func = "algae_simple_func", initforc = "algae_simple_forc",
                    parms = params, forcings = forcings,
                    dllname = "cvasi", method = method, hmax = hmax, outnames = outnames,
                    ...))
}

#' @describeIn solver numerically integrates Algae_Simple models
setMethod("solver", "AlgaeSimple", solver_algae_simple)


########################
## Effects
########################

# Calculate effect of Algae scenario
fx_algae <- function(scenario, ...) {
  efx_r <- "r" %in% scenario@endpoints
  # TODO move to a validate_scenario function, this takes precious time on every effect() call
  if(efx_r & has_transfer(scenario))
    stop("endpoint r is incompatible with biomass transfers")

  out <- simulate(scenario, ...)

  efx <- c("A"=tail(out$A, 1))
  if(efx_r) # we skip the log() operation if we can
    efx["r"] <- log(tail(out$A,1) / out$A[1]) / (tail(out[,1],1) - out[1,1])

  efx
}

#' @include fx.R
#' @describeIn fx Effect at end of simulation of [Algae-models]
setMethod("fx", "Algae", function(scenario, ...) fx_algae(scenario, ...))
