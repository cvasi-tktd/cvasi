#' Calls ODE solver for a particular model
#'
#' Please refer to the *Modeling Howto* vignette on how to implement custom
#' models by overloading the `solver` function.
#'
#' Please note that not all solvers support setting the parameters listed here.
#' In addition, some solvers may set reasonable default values for e.g. maximum step
#' length in time, but not all do. Please check the model documentation for
#' details.
#'
#' @param scenario [scenario] object
#' @param times numeric vector of output times, overrides any scenario setting
#' @param ... additional parameters passed on to [deSolve::ode()]
#' @param approx string, interpolation method of exposure series, see [deSolve::forcings]
#' @param f if `approx="constant"`, a number between 0 and 1 inclusive, see [deSolve::forcings]
#' @param rule if `approx="constant"`, a number between 0 and 1 inclusive, see [deSolve::forcings]
#' @param method string, numerical solver used by [deSolve::ode()]
#' @param hmax numeric, maximum step length in time, see [deSolve::ode()]
#' @return `data.frame` with simulation results
#' @export
setGeneric("solver",
           function(scenario, times, ...) standardGeneric("solver"),
           signature = "scenario"
)

# Default solver which uses the model's name to switch between solver calls
solver_default <- function(scenario, times, ...) {
  stop("unknown model type, cannot simulate scenario")
}
#' @describeIn solver Default solver, raises an error
setMethod("solver", "ANY", solver_default)

# Solver for MyrioExp scenarios
solver_MyrioExp <- function(scenario, ...) {
  # Constant identifying exponential growth model
  scenario@param$growthno <- 1
  # Dummy value, only used for logistic growth
  scenario@param$BM_L <- NA_real_

  solver_Myrio(scenario, ...)
}
#' @include class-Myriophyllum.R
#' @describeIn solver Numerically integrates Myrio models
setMethod("solver", "MyrioExpScenario", function(scenario, times, ...) solver_MyrioExp(scenario, times, ...))

# Solver for MyrioLog scenarios
solver_MyrioLog <- function(scenario, ...) {
  # Constant identifying logistic growth model
  scenario@param$growthno <- 2
  solver_Myrio(scenario, ...)
}
#' @include class-Myriophyllum.R
#' @describeIn solver Numerically integrates Myrio_log models
setMethod("solver", "MyrioLogScenario", function(scenario, times, ...) solver_MyrioLog(scenario, times, ...))

# Numerically solve Myriophyllum scenarios
#
# @param scenario
# @param times
# @param ... additional parameters passed on to `solve_Lemna_SETAC()`
# @return data.frame
solver_Myrio <- function(scenario, times, approx=c("linear","constant"),
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

#' @importFrom deSolve ode
solver_DEB_abj <- function(scenario, times, approx=c("linear","constant"), f=1,
                           method="lsoda", ...) {
  # use time points from scenario if nothing else is provided
  if(missing(times))
    times <- scenario@times
  # check if at least two time points are present
  if(length(times)<2) stop("times vector is not an interval")

  approx <- match.arg(approx)
  # make sure that parameters are present and in required order
  params.req <- c("p_M","v","k_J","p_Am","kap","E_G","f","E_Hj","E_Hp","kap_R","ke","c0",
                  "cT","L_b","L_j","MoA")
  # additional output variables
  outnames <- c("pC","pA","pJ","MV")

  params <- scenario@param
  if(is.list(params))
    params <- unlist(params)

  # check for missing parameters
  params.missing <- setdiff(params.req, names(params))
  if(length(params.missing)>0)
    stop(paste("missing parameters:",paste(params.missing,sep=",",collapse=",")))

  # reorder parameters for deSolve
  params <- params[params.req]

  # create forcings list
  forcings <- list(scenario@exposure@series)

  # run solver
  as.data.frame(ode(y=scenario@init, times=times, parms=params, method=method,
      dllname="cvasi", initfunc="deb_abj_init", func="deb_abj_func", initforc="deb_abj_forc",
      forcings=forcings, fcontrol=list(method=approx, rule=2, f=f, ties="ordered"),
      outnames=outnames, ...))
}
#' @include class-Deb.R
#' @describeIn solver Numerically integrates DEB_abj models
#' @importFrom utils head tail
#' @importFrom deSolve ode
setMethod("solver", "DebAbj", function(scenario, times, ...) solver_DEB_abj(scenario, times, ...))


# Solver function for Algae_Weber models
# @param scenario Scenario object
# @param times numeric vector, time points for result set
# @param approx string, interpolation method of exposure series, see [stats::approxfun()]
# @param f if `approx="constant"`, a number between 0 and 1 inclusive, see [stats::approxfun()]
# @param rule how to handle data points outside of time-series, see [deSolve::forcings]
# @param method string, numerical solver used by [deSolve::ode()]
# @param hmax numeric, maximum step length in time, see [deSolve::ode()]
# @param ... additional arguments passed to [deSolve::ode()]
#' @importFrom deSolve ode
solver_Algae_Weber <- function(scenario, times, approx = c("linear","constant"),
                                 f = 1, method = "lsoda", hmax = 0.1, ...) {
  # use time points from scenario if nothing else is provided
  if(missing(times))
    times <- scenario@times
  # check if at least two time points are present
  if(length(times)<2)
    stop("times vector is not an interval")

  params.req = c("mu_max", "m_max", "v_max", "k_s",
                "Q_min", "Q_max", "R_0", "D",
                "T_opt", "T_min", "T_max", "I_opt",
                "EC_50", "b", "k"
  )

  params <- scenario@param
  if(is.list(params))
    params <- unlist(params)

  # reorder parameters for deSolve
  params <- params[params.req]

  if(is.list(params)) params <- unlist(params)
  approx <- match.arg(approx)

  # create forcings list
  forcings <- list(
    scenario@exposure@series,
    scenario@forcings$I,
    scenario@forcings$T_act
    )

  # set names of additional output variables
  outnames <- c("Cin", "I", "Tact", "dA", "dQ", "dP", "dC")

  # run solver
  as.data.frame(ode(y = scenario@init, times,
    initfunc = "algae_init",
    func = "algae_func",
    initforc = "algae_forc",
    parms = params,
    forcings = forcings,
    fcontrol = list(rule = 2, method = approx, f = f, ties = "ordered"),
    dllname = "cvasi",
    method = method,
    hmax = hmax,
    outnames = outnames,
    ...))
}

#' @include class-Algae.R
#' @describeIn solver numerically integrates Algae_Weber models
setMethod("solver", "AlgaeWeberScenario", solver_Algae_Weber)

# Solver function for Algae_TKTD models
# @param scenario Scenario object
# @param times numeric vector, time points for result set
# @param approx string, interpolation method of exposure series, see [stats::approxfun()]
# @param f if `approx="constant"`, a number between 0 and 1 inclusive, see [stats::approxfun()]
# @param rule how to handle data points outside of time-series, see [deSolve::forcings]
# @param method string, numerical solver used by [deSolve::ode()]
# @param hmax numeric, maximum step length in time, see [deSolve::ode()]
# @param ... additional arguments passed to [deSolve::ode()]
#' @importFrom deSolve ode
solver_Algae_TKTD <- function(scenario, times, approx = c("linear","constant"),
                               f = 1, method = "lsoda", hmax = 0.1, ...) {
  # use time points from scenario if nothing else is provided
  if(missing(times))
    times <- scenario@times
  # check if at least two time points are present
  if(length(times)<2)
    stop("times vector is not an interval")

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

  if(is.list(params)) params <- unlist(params)
  approx <- match.arg(approx)

  # set names of additional output variables
  outnames <- c("Cw", "I", "Tact", "dA", "dQ", "dP", "dDw")

  # run solver
  as.data.frame(ode(y = scenario@init, times,
    initfunc = "algae_TKTD_init",
    func = "algae_TKTD_func",
    initforc = "algae_TKTD_forc",
    parms = params,
    forcings = forcings,
    fcontrol = list(rule = 2, method = approx, f = f, ties = "ordered"),
    dllname = "cvasi",
    method = method,
    hmax = hmax,
    outnames = outnames,
    ...))
}

#' @include class-Algae.R
#' @describeIn solver numerically integrates Algae_TKTD models
setMethod("solver", "AlgaeTKTDScenario", solver_Algae_TKTD)


# Solver function for Algae_Weber models
# @param scenario Scenario object
# @param times numeric vector, time points for result set
# @param approx string, interpolation method of exposure series, see [stats::approxfun()]
# @param f if `approx="constant"`, a number between 0 and 1 inclusive, see [stats::approxfun()]
# @param rule how to handle data points outside of time-series, see [deSolve::forcings]
# @param method string, numerical solver used by [deSolve::ode()]
# @param hmax numeric, maximum step length in time, see [deSolve::ode()]
# @param ... additional arguments passed to [deSolve::ode()]
#' @importFrom deSolve ode
solver_Algae_Simple <- function(scenario, times, approx = c("linear","constant"),
                                f = 1, method = "ode45", hmax = 0.01, ...) {
  # use time points from scenario if nothing else is provided
  if(missing(times))
    times <- scenario@times
  # check if at least two time points are present
  if(length(times)<2)
    stop("times vector is not an interval")

  params <- scenario@param
  if(is.list(params))
    params <- unlist(params)

  # create forcings list
  forcings <- list(scenario@exposure@series, scenario@forcings$f_growth)

  #required for C code
  params.req = c("mu_max",
                 "EC_50", "b", "kD",
                 "scaled", "dose_response"
  )
  # reorder parameters for deSolve
  params <- params[params.req]

  if(is.list(params)) params <- unlist(params)
  approx <- match.arg(approx)

  # set names of additional output variables
  outnames <- c("dA", "dDw", "dose_response", "scaled", "f_growth")

  # run solver
  as.data.frame(ode(y = scenario@init, times,
    initfunc = "algae_simple_init",
    func = "algae_simple_func",
    initforc = "algae_simple_forc",
    parms = params,
    forcings = forcings,
    fcontrol = list(rule = 2, method = approx, f = f, ties = "ordered"),
    dllname = "cvasi",
    method = method,
    hmax = hmax,
    outnames = outnames,
    ...))
}

#' @include class-Algae.R
#' @describeIn solver numerically integrates Algae_Simple models
setMethod("solver", "AlgaeSimpleScenario", solver_Algae_Simple)
