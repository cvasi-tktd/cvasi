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

# @param scenario Scenario object
# @param times numeric vector, time points for result set
# @param approx string, interpolation method of exposure series, see [stats::approxfun()]
# @param hmax numeric, maximum step length in time, see [deSolve::ode()]
# @param ... additional arguments passed to [deSolve::ode()]
# @param approx string, interpolation method of exposure series, see [stats::approxfun()]
# @param f if `approx="constant"`, a number between 0 and 1 inclusive, see [stats::approxfun()]
# @param method string, numerical solver used by [deSolve::ode()]
#' @importFrom deSolve ode
solver_GUTS_RED_SD <- function(scenario, times, approx=c("linear","constant"),
                               f=1, method="lsoda", hmax=1, ...) {
  # use time points from scenario if nothing else is provided
  if(missing(times))
    times <- scenario@times
  # check if at least two time points are present
  if(length(times)<2) stop("times vector is not an interval")

  params <- scenario@param
  if(is.list(params))
    params <- unlist(params)
  approx <- match.arg(approx)

  # make sure that parameters are present and in required order
  params <- params[c("kd", "hb", "z", "kk")]

  df <- as.data.frame(ode(y=scenario@init, times=times, parms=params, dllname="cvasi",
      initfunc="gutsredsd_init", func="gutsredsd_func", initforc="gutsredsd_forc",
      forcings=scenario@exposure@series, fcontrol=list(method=approx, rule=2, f=f, ties="ordered"),
      outnames=c("Cw"), method=method, hmax=hmax, ...))
  # Derive survival probability, see EFSA Scientific Opinion on TKTD models, p. 33
  # doi:10.2903/j.efsa.2018.5377
  df$S <- exp(-df$H) # background hazard rate included in H (if enabled)
  df
}
#' @include class-GutsRed.R
#' @describeIn solver Numerically integrates GUTS-RED-SD models
setMethod("solver", "GutsRedSd", function(scenario, times, ...) solver_GUTS_RED_SD(scenario, times, ...))

# @param scenario Scenario object
# @param times numeric vector, time points for result set
# @param approx string, interpolation method of exposure series, see [stats::approxfun()]
# @param f if `approx="constant"`, a number between 0 and 1 inclusive, see [stats::approxfun()]
# @param method string, numerical solver used by [deSolve::ode()]
# @param hmax numeric, maximum step length in time, see [deSolve::ode()]
# @param ... additional arguments passed to [deSolve::ode()]
#' @importFrom deSolve ode
solver_GUTS_RED_IT <- function(scenario, times, approx=c("linear","constant"),
                               f=1, method="lsoda", hmax=1, ...) {
  # use time points from scenario if nothing else is provided
  if(missing(times))
    times <- scenario@times
  # check if at least two time points are present
  if(length(times)<2) stop("times vector is not an interval")

  params <- scenario@param
  if(is.list(params))
    params <- unlist(params)
  approx <- match.arg(approx)

  # make sure that parameters are present and in required order
  odeparams <- params[c("kd","hb")]

  df <- as.data.frame(ode(y=scenario@init, times=times, parms=odeparams, dllname="cvasi",
                    initfunc="gutsredit_init", func="gutsredit_func", initforc="gutsredit_forc",
                    forcings=scenario@exposure@series, fcontrol=list(method=approx, rule=2, f=f, ties="ordered"),
                    outnames=c("Cw"), method=method, hmax=hmax, ...))
  # Derive survival probability, EFSA Scientific Opinion on TKTD models, p. 33
  # doi:10.2903/j.efsa.2018.5377
  FS <- (1 / (1 + (cummax(df$D) / params["alpha"])^(-params["beta"])))
  df$S <- (1 - FS) * exp(-df$H)
  df
}
#' @include class-GutsRed.R
#' @describeIn solver Numerically integrates GUTS-RED-IT models
setMethod("solver", "GutsRedIt", function(scenario, times, ...) solver_GUTS_RED_IT(scenario, times, ...) )

# @param scenario Scenario object
# @param times numeric vector, time points for result set
# @param approx string, interpolation method of exposure series, see [stats::approxfun()]
# @param f if `approx="constant"`, a number between 0 and 1 inclusive, see [stats::approxfun()]
# @param nout `numeric`, number of additional output variables, `nout=1` appends
#   the internal concentration `C_int`, the maximum number is 13
# @param method string, numerical solver used by [deSolve::ode()]
# @param hmax numeric, maximum step length in time, see [deSolve::ode()]
# @param ... additional arguments passed to [deSolve::ode()]
#' @importFrom deSolve ode
solver_Lemna_Schmitt <- function(scenario, times, approx=c("linear","constant"),
                                 f=1, nout=2, method="ode45", hmax=0.1, ...) {
  # use time points from scenario if nothing else is provided
  if(missing(times))
    times <- scenario@times
  # check if at least two time points are present
  if(length(times)<2)
    stop("times vector is not an interval")

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
    stop(paste("parameter out of bounds:",paste(names(params)[which(head(params,-1)<0)],sep=",")))

  # create forcings list
  # TODO check if it can be activated without issues
  #if(params["k_phot_fix"]==FALSE)
  forcings <- list(scenario@exposure@series, scenario@forcings$temp, scenario@forcings$rad)
  #else  # temp and radiation are not required by model if k_phot_fix==TRUE
  #  forcings <- list(exposure, data.frame(t=0,temp=-1), data.frame(t=0,rad=-1))

  # run solver
  as.data.frame(ode(y=scenario@init, times=times, parms=params, dllname="cvasi",
                    initfunc="lemna_init", func="lemna_func", initforc="lemna_forc",
                    forcings=forcings, fcontrol=list(method=approx, rule=2, f=f, ties="ordered"),
                    nout=nout, outnames=outnames, method=method, hmax=hmax, ...))
}
#' @include class-Lemna.R
#' @describeIn solver Numerically integrates Lemna_Schmitt models
setMethod("solver", "LemnaSchmittScenario", function(scenario, times, ...) solver_Lemna_Schmitt(scenario, times, ...) )

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
### disabled for now @importFrom lemna lemna_desolve param_new
solver_Lemna_SETAC <- function(scenario, times, approx = c("linear","constant"),
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
    stop(paste("missing parameters:",paste(params.missing,sep=",",collapse=",")))

  # reorder parameters for deSolve
  params <- params[names(lemna::param_new())]
  # check if any parameter is negative
  if(any(params<0, na.rm=TRUE))
    stop(paste("parameter out of bounds:",paste(names(params)[which(params<0)],sep=",")))

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

  # run deSolve
  as.data.frame(lemna::lemna_desolve(y=scenario@init, times=times, parms=params, forcings=forcings,
    fcontrol=list(method=approx, rule=2, f=f, ties="ordered"), nout=nout,
    method=method, hmax=hmax, ...))
}
#' @include class-Lemna.R
#' @describeIn solver Numerically integrates Lemna_SETAC models
setMethod("solver", "LemnaSetacScenario", function(scenario, times, ...) solver_Lemna_SETAC(scenario, times, ...) )


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
