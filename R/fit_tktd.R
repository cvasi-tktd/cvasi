# TODO output messages for fitted parameter(s), starting values, output variable
#      reuse outputs from calibrate()


#' Fit TK/TD parameters
#'
#' `r lifecycle::badge("experimental")` <br/>
#' High-level function to fit TK/TD parameters to observed data. It eases the
#' use of [calibrate()] by providing sensible defaults for various (model dependent)
#' settings.
#'
#' ### Data
#'
#' The function can be used in three basic ways: Fit parameters of
#'
#' 1) a single [scenario] to a data set
#' 2) a list of [caliset]s
#' 3) a list of [caliset]s in a chain of `fit` functions
#'
#' For option 1), scenario and data are supplied separately. In this case,
#' supported types of argument `data` are `data.frame`s and [tox_data] objects.
#' Any `data.frame` must have a format that is compatible with the [tox_data()]
#' function.
#'
#' In option 2), all conditions are fully described by a list of calibration sets.
#' The user is responsible to set up all [caliset] objects to their needs.
#'
#'
#' Option 3) is for convenience purposes and allows the chaining of fit functions.
#' The latter alternative accepts the return value of e.g. [fit_growth()]
#' as argument `x` and the fitted parameter values are applied to all
#' calibration sets in argument `data`.
#'
#' @param x a [scenario] or a list of [caliset] objects
#' @param data toxicological trial data to fit TK/TD parameters to: required if
#'      `x` is either a [scenario] or the result of a fit. See Section *Data*
#'      for details.
#' @param par named vector, of parameters to fit and their starting values
#' @param log_scale logical, if `TRUE` then fitting will be performed on log-transformed
#'   observations and predictions, else the data will be used as-is
#' @param verbose logical, if `TRUE` then info messages are printed to the console
#' @param ... additional arguments passed through to [calibrate()]
#' @aliases fit_tktd,ANY,ANY-method fit_tktd,list,missing-method fit_tktd,cvasi_fit,ANY-method
#'    fit_tktd,CalibrationSet,ANY-method fit_tktd,CalibrationSet,missing-method
#'    fit_tktd,EffectScenario,ANY-method fit_tktd,ScenarioSequence,ANY-method
#' @returns a list
#' @export
#' @examples
#' # Use experimental data from control trial ('T0') to fit growth rate
#' ctrl <- schmitt2013[schmitt2013$trial == "T0" ,]
#'
#' # Set up a scenario, provide dummy parameter values where necessary
#' sc <- Lemna_Schmitt() %>%
#'   set_init(c(BM=0.0012)) %>%
#'   set_param(c(EC50=1, b=1, P_up=1))
#'
#' # Run fitting routine
#' fit_growth(sc, data=ctrl, verbose=TRUE)
#'
#' # Use fitted growth parameter to adapt scenario
#' sc2 <- sc %>% set_param(c(k_phot_max=0.43925))
#'
#' # Use experimental ecotox data for various concentrations of 'metsulfuron-methyl'
#' trials <- schmitt2013[schmitt2013$trial != "T0" ,]
#'
#' # Fit remaining TK/TD parameters
#' fit_tktd(sc2, data=trials, verbose=TRUE)

setGeneric("fit_tktd", function(x, data, ...) standardGeneric("fit_tktd"))

# Default handler
#' @noRd
setMethod("fit_tktd", c("ANY", "ANY"), function(x, data, ...) stop("Object type or argument combination not supported"))

# Wrapper for [sequence]s
#' @noRd
setMethod("fit_tktd", c("ScenarioSequence", "ANY"), function(x, data, ...) fit_tktd_scenario(x=x, data=data, ...))

# Wrapper for [scenario]s
#' @noRd
setMethod("fit_tktd", c("EffectScenario", "ANY"), function(x, data, ...) fit_tktd_scenario(x=x, data=data, ...))

# Wrapper for list of caliset objects
#' @noRd
setMethod("fit_tktd", c("list", "missing"), function(x, data, ...) fit_tktd_list(x, ...))

#' @noRd
setMethod("fit_tktd", c("CalibrationSet", "missing"), function(x, data, ...) fit_tktd_caliset(x=x, ...))

# Make user aware of invalid argument combination
#' @noRd
setMethod("fit_tktd", c("CalibrationSet", "ANY"), function(x, data, ...)
  stop("Calisets (argument 'x') cannot be used together with argument 'data'")
)

#' @noRd
setOldClass("cvasi_fit")

# @describeIn fit_tktd Wrapper for chaining fit_* functions
#' @noRd
setMethod("fit_tktd", c("cvasi_fit", "ANY"), function(x, data, ...) fit_tktd_fit(x=x, data=data, ...))

# Wrapper for single scenario + dataset
fit_tktd_scenario <- function(x, data, ...) {
  # if scenario type not supported yet, we may inadvertently land here
  if(is.list(x) & all(is_caliset(x))) {
    stop("Scenario type not supported yet")
  }

  # normal call handling starts here...
  if(missing(data)) {
    stop("Argument 'data' is missing")
  }
  # check if dataset is represented by tabular data
  if(is.data.frame(data)) {
    data <- tox_data(data)
  }
  else if(is(data, "ToxData")) {
    # nothing to do
  } else {
    stop("Argument 'data' has unsupported type")
  }
  # create calisets
  x <- td2cs(x, data)
  fit_tktd(x, ...)
}

# Wrapper for single caliset objects
fit_tktd_caliset <- function(x, ...) {
  fit_tktd(list(x), ...)
}

# Wrapper for cvasi_fit objects, allows chaining of fit* functions, such as
# scenario %>%
#  fit_growth(data=calisets1) %>%
#  fit_tktd(data=calisets2)
fit_tktd_fit <- function(x, data, ...) {
  if(missing(data)) {
    stop("Argument 'data' is missing")
  }

  # check data type, only works for (list of) calisets
  if(!all(is_caliset(data))) {
    stop("Argument 'data' must only contain caliset objects")
  }
  if(!is.vector(data)) {
    data <- list(data)
  }
  # apply fitted parameter(s) to all scenarios
  for(i in seq_along(data)) {
    data[[i]]@scenario <- data[[i]]@scenario %>% set_param(x$par)
  }
  # continue with normal fitting
  fit_tktd_list(data)
}

# Wrapper for lists of calisets
#' @importFrom methods selectMethod
fit_tktd_list <- function(x, ...) {
  # check that list only contains caliset object
  if(!all(is_caliset(x))) {
    stop("Argument 'x' must only contain caliset objects")
  }
  # then use scenario in first caliset to decide which method to call
  sc <- x[[1]]@scenario
  if(is_sequence(x[[1]]@scenario))
    sc <- sc[[1]]

  cls <- class(sc)[1]
  fun <- selectMethod("fit_tktd", signature=c(x=cls, data="missing"))
  fun(x, ...)
}

#' @describeIn fit_tktd Fit TK/TD parameters of [Lemna_SETAC] scenarios
#' @include model-lemna_schmitt.R
#' @include model-lemna_setac.R
setMethod("fit_tktd", c("LemnaSetac", "missing"), function(x, data, par, log_scale=TRUE, verbose=FALSE, ...)
  fit_tktd_lemna_setac(x, data=data, par=par, log_scale=log_scale, verbose=verbose, ...))
#' @describeIn fit_tktd Fit TK/TD parameters of [Lemna_Schmitt] scenarios
setMethod("fit_tktd", c("LemnaSchmitt", "missing"), function(x, data, par, log_scale=TRUE, verbose=FALSE, ...)
  fit_tktd_lemna_schmitt(x, data=data, par=par, log_scale=log_scale, verbose=verbose, ...))

#' @autoglobal
fit_tktd_lemna_setac <- function(x, par, data, log_scale, verbose=TRUE,
                                 lower=NULL, upper=NULL, ...) {
  # TODO this is not a good solution, as every model-dep function must check this.
  #      better to not pass arguments through to calibrate() in the future
  if(!is.null(lower)) {
    stop("Argument 'lower' is not allowed, use `set_bounds()` instead")
  }
  if(!is.null(upper)) {
    stop("Argument 'upper' is not allowed, use `set_bounds()` instead")
  }

  if(any(is_scenario(x))) {
    stop("Argument 'x' is a scenario and must be combined with argument 'data'")
  }
  if(!missing(data)) {
    stop("Argument 'data' not supported here")
  }
  # is it a list of calisets?
  if(is.vector(x)) {
    if(!all(is_caliset(x))) {
      stop("Argument 'x' must only contain calisets")
    }
  }

  for(i in seq_along(x)) {
    # check if all datasets start at time=0, which is an implicit assumption in
    # the following calculation of statistics etc
    if(min(x[[1]]@data[, 1]) != 0) {
      stop("Scenarios/datasets must start at time zero (t=0)")
    }
  }

  # if no starting value set, use default values
  if(missing(par)) {
    par <- c(EC50_int=1, b=2, P=0.1)
  }
  if(is.list(par)) {
    par <- unlist(par)
  }
  # check for invalid values
  if(any(is.na(par) | is.nan(par) | is.infinite(par))) {
    stop("Starting values contain invalid values")
  }

  # determine variable to fit on
  output_nm <- "FrondNo"

  # run fitting routine
  fit <- calibrate(x, par=par, output=output_nm, hessian=TRUE, log_scale=log_scale,
                   err_fun="sse", verbose=verbose, ...)

  if(length(fit$par) == 0) {
    warning("Optimized parameters missing, cannot compute residuals")
    return(fit)
  }

  # remove data points at time==0, as these are the initial conditions and not predictions
  df <- fit$data %>% dplyr::filter(time > 0)

  ## Calculate Statistics
  fit$shapiro <- stats::shapiro.test(fit$data$residuals)

  # Degrees of Freedom: number of data points minus number of fitted parameters, excluding t=0
  fit$dof <- nrow(df) - length(par)

  # limits of the 95% confidence interval
  # TODO likelihood profiling as alternative
  fit$ci <- ci_from_hessian(fit, dof=fit$dof, level=0.95)

  # NRMSE
  fit$nrmse <-  1 / mean(df[, 2]) * sqrt(1 / nrow(df) * sum((df[, 2] - df[, 3])^2))

  fit
}

fit_tktd_lemna_schmitt <- function(x, par, ...) {
  if(missing(par)) {
    par <-  c(EC50=1, b=2, P_up=0.1)
  }
  fit_tktd_lemna_setac(x=x, par=par, ...)
}

#' @describeIn fit_tktd Fit TK/TD parameters of [Magma] scenarios
#' @include model-magma.R
setMethod("fit_tktd", c("Magma", "missing"), function(x, data, par, log_scale=TRUE, verbose=FALSE, ...)
  fit_tktd_magma(x, data=data, par=par, log_scale=log_scale, verbose=verbose, ...))

#' @autoglobal
fit_tktd_magma <- function(x, par, data, log_scale, verbose=TRUE, lower=NULL, upper=NULL, ...) {
  # TODO this is not a good solution, as every model-dep function must check this.
  #      better to not pass arguments through to calibrate() in the future
  if(!is.null(lower)) {
    stop("Argument 'lower' is not allowed, use `set_bounds()` instead")
  }
  if(!is.null(upper)) {
    stop("Argument 'upper' is not allowed, use `set_bounds()` instead")
  }

  if(any(is_scenario(x))) {
    stop("Argument 'x' is a scenario and must be combined with argument 'data'")
  }
  if(!missing(data)) {
    stop("Argument 'data' not supported here")
  }
  # is it a list of calisets?
  if(is.vector(x)) {
    if(!all(is_caliset(x))) {
      stop("Argument 'x' must only contain calisets")
    }
  }

  for(i in seq_along(x)) {
    # check if all datasets start at time=0, which is an implicit assumption in
    # the following calculation of statistics etc
    if(min(x[[1]]@data[, 1]) != 0) {
      stop("Scenarios/datasets must start at time zero (t=0)")
    }
  }

  # if no starting value set, use default values
  if(missing(par)) {
    par <- c(EC50_int=1, E_max=1, b=1)
  }
  if(is.list(par)) {
    par <- unlist(par)
  }
  # check for invalid values
  if(any(is.na(par) | is.nan(par) | is.infinite(par))) {
    stop("Starting values contain invalid values")
  }

  # determine variable to fit on
  output_nm <- "FrondNo"

  # run fitting routine
  fit <- calibrate(x, par=par, output=output_nm, hessian=TRUE, log_scale=log_scale,
                   err_fun="sse", verbose=verbose, ...)

  if(length(fit$par) == 0) {
    warning("Optimized parameters missing, cannot compute residuals")
    return(fit)
  }

  # remove data points at time==0, as these are the initial conditions and not predictions
  df <- fit$data %>% dplyr::filter(time > 0)

  ## Calculate Statistics
  fit$shapiro <- stats::shapiro.test(fit$data$residuals)

  # Degrees of Freedom: number of data points minus number of fitted parameters, excluding t=0
  fit$dof <- nrow(df) - length(par)

  # limits of the 95% confidence interval
  # TODO likelihood profiling as alternative
  fit$ci <- ci_from_hessian(fit, dof=fit$dof, level=0.95)

  # NRMSE
  fit$nrmse <-  1 / mean(df[, 2]) * sqrt(1 / nrow(df) * sum((df[, 2] - df[, 3])^2))

  fit
}
