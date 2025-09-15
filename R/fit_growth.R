# TOOD support algae and other primary producer models as well
# TODO output messages for fitted parameter(s), starting values, output variable
#      reuse outputs from calibrate()


#' Fit growth parameters
#'
#' `r lifecycle::badge("experimental")` <br />
#' High-level function to fit growth and/or loss parameters to observed data. It eases the
#' use of [calibrate()] by providing sensible defaults for various (model dependent)
#' settings.
#'
#' @param x a [scenario] or a list of [caliset] objects
#' @param data toxicological trial data to fit growth parameters to: required if
#'      `x` is either a [scenario] or the result of a fit. See Section *Data*
#'      for details.
#' @param par named vector, of parameters to fit and their starting values
#' @param log_scale logical, if `TRUE` then fitting will be performed on log-transformed
#'   observations and predictions, else the data will be used as-is
#' @param verbose logical, if `TRUE` then info messages are printed to the console
#' @param ... additional arguments passed through to [calibrate()]
#' @aliases fit_growth,list,missing-method fit_growth,EffectScenario,ANY-method
#'   fit_growth,ScenarioSequence,ANY-method
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
setGeneric("fit_growth", function(x, data, ...) standardGeneric("fit_growth"))

#' @describeIn fit_growth Default handler
setMethod("fit_growth", c("ANY", "ANY"), function(x, data, ...) stop("Object type or argument combination not supported"))

# Wrapper for list of caliset objects
#' @noRd
setMethod("fit_growth", c("list", "missing"), function(x, data, ...) fit_growth_list(x, data=data, ...))

# Wrapper for [scenario]s
#' @noRd
setMethod("fit_growth", c("EffectScenario", "ANY"), function(x, data, ...) fit_growth_scenario(x=x, data=data, ...))

# Wrapper for [sequence]s
#' @noRd
setMethod("fit_growth", c("ScenarioSequence", "ANY"), function(x, data, ...) fit_growth_scenario(x=x, data=data, ...))

# Wrapper for single scenario + dataset
fit_growth_scenario <- function(x, data, ...) {
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
  fit_growth(x, ...)
}

# Wrapper for lists of calisets
#' @importFrom methods selectMethod
fit_growth_list <- function(x, ...) {
  # check that list only contains caliset object
  if(!all(is_caliset(x))) {
    stop("Argument 'x' must only contain caliset objects")
  }
  # then use scenario in first caliset to decide which method to call
  sc <- x[[1]]@scenario
  if(is_sequence(x[[1]]@scenario))
    sc <- sc[[1]]

  cls <- class(sc)[1]
  fun <- selectMethod("fit_growth", signature=c(x=cls, data="missing"))
  fun(x, ...)
}

#' @describeIn fit_growth Fit growth parameters of [Lemna_SETAC] scenarios
#' @include model-lemna_setac.R
setMethod("fit_growth", c("LemnaSetac", "missing"), function(x, data, par, log_scale=TRUE, verbose=FALSE, ...)
  fit_growth_lemna(x, data=data, par=par, log_scale=log_scale, verbose=verbose, ...))

#' @describeIn fit_growth Fit growth parameters of [Lemna_Schmitt] scenarios
#' @include model-lemna_schmitt.R
setMethod("fit_growth", c("LemnaSchmitt", "missing"), function(x, data, par, log_scale=TRUE, verbose=FALSE, ...)
  fit_growth_lemna_schmitt(x, data=data, par=par, log_scale=log_scale, verbose=verbose, ...))


#' @autoglobal
fit_growth_lemna <- function(x, par, data=NULL, log_scale=TRUE, verbose=FALSE,
                             maxsteps=10^6, lower=NULL, upper=NULL, ...) {
  # TODO this is not a good solution, as every model-dep function must check this.
  #      better to not pass arguments through to calibrate() in the future
  if(!is.null(lower)) {
    stop("Argument 'lower' is not allowed, use `set_bounds()` instead")
  }
  if(!is.null(upper)) {
    stop("Argument 'upper' is not allowed, use `set_bounds()` instead")
  }

  # is it a list of calisets?
  if(is.vector(x)) {
    if(!all(is_caliset(x))) {
      stop("Argument 'x' must either be a scenario or a list of calisets")
    }
  } else if(is_scenario(x) | is_sequence(x)) {
    # check if a dataset was supplied as well, then create calisets
    if(is.null(data)) {
      stop("Argument 'data' is missing")
    }
    if(is.matrix(data)) {
      data <- as.data.frame(data)
    }
    if(is.data.frame(data)) {
      data <- tox_data(data)
    }
    else if(is(data, "ToxData")) {
      # nothing to do
    } else {
      stop("Argument 'data' has unsupported type")
    }
    x <- td2cs(x, data)
  }

  # make sure that no exposure is set in any scenario/caliset
  for(i in seq_along(x)) {
    if(!is_no_exposure(x[[i]]@scenario)) {
      stop("Scenarios/observed data must not include exposure to fit growth parameters")
    }
    # check if all datasets start at time=0, which is an implicit assumption in
    # the following calculation of statistics etc
    if(min(x[[i]]@data[, 1]) != 0) {
      stop("Scenarios/datasets must start at time zero (t=0)")
    }
    # make sure that each scenario has an exposure series set
    x[[i]]@scenario <- set_noexposure(x[[i]]@scenario)
  }

  # determine parameter to fit: for lemna, use fit 'k_photo_max'
  par_nms <- "k_photo_max"

  # if no starting value set, use a default
  if(missing(par)) {
    par <- c("k_photo_max" = 0.35)
  }

  # check for invalid values
  if(any(is.na(par) | is.nan(par) | is.infinite(par))) {
    stop("Starting value of `", par_nms,"` is invalid")
  }

  # determine variable to fit on
  output_nm <- "FrondNo"

  # run fitting routine
  fit <- calibrate(x, par=par, output=output_nm, hessian=TRUE, log_scale=log_scale,
                   err_fun="sse", verbose=verbose, maxsteps=maxsteps, ...)

  # remove data points at time==0, as these are the initial conditions and not predictions
  df <- fit$data %>% dplyr::filter(time > 0)

  ## Calculate Statistics
  # Degrees of Freedom: number of data points minus number of fitted parameters, excluding t=0
  fit$dof <- nrow(df) - length(par)

  # limits of the 95% confidence interval
  # TODO likelihood profiling as alternative
  fit$ci <- ci_from_hessian(fit, dof=fit$dof, level=0.95)

  # NRMSE
  fit$nrmse <-  1 / mean(df[, 2]) * sqrt(1 / nrow(df) * sum((df[, 2] - df[, 3])^2))

  fit
}

fit_growth_lemna_schmitt <- function(x, par, ...) {
  if(missing(par)) {
    par <-  c("k_phot_max" = 0.35)
  }
  fit_growth_lemna(x=x, par=par, ...)
}


#' @describeIn fit_growth Fit growth parameters of [Magma] scenarios
#' @include model-magma.R
setMethod("fit_growth", c("Magma", "missing"), function(x, data, par, log_scale=TRUE, verbose=FALSE, ...)
  fit_growth_magma(x, data=data, par=par, log_scale=log_scale, verbose=verbose, ...))

#' @autoglobal
fit_growth_magma <- function(x, par, data=NULL, log_scale=TRUE, verbose=FALSE,
                             maxsteps=10^6, lower=NULL, upper=NULL, ...) {
  # TODO this is not a good solution, as every model-dep function must check this.
  #      better to not pass arguments through to calibrate() in the future
  if(!is.null(lower)) {
    stop("Argument 'lower' is not allowed, use `set_bounds()` instead")
  }
  if(!is.null(upper)) {
    stop("Argument 'upper' is not allowed, use `set_bounds()` instead")
  }

  # is it a list of calisets?
  if(is.vector(x)) {
    if(!all(is_caliset(x))) {
      stop("Argument 'x' must either be a scenario or a list of calisets")
    }
  } else if(is_scenario(x) | is_sequence(x)) {
    # check if a dataset was supplied as well, then create calisets
    if(is.null(data)) {
      stop("Argument 'data' is missing")
    }
    if(is.matrix(data)) {
      data <- as.data.frame(data)
    }
    if(is.data.frame(data)) {
      data <- tox_data(data)
    }
    else if(is(data, "ToxData")) {
      # nothing to do
    } else {
      stop("Argument 'data' has unsupported type")
    }
    x <- td2cs(x, data)
  }

  # make sure that no exposure is set in any scenario/caliset
  for(i in seq_along(x)) {
    if(!is_no_exposure(x[[1]]@scenario)) {
      stop("Scenarios/observed data must not include exposure to fit growth parameters")
    }
    # check if all datasets start at time=0, which is an implicit assumption in
    # the following calculation of statistics etc
    if(min(x[[1]]@data[, 1]) != 0) {
      stop("Scenarios/datasets must start at time zero (t=0)")
    }
  }

  # determine parameter to fit
  par_nms <- "mu_control"
  # TODO check if all calisets use the same growth model?
  is_log_growth <- x[[1]]@scenario@growth_model == "log"
  if(is_log_growth) {
    par_nms <- c(par_nms, "D_L")
  }

  # if no starting value set, use a default
  if(missing(par)) {
    par <- c("mu_control" = 0.47) # magic value: 0.47 is the default value proposed by model authors
    if(is_log_growth) {
      par["D_L"] <- 1 # magic value: no specific reason
    }
  }

  # check for invalid values
  if(any(is.na(par) | is.nan(par) | is.infinite(par))) {
    stop("Starting value of '", paste(par_nms, collapse=","), "' is invalid")
  }

  # determine variable to fit on
  output_nm <- "TSL"

  # run fitting routine
  fit <- calibrate(x, par=par, output=output_nm, hessian=TRUE, log_scale=log_scale,
                   err_fun="sse", verbose=verbose, maxsteps=maxsteps, ...)

  # remove data points at time==0, as these are the initial conditions and not predictions
  df <- fit$data %>% dplyr::filter(time > 0)

  ## Calculate Statistics
  # Degrees of Freedom: number of data points minus number of fitted parameters, excluding t=0
  fit$dof <- nrow(df) - length(par)

  # limits of the 95% confidence interval
  # TODO likelihood profiling as alternative
  fit$ci <- ci_from_hessian(fit, dof=fit$dof, level=0.95)

  # NRMSE
  fit$nrmse <-  1 / mean(df[, 2]) * sqrt(1 / nrow(df) * sum((df[, 2] - df[, 3])^2))

  fit
}


#' Parameter confidence intervals from fit
#'
#' Calculates parameter confidence intervals for a fit returned by [calibrate()].
#' The fit must provide a valid *Hessian* matrix.
#'
#' @param fit return value from [calibrate()]
#' @param dof integer, Degrees of Freedom, commonly the number of independent observations
#'   minus the number of fitted parameters
#' @param level numeric, desired confidence level, i.e a value between zero and one.
#'   Defaults to 95% (`0.95`).
#' @returns name list of numeric vectors of length two, the elements
#'   representing the lower and upper confidence limit, respectively.
#' @export
ci_from_hessian <- function(fit, dof, level=0.95) {
  if(missing(fit))
    stop("Argument 'fit' is missing")
  if(!inherits(fit, "cvasi_fit"))
    stop("Argument 'fit' has incorrect type, not a result from `calibrate()`")
  if(!("hessian" %in% names(fit)))
     stop("Argument 'fit' does not contain a Hessian matrix")
  if(missing(dof))
    stop("Argument 'dof' is missing")
  if(any(dof <= 0 | is.na(dof) | is.infinite(dof)))
    stop("argument 'dof' is out of range")
  if(!is.numeric(level))
    stop("Argument 'level' must be numeric")
  if(any(level <= 0 | level >= 1 | is.na(level) | is.infinite(level)))
    stop("Argument 'level' is out of range")

  par_names <- names(fit$par)
  std_err <- try(sqrt(diag(solve(fit$hessian)) * fit$value / dof), silent=TRUE)
  if(inherits(std_err, "try-error")) {
    warning("Hessian cannot be inverted: ", as.character(std_err), call.=FALSE)
    std_err <- rep(NA_real_, length(par_names))
  }
  std_err <- setNames(std_err, par_names)

  # limits of the confidence interval
  ci <- lapply(par_names, function(nm) {
    ci_lower <- (1 - level)/2
    ci_upper <- 1 - ci_lower
    unname(fit$par[[nm]] + stats::qnorm(p=c(ci_lower, ci_upper)) * std_err[[nm]])
  })
  ci <- setNames(ci, par_names)
  ci
}
