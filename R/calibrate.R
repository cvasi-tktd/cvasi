# TODO provide sensible default value for output by model type

#' Fit model parameters to experimental data
#'
#' The function `calibrate()` performs the calibration (fitting) of model
#' parameters to observed data. The data can originate from one or more experiments or
#' trials. Experimental conditions, such as model parameters and exposure
#' level, can differ between trials; fitting can be performed on all datasets
#' at the same time.
#'
#' Fitting of model parameters can be performed in two ways:
#' 1) A single [scenario] is fitted to a single dataset.
#'   The dataset must represent a time-series of an output variable of the
#'   model, e.g. observed biomass over time (effect data). The dataset can represent
#'   results of one or more experimental replicates under identical conditions.
#' 2) One or more datasets of observed data are fitted each to a scenario which
#'   describes the experimental conditions during observation, such as exposure
#'   level and environmental properties. Each combination of dataset and scenario
#'   is represented by a [calibration set][caliset]. During fitting,
#'   all *calibration sets* are evaluated and a total error term is calculated
#'   over all observed and predicted values.
#'
#' ### Observed data
#' Experimental, or effect, data must be supplied as a `data.frame` in long format
#' with at least two columns: the first column contains `numeric` timestamps and
#' the remaining columns must contain the observed quantity. The observed quantitiy
#' must match in unit and meaning with the model output defined by argument `output`.
#'
#' As an example, the simulation result of [Lemna_Schmitt] model contains the
#' output column *biomass* (`BM`), amongst others. To fit model parameters of said
#' *Lemna_Schmitt* scenario based on observed biomass, the observed data must
#' represent biomass as well. A minimal observed dataset could look like this:
#'
#' ```
#' observed <- data.frame(time=c(0,  7, 14, 21),
#'                        BM=c( 12, 23, 37, 56))
#' ```
#'
#' ### Error function
#' The error function quantifies the deviations between simulated and observed data.
#' The decision for an error function can have influence on the result of the
#' fitting procedure. Two error functions are pre-defined by the package and
#' can be selected by the user, but custom error functions can be used as well.
#'
#' Available pre-defined error functions:
#'
#' * `"sse"`: Sum of squared errors
#'
#' By default, the sum of squared errors is used as the error function which
#' gets minimized during fitting. A custom error function must accept four vectorized
#' arguments and return a numeric of length one, i.e. the total error value.
#' The arguments to the error function will all have the same length and are
#' defined as follows:
#'
#' * First argument: all observed data points
#' * Second argument: all simulated data points
#' * Third argument: optional weights for each data point
#' * Fourth argument: a list of optional caliset tags
#'
#' You can choose to ignore certain arguments, such as weights and tags, in your
#' custom error function. An example of a custom error function which returns
#' the sum of absolute errors looks as follow:
#' ```
#' my_absolute_error <- function(observed, predicted, weights, tags) {
#'   sum(abs(observed - predicted))
#' }
#' ```
#'
#' As tags are optional, the fourth argument may be a list containing `NULL` values.
#' The fourth argument can be used to pass additional information to the error
#' function: For example, the tag may identify the study from where the data
#' originates from and the error function could group and evaluate the data
#' accordingly.
#'
#' @name calibrate
#' @param x either a single [scenario]. [sequence], or a list of [caliset] objects
#'    to be fitted. If only a scenario or sequence is supplied, the additional argument
#'    `data` is required.
#' @param par named numeric vector with parameters to fit and their start values
#' @param output `character`, name of a single output column of [simulate()] to
#'   optimize on
#' @param err_fun a `character` choosing one of the pre-defined error functions,
#'   or alternatively a function implementing a custom error function. Defaults
#'   to *Sum of squared errors* (`"sse"`).
#' @param log_scale logical, if `TRUE` then observed and predicted values are
#'   log-transformed before the error function is evaluated. Defaults to `FALSE`.
#' @param verbose `logical`, if `TRUE` then debug outputs are displayed during
#'   optimization
#' @param data a `data.frame` or return value of [tox_data()]; the scenarios's
#'   output is fitted to the (observed) data. See [tox_data()] for valid tabular
#'   data formats.
#' @param ode_method optional `character` to select an ODE solver for [simulate()]
#' @param ... additional parameters passed on to [stats::optim()] and [simulate()]
#'
#' @returns A list of fitted parameters (as produced by [stats::optim()])
#' is returned.
#'
#' @aliases calibrate,EffectScenario-method calibrate,CalibrationSet-method
#'    calibrate,list-method calibrate,ScenarioSequence-method
#' @examples
#' # Create an artificial data set of observed frond numbers.
#' # It assumes exponential growth with an effective growth rate of 0.38
#' trial <- data.frame(time=0:14,
#'                    fronds=12 * exp(0:14 * 0.38))
#' plot(trial)
#'
#' # Create a Lemna scenario that represents unrestricted, exponential growth.
#' scenario <- Lemna_Schmitt() %>%
#'   set_param(c(k_phot_max=1, k_resp=0, EC50=1, b=1, P_up=1)) %>%
#'   set_init(c(BM=12)) %>%
#'   set_noexposure()
#'
#' # Fit scenario parameter 'k_phot_max' to observed frond numbers:
#' fit <- calibrate(
#'   scenario,
#'   par="k_phot_max",
#'   data=trial,
#'   output="BM"
#' )
#'
#' # The fitted value of 'k_phot_max' matches the effective growth rate which
#' # was used to create the artificial data set:
#' fit$par
NULL

#' @noRd
#' @export
setGeneric("calibrate", function(x, ...) standardGeneric("calibrate"), signature="x")

#' @noRd
#' @include sequence.R
setMethod("calibrate", "ScenarioSequence", function(x, ...) {
    calibrate_scenario(x=x, ...)
  }
)

#' @describeIn calibrate Fit single [scenario] to a ([tox_data]) dataset
#' @include class-EffectScenario.R
setMethod("calibrate", "EffectScenario", function(x, output, data, ...) {
    calibrate_scenario(x=x, output=output, data=data, ...)
  }
)

#' @autoglobal
calibrate_scenario <- function(x, data, endpoint=deprecated(), output, by=deprecated(), ...)
{
  if(lifecycle::is_present(endpoint)) { # for backwards compatibility
    lifecycle::deprecate_warn("1.1.0", "calibrate(endpoint)", "calibrate(output)")
    output <- endpoint
  }
  if(lifecycle::is_present(by)) {
    lifecycle::deprecate_warn("1.5.0", "calibrate(by)", details="Argument 'data' must use a position-based input format, see `?tox_data` for details")
    if(length(by) != 1)
      stop("Argument `by` must be of length one")
    if(!is.character(by))
      stop("Argument `by` must be a character string")
    if(!(by %in% names(data)))
      stop("Argument `by` is not a column in argument `data`")
    # workaround for backwards compatibility
    data <- data %>% dplyr::relocate(!!by, .after=3)
  }

  if(length(x) != 1) {
    stop("Argument 'x' must be of length one")
  }
  if(!(is_scenario(x) | is_sequence(x))) {
    stop("Argument 'x' must be a scenario")
  }
  # coerce matrix to data.frame
  if(is.matrix(data)) {
    data <- as.data.frame(data)
  }
  if(is.data.frame(data)) {
    data <- tox_data(data)
  } else if(is(data, "ToxData")) {
    # nothing to do
  } else {
    stop("Argument 'data' must be a `data.frame` or return value of `tox_data()`")
  }

  if(length(data@data) == 0) {
    stop("Argument 'data' is empty")
  }

  # convert tox_data object to list of calisets
  lst <- td2cs(x, data)
  calibrate(x=lst, output=output, ...)
}

#' @noRd
#' @include class-CalibrationSet.R
setMethod("calibrate", "CalibrationSet", function(x, ...) calibrate_list(x=list(x), ...))

# @describeIn calibrate Fit to list of [caliset]s
#' @rdname calibrate
setMethod("calibrate", "list", function(x, par, output, err_fun="sse", log_scale=FALSE, verbose=TRUE, ode_method=NULL, ...)
  calibrate_list(x=x, par=par, output=output, err_fun=err_fun, log_scale=log_scale, verbose=verbose, ode_method=ode_method, ...))

#' @importFrom lifecycle deprecated
#' @importFrom stats optim
#' @autoglobal
calibrate_list <- function(x, par, output, endpoint=deprecated(), metric_fun=deprecated(),
                          metric_total=deprecated(), err_fun="sse", log_scale=FALSE,
                          hessian=TRUE, as_tibble=deprecated(), catch_errors=deprecated(),
                          verbose=TRUE, data, ode_method=NULL, lower=NULL, upper=NULL,
                          method=NULL, ...) {
  if(!missing(data)) {
    stop("Argument 'data' cannot be used in combination with calisets")
  }
  if(!all(sapply(x, function(y) is(y, "CalibrationSet")))) {
    stop("Argument 'x' must only contain caliset objects")
  }

  ##
  ## Parameters to fit and their starting values
  ##
  if(missing(par)) {
    stop("Argument 'par' is missing")
  }
  # vector of parameter names
  if(is.character(par)) {
    par <- as.list(par)
  }
  # (a mix of) parameter names and/or name/starting value pairs
  if(is.list(par))
  {
    # if `par`contains character elements, then it's a parameter name. get
    # associated starting value from scenario's parameters
    is_nm <- sapply(par, is.character)
    if(any(is_nm)) {
      missing_nms <- unlist(par[is_nm])
      all_nms <- names(par)
      all_nms[which(is_nm)] <- missing_nms
      par_val <- x[[1]]@scenario@param[missing_nms]
      par_val <- setNames(par_val, missing_nms) # make sure every element has a name
      par[which(is_nm)] <- par_val
      par <- setNames(par, all_nms)

      # did we find all values we are looking for?
      if(any(sapply(par, is.null))) {
        stop("Could not determine starting value for parameters '", paste(missing_nms, collapse="', '"), "'")
      }
    }

    # finally, convert list to named numerical vector
    par <- unlist(par)
  }
  if(!is.numeric(par) | !is.vector(par)) {
    stop("Argument 'par' contains non-numerical elements")
  }
  if(any(is.na(par) | is.infinite(par))) {
    stop("Argument 'par' contains invalid starting values such as NA or Infinity")
  }
  if(length(par) == 0) {
    stop("Argument 'par' is empty")
  }
  nms <- names(par)
  nms <- nms[nms != ""]
  if(length(nms) < length(par)) {
    stop("All elements of argument 'par' must be named")
  }

  unused <- setdiff(nms, c(names(get_param(x[[1]]@scenario)), get_rparam(x[[1]]@scenario)))
  if(length(unused) > 0) {
    stop(paste("Argument 'par' contains elements which are not scenario parameters: ", paste(unused, collapse=", ")))
  }
  rm(nms, unused)

  ##
  ## Auto-select a sensible optimization method
  ##
  # TODO use L-BFGS-B if bounds set/available for all parameters
  if(is.null(method)) {
    if(length(par) == 1)
      method <- "Brent"
    else
      method <- "Nelder-Mead"
  }

  ##
  ## Output variable
  ##
  if(lifecycle::is_present(endpoint)) {
    lifecycle::deprecate_warn("1.1.0", "calibrate(endpoint)", "calibrate(output)")
    output <- endpoint
  }
  if(missing(output)) {
    stop("Argument 'output' is missing")
  }
  if(length(output) != 1) {
    stop("Argument 'output' must be of length one")
  }
  if(!is.character(output)) {
    stop("Argument 'output' must be a string")
  }

  ##
  ## Parameter boundaries
  ##
  # Override boundaries using arguments to function?
  if(!is.null(lower) | !is.null(upper)) {
    lifecycle::deprecate_soft("1.5.0", "cvasi::calibrate(lower)", "cvasi::set_bounds()")
    lifecycle::deprecate_soft("1.5.0", "cvasi::calibrate(upper)", "cvasi::set_bounds()")

    if(!(method %in% c("Brent", "L-BFGS-B"))) {
      stop("Selected optimization method does not support parameter bounds")
    }
    if(length(par) != length(lower) | length(par) != length(upper)) {
      stop("Length of arguments 'lower' and 'upper' must be equal to length of 'par'")
    }
    if(any(is.na(lower) | is.na(upper))) {
      stop("Arguments 'lower' and 'upper' must not contain NA")
    }
    # reformart boundaries
    new_bounds <- list()
    for(i in seq_along(par)) {
      new_bounds[[names(par)[i]]] <- c(lower[[i]], upper[[i]])
    }
    # update scenarios in calibration sets with new parameter boundaries
    for(i in seq_along(x)) {
      x[[i]]@scenario <- x[[i]]@scenario %>% set_bounds(new_bounds)
    }
    rm(i, new_bounds)
  }

  # Boundaries can only be used for optimization methods Brent and L-BFGS-B
  if(method %in% c("Brent", "L-BFGS-B")) {
    # Collect relevant boundaries from 1st caliset
    lower <- upper <- numeric(0)
    bounds <- x[[1]]@scenario@param.bounds
    for(i in seq_along(par)) {
      nm <- names(par)[i]
      # if no boundary is set, use inifity
      if(!(nm %in% names(bounds))) {
        lower[[i]] <- -Inf
        upper[[i]] <- Inf
      } else {
        lower[[i]] <- bounds[[nm]][[1]]
        upper[[i]] <- bounds[[nm]][[2]]
      }
    }

    if(any(is.infinite(lower) | is.infinite(upper))) {
      stop("Methods Brent and L-BFGS-B require all parameter boundaries to be defined.")
    }

    rm(i, nm, bounds)
  } # Else, set values to defaults
  else {
    lower <- rep(-Inf, length(par))
    upper <- rep(Inf, length(par))
  }

  ##
  ## Error function
  ##
  err_desc <- "n.a."
  if(lifecycle::is_present(metric_fun)) {
    lifecycle::deprecate_warn("1.1.0", "calibrate(metric_fun)", "calibrate(err_fun)")
    err_fun <- metric_fun
  }
  if(lifecycle::is_present(metric_total)) {
    lifecycle::deprecate_stop("1.1.0", "calibrate(metric_total)", details="Has been removed without replacement. Please use 'err_fun' instead.")
  }

  ## Choose an error functions
  # One of the defaults selected?
  if(is.character(err_fun)) {
    err_fun <- err_fun[[1]]
    # one of the pre-defined error functions
    if(err_fun == "sse") {
      err_f <- sse
      err_desc <- "Sum of squared errors"
    } else if(err_fun == "log_sse") {
      lifecycle::deprecate_warn("1.5.0", "cvasi::calibrate(err_fun='must not use `log_sse`')", with="cvasi::calibrate(log_scale)")
      err_f <- log_sse
      err_desc <- "Sum of squared errors on log data"
    } else {
      stop("Argument 'err_fun' contains the name of an unsupported error function: ", err_fun)
    }
  } else if(is.function(err_fun)) {
    err_desc <- "Custom error function"
    err_f <- err_fun
  } else {
    stop("Argument 'err_fun' must be a string or function")
  }

  # deprecated and removed parameters
  if(lifecycle::is_present(as_tibble)) {
    lifecycle::deprecate_warn("1.1.0", "cvasi::calibrate(as_tibble)", details="Results can no longer be returned as a tibble")
  }
  if(lifecycle::is_present(catch_errors)) {
    lifecycle::deprecate_warn("1.1.0", "cvasi::calibrate(catch_errors)", details="Catching errors is always enabled")
  }

  # parameter names have to be re-set for Brent method in each iteration
  par_names <- names(par)

  # some stats
  if(verbose) {
    message("Parameter fitting based on ", length(x), " data set", ifelse(length(x) > 1, "s", ""))
    message("Fitted parameter", ifelse(length(par_names) > 1, "s", ""), ": ")
    for(i in seq_along(par_names)) {
      message("  ", par_names[i], " [", lower[i], "; ", upper[i], "]")
    }
    message("Output variable: ", output)
    message("Error function: ", err_desc)
    message("Optimization method: ", method)
  }

  # make sure that the column name of observed data is identical to the
  # output variable's name in all datasets
  for(i in seq_along(x)) {
    x[[i]]@data <- setNames(x[[i]]@data, c("time", output))
  }

  # create an environment object to enable passing data back from inside `optim()`
  # elements:
  # `desolve_diagn`: only set in case of numerical errors
  # `cvasi_status` :   "   "   "   "  "   "         "
  # `tested`       : list of tested parameter sets
  # `err`          : error of best fit
  # `obs`          : observed values
  # `pred`         : predicted values of best fit
  # `times`        : time points of observed values
  env <- new.env()
  env$tested <- list()
  env$success <- logical(0)
  env$err <- NA_real_
  env$obs <- NA_real_
  env$pred <- NA_real_
  env$times <- NA_real_

  # start optimization
  fit <- optim(par=par, fn=optim_set, sets=x, par_names=par_names, output=output,
               err_fun=err_f, log_scale=log_scale, hessian=hessian, verbose=verbose, env=env,
               ode_method=ode_method, lower=lower, upper=upper, method=method, ...)

  # re-set names in case method 'Brent' dropped them
  names(fit$par) <- par_names
  # did all simulations fail?
  # TODO check reason for failures, e.g. if all due to numerics, then report this to user
  if(all(!env$success) & length(env$success) > 0) { # length check necessary to avoid spurious warnings during tests
    warning("All simulations failed. Please check arguments.", call.=FALSE)
  }
  # optimization successful?
  else if(fit$convergence > 0)
  {
    # set a fit message of well-defined return codes if nothing else is present
    # see `?optim()` for reference
    if(is.null(fit$message)) {
      if(fit$convergence == 1)
        fit$message <- "Iteration limit `maxit` reached"
      else if(fit$convergence == 10)
        fit$message <- "Nelder-Mead simplex has degenerated"
    }
    warning("  Possible convergence problem during optimization: `optim()` convergence code = ",
                  fit$convergence, ifelse(is.null(fit$message), "", paste0("\n  '", fit$message, "'")),
            call. = FALSE)
  }

  if(verbose) {
    message("Best fitting parameter", ifelse(length(par_names) > 1, "s", ""), ":")
    for(nm in par_names) {
      message("  ", nm, " = ", fit$par[[nm]])
    }
  }

  ##
  ## Enrich return value with some relevant metadata
  ##
  # assign solver info, if it exists (in case of serious errors)
  if(!is.null(env$desolve_diagn)) {
    attr(fit, "desolve_diagn") <- env$desolve_diagn
    attr(fit, "cvasi_status") <- env$cvasi_status
  }
  # evaluated data points of best-fit
  fit$data <- data.frame(time=env$times, obs=env$obs, pred=env$pred)
  # was data transformed for error calculation?
  if(log_scale) {
    fit$data <- dplyr::mutate(fit$data, log_obs=log(obs), log_pred=log(pred), residuals=log_obs - log_pred)
  } else {
    fit$data <- dplyr::mutate(fit$data, residuals=obs - pred)
  }

  # set row & col names of Hessian, if missing
  if("hessian" %in% names(fit)) {
    if(is.null(row.names(fit$hessian)))
      row.names(fit$hessian) <- par_names
    if(is.null(colnames(fit$hessian)))
      colnames(fit$hessian) <- par_names
  }

  # save arguments which were provided to calibrate()
  args <- list(
    "x" = x,               # list of calisets
    "par" = par,           # starting values
    "err_fun" = err_fun,   # error function
    "method" = method,     # optim method
    #"ode_method" = ode_method,
    "output" = output,
    "log_scale" = log_scale
  )
  attr(fit, "args") <- args

  # return fit
  class(fit) <- c("cvasi_fit", "list")
  fit
}

# Calculate error for a single set of parameters, functions is called by
# `optim()` repeatedly
optim_set <- function(par, sets, par_names, err_fun, log_scale, verbose=verbose, env, ...) {
  if(verbose) {
    message(paste("Testing:", paste(par_names, sprintf("%g", par), sep="=", collapse="; ")), appendLF=FALSE)
  }
  # check if parameter names got lost, happens sometimes
  if(is.null(names(par))) {
    names(par) <- par_names
  }
  env$tested <- append(env$tested, list(par))

  rs <- eval_cs(set_param(sets, par), verbose=FALSE, .diagnosis=TRUE, ...)

  # any errors or issues during simulations?
  any_issue <-   any(rs$is_error | rs$is_issue)
  env$success <- c(env$success, !any_issue)
  if(any_issue) {
    if(verbose)
      message("; Simulation failed")
    # save diagnosis data of first caliset that caused issues/errors
    idx_first_issue <- head(which(rs$is_error | rs$is_issue), n=1)
    env$desolve_diagn <- rs$desolve_diagn[[idx_first_issue]]
    env$cvasi_status <- rs$cvasi_status[[idx_first_issue]]

    # penalize parameters causing issues, requires a finite value for method
    # 'L-BFGS-B' to work, therefore returning `Inf` or `NA` is not an option
    return(1e100)
  }

  # calculate error term/target function value on all data points at once
  if(log_scale) {
    err_total <- err_fun(log(rs$obs), log(rs$pred), rs$wgts, rs$tags)
  } else {
    err_total <- err_fun(rs$obs, rs$pred, rs$wgts, rs$tags)
  }

  # save data in environment
  # this assumes that we are always minimizing the error, be careful
  if(is.na(env$err) | err_total < env$err) {
    env$obs <- rs$obs
    env$err <- err_total
    env$pred <- rs$pred
    env$times <- rs$times
  }

  if(verbose) {
    message("; Error: ", sprintf("%g", err_total))
  }

  err_total
}

# @param sets list of [caliset] objects
# @param output character, output column to collect as *predicted* values
# @param verbose logical, if TRUE will print messages about occurred issues etc
# @param method character, chooses the numerical integration scheme of [deSolve::ode()]
# @param ode_method character, overwrites contents of argument `method`
# @param .diagnosis logical, if TRUE then diagnostic data from [deSolve::ode()]
#   will be appended to the result
# @param .suppress logical, if TRUE then messages raised by [simulate()]
#   will be suppressed and the process will not stop in case of errors
# @return named list, has keys such as `obs`, `pred`, and `times` containing
#    *observed*, *predicted*, and output times for all calisets
eval_cs <- function(sets, output, verbose=TRUE, method=NULL, ode_method, .diagnosis=FALSE,
                    .suppress=TRUE, .ignore_method=FALSE, ...) {
  if(!is.list(sets))
    stop("Argument 'sets' must be a list")
  if(length(output) != 1)
    stop("Argument 'output' must be of length one")
  if(.ignore_method)
    method <- NULL
  if(!missing(ode_method))
    method <- ode_method

  rs <- list(
    obs=numeric(0),
    pred=numeric(0),
    times=numeric(0),
    wgts=numeric(0),
    tags=list(),
    is_error=logical(0),
    is_issue=logical(0),
    desolve_diagn=list(),
    cvasi_status=list(),
    err_msg=character(0)
  )

  # cycle through all calibration sets, i.e. scenario and data combinations
  for(i in seq_along(sets))
  {
    # current calibration set
    set <- sets[[i]]

    # coerce obs data to data.frame to avoid issues with data.frame-like types
    data <- as.data.frame(set@data)

    # run simulation
    # FIXME of sim fails e.g. due to invalid forcings format, then this error will not be apparent in the return value, error goes unnoticed?
    if(is.null(method)) {
      out <- try(set@scenario %>% set_times(data[, 1]) %>% simulate(..., .suppress=TRUE), silent=TRUE)
    } else {
      out <- try(set@scenario %>% set_times(data[, 1]) %>% simulate(method=method, .suppress=TRUE, ...), silent=TRUE)
    }

    # check if simulation result contains errors
    is_error <- FALSE
    is_issue <- FALSE
    msg <- NA_character_
    if(is(out, "try-error")) {
      is_error <- TRUE
      msg <- attr(out, "condition")$message
    }
    else if(num_aborted(out)) {
      is_issue <- TRUE
      msg <- "simulation terminated early"
    }
    else if(num_error(out)) {
      is_issue <- TRUE
      msg <- "simulation failed"
    }
    else if(!missing(output)) {
      if(!(all(output %in% names(out)))) {
        is_error <- TRUE
        msg <- "output column not in simulation results"
      }
      else if(any(is.infinite(out[, output]))) {
        is_issue <- TRUE
        msg <- "output column contains infinite values"
      }
      else if(any(is.nan(out[, output]))) {
        is_issue <- TRUE
        msg <- "output column contains NaN values"
      }
      else if(any(is.na(out[, output]))) {
        is_issue <- TRUE
        msg <- "output column contains NAs"
      }
    }

    if(is_issue | is_error)
    {
      msg2 <- paste0("Issue with caliset #", i, "/", length(sets), ": ", msg)
      if(verbose) {
        message(msg2)
      } else if(is_error & !.suppress) {
        warning(msg2, call.=FALSE)
      }
    }

    rs$obs <- c(rs$obs, data[, 2])
    # make sure predictions are present and always have the required length
    if(!is.data.frame(out)) {
      rs$pred <- c(rs$pred, rep(NA_real_, nrow(data)))
    } else if(!(output %in% names(out))) {
      rs$pred <- c(rs$pred, rep(NA_real_, nrow(data)))
    } else {
      pred <- out[, output]
      if(length(pred) != nrow(data)) {
        if(length(pred) < nrow(data))
          pred <- c(pred, rep(NA_real_, nrow(data) - length(pred)))
        else
          pred <- pred[seq(1, nrow(data))]
      }
      rs$pred <- c(rs$pred, pred)
    }
    rs$times <- c(rs$times, data[, 1])
    # if only one weight defined, then apply the same weight to all data points
    if(length(set@weight) == 1) {
      rs$wgts <- c(rs$wgts, rep(set@weight, times=nrow(data)))
    } else if(length(set@weight) == nrow(data))  {
      rs$wgts <- c(rs$wgts, set@weight)
    } else {
      stop("Length mismatch of weights and predicted data, caliset #", i)
    }
    # tags can also be non-atomic types, so we put them in a list
    rs$tags <- append(rs$tags, rep(list(set@tag), times=nrow(data)))
    rs$is_error <- c(rs$is_error, is_error)
    rs$is_issue <- c(rs$is_issue, is_issue)
    if(.diagnosis) {
      rs$desolve_diagn <- append(rs$desolve_diagn, list(attr(out, "desolve_diagn")))
      rs$cvasi_status <- append(rs$cvasi_status, list(attr(out, "cvasi_status")))
    } else {
      rs$desolve_diagn <- append(rs$desolve_diagn, NA)
      rs$cvasi_status <- append(rs$cvasi_status, NA)
    }
    rs$err_msg <- c(rs$err_msg, msg)
  }

  rs
}

# Sum of squared errors with optional weights
# @param orig observed data
# @param pred predicted data
# @param weights weighting factor for each data point
# @param tags optional study tag, e.g. to identify where data originates from
sse <- function(obs, pred, weights=1, tags=NULL) {
  if(length(obs) != length(pred))
    stop("Length mismatch of observed and predicted data.")
  if(length(weights) != 1 & length(weights) != length(obs))
    stop("Length mismatch of weights and observed data.")
  sum((obs - pred)^2 * weights)
}

# Sum of squared errors on logarithmized data
# @param orig observed data
# @param pred predicted data
# @param ... additional parameters passed through to [sse()]
# Only kept for backwards compatibility
log_sse <- function(obs, pred, ...) {
  sse(log(obs), log(pred), ...)
}
