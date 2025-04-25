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
#' the remaining columns must contain the observed quantity. The dataset must
#' contain a column that which matches with the contents of parameter `output`.
#'
#' As an example, the simulation result of [Lemna_Schmitt] model contains the
#' output column *biomass* (`BM`), amongst others. To fit model parameters of said
#' *Lemna_Schmitt* scenario based on observed biomass, the observed data must
#' contain a column named `BM` which represents the observed biomass.
#' A minimal observed dataset could look like this:
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
#' * `"log_sse"`: Sum of squared errors on logarithmized data
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
#' @param x either a single [scenario] or a list of [caliset] objects to be fitted
#' @param par named numeric vector with parameters to fit and their start values
#' @param output `character`, name of a single output column of [simulate()] to
#'   optimize on
#' @param err_fun a `character` choosing one of the pre-defined error functions,
#'   or alternatively a function implementing a custom error function. Defaults
#'   to *Sum of squared errors*.
#' @param verbose `logical`, if `TRUE` then debug outputs are displayed during
#'   optimization
#' @param data `data.frame` with two or more columns with experimental data,
#' 1st column must contain time points, the following columns may values
#' which the scenario is fitted to.
#' @param by optional `character`, groups and splits the experimental data
#'   into multiple distinct trials and datasets before fitting
#' @param ode_method optional `character` to select an ODE solver for [simulate()]
#' @param endpoint *deprecated* `character`, please use `output` instead
#' @param metric_fun *deprecated*, please use `err_fun` instead
#' @param metric_total *deprecated*
#' @param as_tibble *deprecated*, result can no longer be returned as a tibble
#' @param catch_errors *deprecated*, simulation errors are always caught
#' @param ... additional parameters passed on to [stats::optim()] and [simulate()]
#'
#' @return A list of fitted parameters (as produced by [stats::optim()])
#' is returned.
#'
#' @export
#' @aliases calibrate,EffectScenario-method calibrate,CalibrationSet-method
#'    calibrate,list-method calibrate,ScenarioSequence-method
#' @examples
#' library(dplyr)
#'
#' # Get observed biomass during control experiment by Schmitt et al. (2013)
#' observed <- Schmitt2013 %>%
#'   filter(ID == "T0") %>%
#'   select(t, BM=obs)
#'
#' # Create a scenario that represents conditions during experiment
#' scenario <- metsulfuron %>%
#'   set_param(c(k_phot_fix=TRUE, k_resp=0, Emax=1)) %>%
#'   set_init(c(BM=12)) %>%
#'   set_noexposure()
#'
#' # Fit parameter 'k_phot_max' to observed biomass growth from experiment
#' calibrate(
#'   scenario,
#'   par=c(k_phot_max=1),
#'   data=observed,
#'   output="BM",
#'   method="Brent", # Brent is recommended for one-dimensional optimization
#'   lower=0,        # lower parameter boundary
#'   upper=0.5       # upper parameter boundary
#' ) -> fit
#' fit$par
#'
setGeneric("calibrate", function(x,...) standardGeneric("calibrate"), signature="x")

#' @export
#' @describeIn calibrate Fit single scenario [sequence] using a dataset
#' @include sequence.R
setMethod("calibrate", "ScenarioSequence", function(x, par, output, data, by, err_fun=c("sse", "log_sse"), verbose=TRUE, ...) {
    calibrate_scenario(x=x, par=par, data=data, output=output, by=by, err_fun=err_fun, verbose=verbose, ...)
  }
)

#' @export
#' @describeIn calibrate Fit single [scenario] using a dataset
#' @include class-EffectScenario.R
setMethod("calibrate", "EffectScenario", function(x, par, output, data, by, err_fun=c("sse", "log_sse"), verbose=TRUE, ...) {
    calibrate_scenario(x=x, par=par, data=data, output=output, by=by, err_fun=err_fun, verbose=verbose, ...)
  }
)

#' @autoglobal
calibrate_scenario <- function(x, data, endpoint=deprecated(), output, by, ...)
{
  if(lifecycle::is_present(endpoint)) { # for backwards compatibility
    lifecycle::deprecate_warn("1.1.0", "calibrate(endpoint)", "calibrate(output)")
    output <- endpoint
  }

  if(length(x) != 1) {
    stop("Argument `x` must be of length one")
  }
  if(!(is_scenario(x) | is_sequence(x))) {
    stop("Argument 'x' must be a scenario")
  }
  if(is.matrix(data)) { # coerce matrix to data.frame
    data <- as.data.frame(data)
  }
  if(!is.data.frame(data)) {
    stop("Argument 'data' must be a data.frame")
  }
  if(nrow(data) == 0) {
    stop("Argument 'data' is empty")
  }
  # make sure we have a real data.frame and not something else that behaves differently
  data <- as.data.frame(data)
  # is output column contained in data?
  if(!all(output %in% names(data))) {
    stop("Output variable is not a column in argument `data`")
  }

  # group data first?
  if(!missing(by))
  {
    if(length(by) != 1) {
      stop("Argument `by` must be of length one")
    }
    if(!is.character(by)) {
      stop("Argument `by` must be a character string")
    }
    if(!(by %in% names(data))) {
      stop("Argument `by` is not a column in argument `data`")
    }
    data <- data %>% dplyr::group_by(.data[[by]])
  }

  # create calibration sets from data.frame
  cs <- dplyr::group_map(data, .f=function(df, key) {
    # sort by time, i.e. first column
    df <- df %>%
      dplyr::select(dplyr::all_of(1), dplyr::all_of(output)) %>%
      dplyr::arrange(dplyr::pick(1), dplyr::pick(2)) %>%
      dplyr::ungroup()
    caliset(x, df)
  })

  calibrate(x=cs, output=output, ...)
}

#' @export
#' @describeIn calibrate Fit using a single [CalibrationSet]
#' @include class-CalibrationSet.R
setMethod("calibrate", "CalibrationSet", function(x, ...) calibrate_set(x=list(x), ...))

#' @export
#' @importFrom lifecycle deprecated
#' @describeIn calibrate Fit using a list of [caliset] objects
setMethod("calibrate", "list", function(x, par, output, err_fun=c("sse", "log_sse"), verbose=TRUE,
      endpoint=deprecated(), metric_fun=deprecated(), metric_total=deprecated(), as_tibble=deprecated(),
      catch_errors=deprecated(), ode_method, ...) {
    calibrate_set(x=x, par=par, output=output, err_fun=err_fun, verbose=verbose,
      endpoint=endpoint, metric_fun=metric_fun, metric_total=metric_total, as_tibble=as_tibble,
      catch_errors=catch_errors, ode_method=ode_method, ...)
  }
)

calibrate_set <- function(x, par, endpoint=deprecated(), output, metric_fun=deprecated(), metric_total=deprecated(),
                          err_fun=c("sse", "log_sse"), as_tibble=deprecated(), catch_errors=deprecated(), verbose=TRUE, data, ...) {
  if(!missing(data)) {
    stop("Argument 'data' cannot be used in combination with calisets")
  }
  if(!all(sapply(x, function(y) is(y, "CalibrationSet")))) {
    stop("Argument 'x' must only contain caliset objects")
  }

  # parameters to fit
  if(missing(par)) {
    stop("Argument `par` is missing")
  }
  if(is.list(par)) {
    par <- unlist(par)
  }
  if(!is.numeric(par) | !is.vector(par)) {
    stop("Argument `par` must be a numeric vector")
  }
  if(length(par) == 0) {
    stop("Argument `par` is empty")
  }
  nms <- names(par)
  nms <- nms[nms != ""]
  if(length(nms) < length(par)) {
    stop("All elements of argument `par` must be named")
  }

  unused <- setdiff(nms, names(get_param(x[[1]]@scenario)))
  if(length(unused) > 0) {
    stop(paste("Argument `par` contains elements which are not scenario parameters: ", paste(unused, collapse=", ")))
  }

  # output variable
  if(lifecycle::is_present(endpoint)) {
    lifecycle::deprecate_warn("1.1.0", "calibrate(endpoint)", "calibrate(output)")
    output <- endpoint
  }
  if(missing(output)) {
    stop("Argument `output` is missing")
  }
  if(length(output) != 1) {
    stop("Argument 'output' must be of length one")
  }
  if(!is.character(output)) {
    stop("Argument 'output' must be a string")
  }

  # error function
  err_desc <- "n.a."
  if(lifecycle::is_present(metric_fun)) {
    lifecycle::deprecate_warn("1.1.0", "calibrate(metric_fun)", "calibrate(err_fun)")
    err_fun <- metric_fun
  }
  if(lifecycle::is_present(metric_total)) {
    lifecycle::deprecate_stop("1.1.0", "calibrate(metric_total)", details="Has been removed without replacement. Please use `err_fun` instead.")
  }

  ## Choose an error functions
  # One of the defaults selected?
  if(is.character(err_fun)) {
    err_fun <- err_fun[[1]]
    # one of the pre-defined error functions
    if(err_fun == "sse") {
      err_fun <- sse
      err_desc <- "Sum of squared errors"
    } else if(err_fun == "log_sse") {
      err_fun <- log_sse
      err_desc <- "Sum of squared errors on log data"
    } else {
      stop("Argument `err_fun` contains the name of an unsupported error function: ", err_fun)
    }
  } else if(is.function(err_fun)) {
    err_desc <- "Custom error function"
  } else {
    stop("Argument `err_fun` must be a string or function")
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
    message("Calibration based on ", length(x), " data set", ifelse(length(x) > 1, "s", ""))
    message("Fitted parameter", ifelse(length(par_names) > 1, "s", ""), ": ", paste(par_names, collapse=","))
    message("Output variable: ", output)
    message("Error function: ", err_desc)
  }

  # check that the output variable is present in all datasets
  for(i in seq_along(x)) {
    data <- x[[i]]@data
    if(!all(output %in% names(data))) {
      stop("Output variable missing from dataset(s)")
    }
  }

  # create an environment object to enable passing data back from inside `optim()`
  env <- new.env()

  # start optimization
  stats::optim(par=par, fn=optim_set, sets=x, par_names=par_names, output=output,
               err_fun=err_fun, verbose=verbose, env=env, ...) -> fit

  # re-set names in case method 'Brent' dropped them
  names(fit$par) <- par_names
  # optimization successful?
  if(fit$convergence > 0) {
    warning(paste("  Possible convergence problem during optimization: optim gave code =",
                  fit$convergence, ifelse(is.null(fit$message), "", paste("\n  ", fit$message))))
  }

  if(verbose) {
    message("Best fit: ", paste(par_names, fit$par, sep="=", collapse=","))
  }

  # assign solver info, if it exists
  if(!is.null(env$desolve_diagn)) {
    attr(fit, "desolve_diagn") <- env$desolve_diagn
    attr(fit, "cvasi_status") <- env$cvasi_status
  }

  # return fit
  class(fit) <- c("cvasi_fit", "list")
  fit
}

# Calculate error for a single set of parameters, functions is called by
# `optim()` repeatedly
optim_set <- function(par, sets, par_names, output, err_fun, verbose=verbose,
                      ode_method, env, ...) {
  if(verbose) {
    message(paste("Testing:", paste(par_names, par, sep="=", collapse=", ")), appendLF=FALSE)
  }
  # check if parameter names got lost, happens sometimes
  if(is.null(names(par))) {
    names(par) <- par_names
  }

  obs <- c()
  pred <- c()
  wgts <- c()
  tags <- list()

  # cycle through all calibration sets, i.e. scenario and data combinations
  for(i in seq_along(sets))
  {
    # current calibration set
    set <- sets[[i]]

    # coerce fit data to data.frame to avoid issues with data.frame-like types
    data <- as.data.frame(set@data)
    # set parameters supplied by optimization routine to scenario
    scenario <- set_param(set@scenario, par)

    # run simulation
    if(missing(ode_method)) {
      out <- try(scenario %>% set_times(data[, 1]) %>% simulate(...), silent=TRUE)
    } else {
      out <- try(scenario %>% set_times(data[, 1]) %>% simulate(method=ode_method, ...), silent=TRUE)
    }

    # check if simulation result contains errors
    is_error <- FALSE
    is_issue <- FALSE
    msg <- NA
    if(is(out, "try-error")) {
      is_error <- TRUE
      msg <- out
    }
    else if(!(all(output %in% names(out)))) {
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
    else if(num_aborted(out)) {
      is_issue <- TRUE
      msg <- "simulation terminated early"
      if(is.null(env$desolve_diagn)) { # save issue info, but do not overwrite any existing info
        env$desolve_diagn <- attr(out, "desolve_diagn")
        env$cvasi_status <- attr(out, "cvasi_status")
      }
    }
    else if(num_error(out)) {
      is_issue <- TRUE
      msg <- "simulation failed"
      if(is.null(env$desolve_diagn)) { # save issue info, but do not overwrite any existing info
        env$desolve_diagn <- attr(out, "desolve_diagn")
        env$cvasi_status <- attr(out, "cvasi_status")
      }
    }

    if(is_issue | is_error)
    {
      if(verbose) {
        message(" failed")
        message("  ", msg)
      } else if(is_error) {
        warning(msg)
      }
      # penalize parameters causing issues, requires a finite value for method
      # 'L-BFGS-B' to work, therefore returning `Inf` or `NA` is not an option
      return(1e100)
    }

    obs <- c(obs, data[, output])
    pred <- c(pred, out[, output])
    # if only one weight defined, then apply the same weight to all data points
    if(length(set@weight) == 1) {
      wgts <- c(wgts, rep(set@weight, times=nrow(out)))
    } else if(length(set@weight) == nrow(out))  {
      wgts <- c(wgts, set@weight)
    } else {
      stop("Length mismatch of weights and predicted data, caliset #", i)
    }
    # tags can also be non-atomic types, so we put them in a list
    tags <- append(tags, rep(list(set@tag), times=nrow(out)))
  }

  # calculate error term/target function value on all data points at once
  err_total <- err_fun(obs, pred, wgts, tags)

  if(verbose) {
    message(paste(", Error:", err_total))
  }

  err_total
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
log_sse <- function(obs, pred, ...) {
  sse(log(obs), log(pred), ...)
}
