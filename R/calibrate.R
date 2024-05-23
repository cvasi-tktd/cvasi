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
#'   is represented by a [calibration set][CalibrationSet]. During fitting,
#'   all *calibration sets* are evaluated and a total error term is calculated by
#'   summing the error of each *calibration set*.
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
#' By default, the total sum of squared errors is used as the target
#' function which is minimized during fitting. A custom error function can be
#' supplied by the user: The function must accept two numeric vectorized
#' arguments and return a numeric of length one, i.e. the error value.
#'
#' Example of a custom error function which returns the sum of absolute errors:
#' ```
#' my_absolute_error <- function(observed, simulated) {
#'   sum(abs(observed - simulated))
#' }
#' ```
#'
#' When using *calibration sets*, the error term is calculated for each *calibration
#' set* individually, the weighting factor is applied to the error of each set,
#' and then all error terms are summed up.
#'
#' @param x either a single [scenario] or a list of [CalibrationSet] objects to be fitted
#' @param par named numeric vector with parameters to fit and their start values
#' @param endpoint *deprecated* `character`, please use `output` instead
#' @param output `character`, name of a single output column of [simulate()] to
#'   optimize on
#' @param metric_fun *deprecated*, please use `err_fun` instead
#' @param err_fun  vectorized error function to calculate an error term that is
#'   minimized during optimization, must accept exactly two vectorized numeric
#'   arguments, defaults to sum of squared errors
#' @param as_tibble *deprecated*, result can no longer be returned as a tibble
#' @param catch_errors *deprecated*, simulation errors are always caught
#' @param verbose `logical`, if `TRUE` then debug outputs are displayed during
#'   optimization
#' @param data `data.frame` with two or more columns with experimental data,
#' 1st column must contain time points, the following columns may values
#' which the scenario is fitted to.
#' @param by optional `character`, groups and splits the experimental data
#'   into multiple distinct trials and datasets before fitting
#' @param metric_total *deprecated*
#' @param ... additional parameters passed on to [stats::optim()] and [simulate()]
#'
#' @return A list of fitted parameters (as produced by [stats::optim()])
#' is returned.
#'
#' @export
#' @aliases calibrate,EffectScenario-method calibrate,CalibrationSet-method
#'    calibrate,list-method
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
#' @describeIn calibrate Fit single scenario using a dataset
#' @importFrom lifecycle deprecated
#' @include class-EffectScenario.R
setMethod("calibrate", "EffectScenario",
  function(x,
           par,
           data,
           endpoint=deprecated(),
           output,
           by,
           metric_fun=deprecated(),
           err_fun,
           as_tibble=deprecated(),
           catch_errors=deprecated(),
           verbose=FALSE,
           ...) {
    calibrate_scenario(x=x,
                       par=par,
                       data=data,
                       endpoint=endpoint,
                       output=output,
                       by=by,
                       metric_fun=metric_fun,
                       err_fun=err_fun,
                       as_tibble=as_tibble,
                       catch_errors=catch_errors,
                       verbose=verbose,
                       ...)
  }
)

calibrate_scenario <- function(x, data, endpoint, output, by, ...)
{
  if(lifecycle::is_present(endpoint)) { # for backwards compatability
    output <- endpoint
  }

  if(!is_scenario(x)) {
    stop("parameter 'x' must be a scenario")
  }
  if(is.matrix(data)) { # coerce matrix to data.frame
    data <- as.data.frame(data)
  }
  if(!is.data.frame(data)) {
    stop("parameter 'data' must be a data.frame")
  }
  if(nrow(data) == 0) {
    stop("parameter 'data' is empty")
  }
  # make sure we have data.frame and not something else that behaves differently
  data <- tibble::as_tibble(data)
  # is output column contained in data?
  if(!all(output %in% names(data))) {
    stop("Output variable missing from dataset")
  }

  # group data first?
  if(!missing(by)) {
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

  calibrate(x=cs, endpoint=endpoint, output=output, ...)
}

#' @export
#' @describeIn calibrate Fit using a [CalibrationSet]
#' @include class-CalibrationSet.R
setMethod("calibrate", "CalibrationSet",
          function(x, par, output, err_fun, verbose=FALSE, ...) {
            calibrate_set(x=list(x), par=par, output=output, err_fun=err_fun,
                          verbose=verbose, ...)
          }
)

#' @export
#' @describeIn calibrate Fit using a list of [CalibrationSet] objects
setMethod("calibrate", "list",
          function(x,
                   par,
                   endpoint=deprecated(),
                   output,
                   metric_fun=deprecated(),
                   metric_total=deprecated(),
                   err_fun,
                   as_tibble=deprecated(),
                   catch_errors=deprecated(),
                   verbose=FALSE,
                   ...)
            calibrate_set(x=x,
                          par=par,
                          endpoint=endpoint,
                          output=output,
                          metric_fun=metric_fun,
                          metric_total=metric_total,
                          err_fun=err_fun,
                          as_tibble=as_tibble,
                          catch_errors=catch_errors,
                          verbose=verbose,
                          ...)
)

calibrate_set <- function(x, par, endpoint, output, metric_fun, metric_total,
                          err_fun, as_tibble, catch_errors, verbose, data, ...) {
  if(!missing(data)) {
    stop("parameter 'data' cannot be used in combination with calibration sets")
  }
  if(!all(sapply(x, function(y) is(y, "CalibrationSet")))) {
    stop("parameter 'x' must only contain calibration set objects")
  }

  # parameters to fit
  if(!is.numeric(par) | !is.vector(par)) {
    stop("parameter 'par' must be a numeric vector")
  }
  if(length(names(par)) < length(par)) {
    stop("all elements in 'par' must be named")
  }
  unused <- setdiff(names(par), x[[1]]@scenario@param.req)
  if(length(unused) > 0) {
    stop(paste("invalid parameters, can not fit: ", paste(unused, sep=",")))
  }

  # output variable
  if(lifecycle::is_present(endpoint)) {
    lifecycle::deprecate_warn("1.1.0", "calibrate(endpoint)", "calibrate(output)")
    output <- endpoint
  }
  if(length(output) > 1) {
    stop("parameter 'output' must be of length one")
  }
  if(!is.character(output)) {
    stop("parameter 'output' must be a string")
  }

  # error function
  err_desc <- "Custom"
  if(lifecycle::is_present(metric_fun)) {
    lifecycle::deprecate_warn("1.1.0", "calibrate(metric_fun)", "calibrate(err_fun)")
    err_fun <- metric_fun
  }
  if(lifecycle::is_present(metric_total)) {
    lifecycle::deprecate_warn("1.1.0", "calibrate(metric_total)")
  }
  if(missing(err_fun)) {
    err_fun <- sse
    err_desc <- "Sum of squared errors"
  } else if(!is.function(err_fun)) {
    stop("parameter 'err_fun' must be a function")
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

  # check that output variable is present in all datasets
  for(i in seq_along(x)) {
    data <- x[[i]]@data
    if(!all(output %in% names(data))) {
      stop("Output variable missing from dataset(s)")
    }
  }

  # start optimization
  stats::optim(par=par,
               fn=optim_set,
               sets=x,
               par_names=par_names,
               output=output,
               err_fun=err_fun,
               verbose=verbose,
               ...) -> fit

  # re-set names in case method 'Brent' dropped them
  names(fit$par) <- par_names
  # optimization successful?
  if(fit$convergence > 0) {
    warning(paste("  possible convergence problem during optimization: optim gave code =",
                  fit$convergence, ifelse(is.null(fit$message), "", paste("\n  ", fit$message))))
  }

  if(verbose) {
    message("Best fit: ", paste(par_names, fit$par, sep="=", collapse=","))
  }

  # return fit
  fit
}

# Calculate error for a single set of parameters, functions is called by
# `optim()` repeatedly
optim_set <- function(par, sets, par_names, output, err_fun, verbose=verbose,
                      ode_method, ...) {
  if(verbose) {
    message(paste("Testing:", paste(par_names, par, sep="=", collapse=",")), appendLF=FALSE)
  }
  # check if parameter names got lost, happens sometimes
  if(is.null(names(par))) {
    names(par) <- par_names
  }
  # total error
  err_total <- 0

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
      out <- try(simulate(scenario, times=data[, 1], ...), silent=TRUE)
    } else {
      out <- try(simulate(scenario, times=data[, 1], method=ode_method, ...), silent=TRUE)
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

    if(is_issue | is_error) {
      if(verbose) {
        message(" failed, Error: 1e15")
        message("  ", msg)
      } else if(is_error) {
        warning(msg)
      }
      return(1e15) # penalize parameters causing issues, requires a finite value for 'L-BFGS-B' to work
    }

    # calculate and sum up error term values
    err_total <- err_total + err_fun(data[, output], out[, output]) * set@weight
  }

  if(verbose) {
    message(paste(", Error:", err_total))
  }

  err_total
}
