#' Fit model parameters to experimental data
#'
#' The function `calibrate()`performs the calibration (fitting) of model
#' parameters to observed data. Two options are available:
#' 1) Either a single [scenario] is fitted to a single dataset.
#'   The dataset must represent a time-series of an output variable of the
#'   model, e.g. observed biomass over time (effect data).
#' 2) Or a scenario is fitted to one or multiple datasets at once. Each
#'   dataset is represented by a \linkS4class{CalibrationSet} which also
#'   contains a scenario which describes the conditions during observation,
#'   such as exposure and environmental properties. Each `CalibrationSet`
#'   can also be given a weight to be considered in the fitting. By default,
#'   the total sum of squared errors is used as the target function which is
#'   minimized.
#'
#' Effect data must be supplied as a `data.frame` in long format with two
#' columns: the first column contains timestamps and the second column
#' the observed property. The effect data may contain replicates as extra rows.
#'
#' @param x either a single `EffectScenario` or a list of `CalibrationSet` objects to be fitted
#' @param par named numeric vector with parameters to fit and their start values
#' @param endpoint `character`, name of model output column to optimize on
#' @param hessian `logical`, if `TRUE` the Hessian is returned by the optimization routine
#' @param metric_fun vectorized function to calculate an error term that is
#' minimized during optimization, defaults to sum of squared errors
#' @param as_tibble `logical`, if `TRUE` a tibble with scenario and optimization
#'  result is returned, else only the optimization result is returned
#' @param catch_errors `logical`, if `TRUE` simulation errors due to e.g. out of
#' bounds parameters are caught and optimization continues, else optimization fails
#' @param verbose `logical`, if `TRUE` then debug outputs are displayed during
#' optimization
#' @param ... additional parameters passed on to `stats::optim()` and `simulate()`
#'
#' @examples
#' ## 1st option: Fit scenario parameters using a single dataset
#' metsulfuron
#'
#' # Observed biomass from Schmitt et al. 2013 experiment
#' obs32 <- data.frame(
#'   time=c(0, 3, 5, 7, 7.01, 10, 12, 14),
#'   obs=c(12, 25, 30, 33, 33, 64, 127, 300)
#' )
#'
#' # calibrate with an ExposureScenario and corresponding effect data
#' metsulfuron %>%
#'   calibrate(
#'     par=c(k_phot_max=1, k_resp=0.1 ),
#'     data=obs32,
#'     endpoint="BM",
#'   )
#'
#' # plot trial data
#' obs32$trial <- "conc=1.0"
#' plot_sd(treatments=metsulfuron@exposure@series, rs_mean=obs32)
#'
#' ## 2nd option: Fit scenario parameters using CalibrationSets
#'
#' # Create one CalibrationSet for each observed exposure level and time-series
#' # from Schmitt et al. (2013) dataset
#' library(dplyr)
#' Schmitt2013 %>%
#'   group_by(ID) %>%
#'   group_map(function(data, key) {
#'     exp <- data %>% select(t, conc)
#'     obs <- data %>% select(t, obs)
#'     sc <- metsulfuron %>% set_exposure(exp)
#'     CalibrationSet(sc, obs)
#'   }) -> cs
#' # Fit parameters
#' calibrate(cs, par=c(k_phot_max=1), endpoint="BM")
#'
#' @param data `data.frame` with two or more columns with experimental data,
#' 1st column contains time points, 2nd and following columns contain values
#' which the scenario is fitted to.
#' @param metric_total vectorized function to aggregate multiple error terms to a
#' single value, defaults to `sum()`
#'
#' @return By default, a list of fitted parameters (as produced by [stats::optim()])
#' is returned. Alternatively, when setting the argument as_tibble = TRUE,
#' a `tibble` is returned with 2 columns, the first column lists the
#' scenario and the 2nd column the list of fitted parameters.
#'
#' @export
#' @aliases calibrate,EffectScenario-method calibrate,list-method
#' @include class-EffectScenario.R
setGeneric("calibrate", function(x,...) standardGeneric("calibrate"), signature="x")

#' @export
#' @describeIn calibrate Fit a single scenario
setMethod("calibrate", "EffectScenario",
  function(x, par, data, endpoint, hessian=TRUE, metric_fun=sse,
           as_tibble=FALSE, catch_errors=TRUE, verbose=FALSE, ...) {
    calibrate_scenario(x=x, par=par, data=data, endpoint=endpoint, hessian=hessian,
                       metric_fun=metric_fun, as_tibble=as_tibble, catch_errors=catch_errors,
                       verbose=verbose, ...)
  }
)

#' @export
#' @describeIn calibrate Fit using a [CalibrationSet]
setMethod("calibrate", "list",
  function(x, par, endpoint, hessian=TRUE, metric_fun=sse, metric_total=sum,
           as_tibble=FALSE, catch_errors=TRUE, verbose=FALSE, ...)
    calibrate_set(x=x, par=par, endpoint=endpoint, hessian=hessian,
                  metric_fun=metric_fun, metric_total=metric_total,
                  as_tibble=as_tibble, catch_errors=catch_errors,
                  verbose=verbose, ...)
)

calibrate_scenario <- function(x, par, data, endpoint, hessian, metric_fun=sse,
                               as_tibble, catch_errors, verbose, ...) {
  if(!is.numeric(par) | !is.vector(par))
    stop("par is not numeric vector")
  if(length(names(par)) < length(par))
    stop("all parameters to fit have to be named")
  unused <- setdiff(names(par),x@param.req)
  if(length(unused)>0)
    stop(paste("parameters can not be fit:",paste(unused,sep=",")))
  if(nrow(data)==0)
    stop("no data for calibration")
  #if(data[1,1]!=0)
  #  stop("calibration data needs to start at zero")
  if(!is.character(endpoint) | length(endpoint)>1)
    stop("only one endpoint supported")

  # make sure we have data.frame and not a matrix
  data <- as.data.frame(data)
  # some stats
  if(verbose) {
    message(paste("fitted scenario:",is(x)[[1]]))
    message(paste("  data contains",length(data)-1,"replicates"))
    message(paste("  time points:",paste(data[,1],collapse=",")))
  }
  # parameter names have to be re-set for Brent method in each iteration
  par_names <- names(par)

  stats::optim(par=par,
               fn=optim_scenario,
               hessian=hessian,
               scenario=x,
               par_names=par_names,
               data=data,
               endpoint=endpoint,
               metric_fun=metric_fun,
               catch_errors=catch_errors,
               verbose=verbose,
               ...) -> fit

  names(fit$par) <- par_names # re-set names in case method 'Brent' dropped them
  if(fit$convergence>0)
    warning(paste("possible convergence problem: optim gave code =",fit$convergence,
                  ifelse(is.null(fit$message),"",paste("\n",fit$message))))

  # return updated scenario + fit data as tibble
  x <- set_param(x, fit$par)
  if(as_tibble) {
    tibble::tibble(scenario=c(x),fit=list(fit))
  } else {
    fit
  }
}

calibrate_set <- function(x, par, endpoint, hessian, metric_fun, metric_total,
                          as_tibble, catch_errors, verbose, data, ...) {
  if(!missing(data))
    stop("parameter 'data' must not be set when using CalibrationSets")
  if(!all(sapply(x,function(y)is(y,"CalibrationSet"))))
    stop("list must contain CalibrationSet objects")
  if(!is.numeric(par) | !is.vector(par))
    stop("par is not numeric vector")
  if(length(names(par)) < length(par))
    stop("all parameters to fit have to be named")
  #unused <- setdiff(names(par),x[[1]]@param.req)
  #if(length(unused)>0)
  #  stop(paste("parameters can not be fit:",paste(unused,sep=",")))
  if(!is.character(endpoint) | length(endpoint)>1)
    stop("only one endpoint supported")

  # some stats
  if(verbose) {
    message(paste("fitted scenario:",is(x[[1]]@scenario)[[1]]))
    message(paste("  set contains",length(x), "trials"))
  }
  # parameter names have to be re-set for Brent method in each iteration
  par_names <- names(par)

  stats::optim(par=par,
               fn=optim_set,
               hessian=hessian,
               sets=x,
               par_names=par_names,
               endpoint=endpoint,
               metric_fun=metric_fun,
               metric_total=metric_total,
               catch_errors=catch_errors,
               verbose=verbose,
               ...) -> fit

  names(fit$par) <- par_names # re-set names in case method 'Brent' dropped them
  if(fit$convergence>0)
    warning(paste("possible convergence problem: optim gave code =",
                  fit$convergence,ifelse(is.null(fit$message),"",paste("\n",fit$message))))

  # return updated scenario + fit data as tibble
  if(as_tibble) {
    x <- purrr::map(x, ~.x@scenario %>% set_param(fit$par))
    tibble::tibble(scenario=c(x), fit=list(fit))
  } else {
    fit
  }
}

# Default wrapper function that is called by `stats::optim()`
#
# Can optimize on several data sets at once using different model setups.
# @param x Current parameter values set by `stats::optim()`
# @param scenario `EffectScenario` object
# @param data `data.frame` with (measured) calibration data
# @param endpoint `character` or `numeric` value identifying the relevant model output
# @param metric_fun `function` to calculate error metrics
# @param ... additional parameters passed on to `simulate()`
#
# @return `numeric` value, error metric
optim_scenario <- function(par, scenario, par_names, data, endpoint, metric_fun,
                           catch_errors=catch_errors, verbose=verbose, ode_method, ...) {
  # set parameters supplied by optimization routine
  if(is.null(names(par))) names(par) <- par_names # req for Brent method
  scenario <- set_param(scenario, par)
  # comparison table, measured vs. simulated
  df.cmp <- data.frame(m=numeric(),s=numeric())

  # cycle through all data sets/columns
  for(i in seq(length(data)-1)) {
    # run simulation
    if(missing(ode_method))
      out <- try(simulate(scenario,times=data[,1],...), silent=TRUE)
    else
      out <- try(simulate(scenario,times=data[,1],method=ode_method,...), silent=TRUE)

    if(catch_errors & is(out,"try-error")) {
      if(verbose)
        message(paste(" -- Invalid param:",paste(par_names,par,sep="=",collapse=",")," Err: 1e15"))
      return(1e15) # penalize invalid parameters, requires a finite value for 'L-BFGS-B' to work
    }
    df.cmp <- rbind(df.cmp, list(ref=data[,i+1],fit=out[,endpoint]))
  }

  if(verbose)
    message(paste("Param:",paste(par_names,par,sep="=",collapse=",")," Err:",metric_fun(df.cmp[,1], df.cmp[,2])))

  # error term
  metric_fun(df.cmp[,1],df.cmp[,2])
}


optim_set <- function(par, sets, par_names, endpoint, metric_fun, metric_total,
                      catch_errors=catch_errors, verbose=verbose, ode_method, ...) {
  if(verbose)
    message(paste("Param:", paste(par_names, par, sep="=", collapse=",")), appendLF=FALSE)
  # error term
  err <- c()
  # cycle through all calibration sets, i.e. scenario/data combination
  for(i in seq(length(sets))) {
    # current calibration set and scenario object
    set <- sets[[i]]
    # coerce fit data to data.frame to avoid issues with data.frame-like types
    data <- as.data.frame(set@data)
    # set parameters supplied by optimization routine
    if(is.null(names(par))) names(par) <- par_names # req for Brent method
    scenario <- set_param(set@scenario, par)

    # run simulation
    if(missing(ode_method))
      out <- try(simulate(scenario,times=data[,1],...), silent=TRUE)
    else
      out <- try(simulate(scenario,times=data[,1],method=ode_method,...), silent=TRUE)
    if(catch_errors & is(out,"try-error")) {
      if(verbose)
        message(paste(" -- Invalid param:",paste(par_names,par,sep="=",collapse=",")," Err: 1e15"))
        message(out)
      return(1e15) # penalize invalid parameters, requires a finite value for 'L-BFGS-B' to work
    }
    if(!(endpoint %in% names(out)))
      stop("endpoint not present in simulation results")
    # todo handle missing/invalid values more gracefully
    if(any(is.infinite(out[,endpoint])))
       stop("endpoints contain infinite values")
    if(any(is.nan(out[,endpoint])))
      stop("endpoints contain NaN values")
    if(any(is.na(out[,endpoint])))
      stop("endpoints contain NA")
    err <- c(err, metric_fun(data[,2],out[,endpoint]) * set@weight)
  }

  err.total <- metric_total(err)
  if(verbose)
    message(paste(" Err:",err.total))

  err.total
}
