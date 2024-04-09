#' Fit model parameters to experimental data
#'
#' @description The function `calibrate()`performs the calibration (fitting) of model parameters to observed data.
#' Two options are available: One can either calibrate a single \linkS4class{EffectScenario} (with corresponding observations on effect data),
#' or one can fit to a list of one or more \linkS4class{CalibrationSet} objects (i.e., a combiation of `EffectScenario`
#' and effect data joined together into a `CalibrationSet` object). The latter allows one to combine the results from several experiments
#' within one fitting process.
#'
#' Effect data should be supplied in long format with 2 columns: the timepoints and the observations. Effect data can contain replicates as extra rows.
#'
#' @param x either a single `EffectScenario` or a list of `CalibrationSet` objects to be fitted
#' @param par named numeric vector with parameters to fit and their start values
#' @param endpoint `character`, name of model output column to optimize on
#' @param hessian `logical`, if `TRUE` the Hessian is returned by the optimization routine
#' @param metric.fun vectorized function to calculate an error term that is
#' minimized during optimization, defaults to sum of squared errors
#' @param as.tibble `logical`, if `TRUE` a tibble with scenario and optimization
#'  result is returned, else only the optimization result is returned
#' @param catch.errors `logical`, if `TRUE` simulation errors due to e.g. out of
#' bounds parameters are caught and optimization continues, else optimization fails
#' @param verbose `logical`, if `TRUE` then debug outputs are displayed during
#' optimization
#' @param ... additional parameters passed on to `stats::optim()` and `simulate()`
#'
#' @examples
#' # Example with Lemna ----------------------------------------------
#' # 1st option: with an EffectScenario ----------------------------
#' # EffectScenario
#' is(metsulfuron)
#' # Effect data
#' obs32 <- Schmitt2013 %>%
#'   dplyr::filter(ID == "T0.32") %>%
#'   dplyr::select(t, obs) # can only contain t and conc
#' # calibrate with an ExposureScenario and corresponding effect data
#' calib_scen <- calibrate(
#'   x = metsulfuron,
#'   par = c(k_phot_max = 1, k_resp = 0.1 ),
#'   data = obs32,
#'   endpoint = "BM",
#'  )
#' # check return
#' is(calib_scen)
#' calib_scen
#' # plot
#' obs32$trial <- "trail1"
#' colnames(obs32) <- c("time", "obs", "trial")
#' plot_sd(model_base = metsulfuron,
#'   treatments = metsulfuron@exposure@series,
#'   rs_mean = obs32)
#'
#' # 2nd option: with list of CalibrationSets ----------------------
#' # step 1. list of EffectScenarios
#' list_of_eff_scen <- Schmitt2013 %>%
#'   dplyr::group_by(ID) %>%
#'   dplyr::group_map(~ metsulfuron %>% set_exposure(data.frame(.x$t, .x$conc)))
#' # step 2. list of effect data
#' list_of_eff_data <- Schmitt2013 %>%
#'   dplyr::group_by(ID) %>%
#'   dplyr::group_map(~data.frame(.x$t, .x$obs))
#' # step 3. combined into list of CalibrationSets
#' list_of_calib_sets <- list()
#' for(i in 1:length(list_of_eff_scen)){
#'   list_of_calib_sets[[i]] <- CalibrationSet(list_of_eff_scen[[i]], list_of_eff_data[[i]])
#' }
#' # 4. calibrate
#' calib_scen_list <- calibrate(
#'  x = list_of_calib_sets, # list of calibration sets
#'  par = c(k_phot_max = 1),
#'  endpoint = "BM",
#' )
#'  # check return
#' is(calib_scen_list)
#' calib_scen_list$scenario[[1]]
#'
#'
#' @return By default, a list of fitted parameters (as produced by [stats::optim()])
#' is returned. Alternatively, when setting the argument as.tibble = TRUE,
#' a `tibble` is returned with 2 columns, the first column lists the
#' scenario and the 2nd column the list of fitted parameters.
#'
#' @export
#' @include class-EffectScenario.R
setGeneric("calibrate", function(x,...) standardGeneric("calibrate"), signature="x")

#' @param data `data.frame` with two or more columns with experimental data,
#' 1st column contains time points, 2nd and following columns contain values
#' which the scenario is fitted to.
#' @rdname calibrate
#' @export
setMethod("calibrate", "EffectScenario",
  function(x, par, data, endpoint, hessian=TRUE, metric.fun=sse,
           as.tibble=FALSE, catch.errors=TRUE, verbose=FALSE, ...) {
    calibrate_scenario(x=x, par=par, data=data, endpoint=endpoint, hessian=hessian,
                       metric.fun=metric.fun, as.tibble=as.tibble, catch.errors=catch.errors,
                       verbose=verbose, ...)
  }
)

#' @param metric.total vectorized function to aggregate multiple error terms to a
#' single value, defaults to `sum()`
#' @rdname calibrate
#' @export
setMethod("calibrate", "list",
  function(x, par, endpoint, hessian=TRUE, metric.fun=sse, metric.total=sum,
           as.tibble=FALSE, catch.errors=TRUE, verbose=FALSE, ...)
    calibrate_set(x=x, par=par, endpoint=endpoint, hessian=hessian,
                  metric.fun=metric.fun, metric.total=metric.total,
                  as.tibble=as.tibble, catch.errors=catch.errors,
                  verbose=verbose, ...)
)

calibrate_scenario <- function(x, par, data, endpoint, hessian, metric.fun=sse,
                     as.tibble, catch.errors, verbose, ...) {
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
  par.names <- names(par)

  stats::optim(par=par,
               fn=optim_scenario,
               hessian=hessian,
               scenario=x,
               par.names=par.names,
               data=data,
               endpoint=endpoint,
               metric.fun=metric.fun,
               catch.errors=catch.errors,
               verbose=verbose,
               ...) -> fit

  names(fit$par) <- par.names # re-set names in case method 'Brent' dropped them
  if(fit$convergence>0)
    warning(paste("possible convergence problem: optim gave code =",fit$convergence,
                  ifelse(is.null(fit$message),"",paste("\n",fit$message))))

  # return updated scenario + fit data as tibble
  x <- set_param(x, fit$par)
  if(as.tibble) {
    tibble::tibble(scenario=c(x),fit=list(fit))
  } else {
    fit
  }
}

calibrate_set <- function(x, par, endpoint, hessian, metric.fun, metric.total,
                          as.tibble, catch.errors, verbose, data, ...) {
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
  par.names <- names(par)

  stats::optim(par=par,
               fn=optim_set,
               hessian=hessian,
               sets=x,
               par.names=par.names,
               endpoint=endpoint,
               metric.fun=metric.fun,
               metric.total=metric.total,
               catch.errors=catch.errors,
               verbose=verbose,
               ...) -> fit

  names(fit$par) <- par.names # re-set names in case method 'Brent' dropped them
  if(fit$convergence>0)
    warning(paste("possible convergence problem: optim gave code =",
                  fit$convergence,ifelse(is.null(fit$message),"",paste("\n",fit$message))))

  # return updated scenario + fit data as tibble
  if(as.tibble) {
    x <- purrr::map(x, ~.x@scenario %>% set_param(fit$par))
    tibble::tibble(scenario=c(x),fit=list(fit))
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
# @param metric.fun `function` to calculate error metrics
# @param ... additional parameters passed on to `simulate()`
#
# @return `numeric` value, error metric
optim_scenario <- function(par, scenario, par.names, data, endpoint, metric.fun,
                          catch.errors=catch.errors, verbose=verbose, ode.method, ...) {
  # set parameters supplied by optimization routine
  if(is.null(names(par))) names(par) <- par.names # req for Brent method
  scenario <- set_param(scenario, par)
  # comparison table, measured vs. simulated
  df.cmp <- data.frame(m=numeric(),s=numeric())

  # cycle through all data sets/columns
  for(i in seq(length(data)-1)) {
    # run simulation
    if(missing(ode.method))
      out <- try(simulate(scenario,times=data[,1],...), silent=TRUE)
    else
      out <- try(simulate(scenario,times=data[,1],method=ode.method,...), silent=TRUE)

    if(catch.errors & is(out,"try-error")) {
      if(verbose)
        message(paste(" -- Invalid param:",paste(par.names,par,sep="=",collapse=",")," Err: 1e15"))
      return(1e15) # penalize invalid parameters, requires a finite value for 'L-BFGS-B' to work
    }
    df.cmp <- rbind(df.cmp, list(ref=data[,i+1],fit=out[,endpoint]))
  }

  if(verbose)
    message(paste("Param:",paste(par.names,par,sep="=",collapse=",")," Err:",metric.fun(df.cmp[,1],df.cmp[,2])))

  # error term
  metric.fun(df.cmp[,1],df.cmp[,2])
}


optim_set <- function(par, sets, par.names, endpoint, metric.fun, metric.total,
                      catch.errors=catch.errors, verbose=verbose, ode.method, ...) {
  # error term
  err <- c()
  # cycle through all calibration sets, i.e. scenario/data combination
  for(i in seq(length(sets))) {
    # current calibration set and scenario object
    set <- sets[[i]]
    # set parameters supplied by optimization routine
    if(is.null(names(par))) names(par) <- par.names # req for Brent method
    scenario <- set_param(set@scenario, par)

    # run simulation
    if(missing(ode.method))
      out <- try(simulate(scenario,times=set@data[,1],...), silent=TRUE)
    else
      out <- try(simulate(scenario,times=set@data[,1],method=ode.method,...), silent=TRUE)
    if(catch.errors & is(out,"try-error")) {
      if(verbose)
        message(paste(" -- Invalid param:",paste(par.names,par,sep="=",collapse=",")," Err: 1e15"))
        message(out)
      return(1e15) # penalize invalid parameters, requires a finite value for 'L-BFGS-B' to work
    }
    if(!(endpoint %in% names(out)))
      stop("endpoint not present in simulation results")
    err <- c(err, metric.fun(set@data[,2],out[,endpoint]) * set@weight)
  }

  err.total <- metric.total(err)
  if(verbose)
    message(paste("Param:",paste(par.names,par,sep="=",collapse=",")," Err:",err.total))

  err.total
}
