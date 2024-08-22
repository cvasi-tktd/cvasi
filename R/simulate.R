#' Simulate an effect scenario
#'
#' The supplied `EffectScenario` is passed on to the ODE solver for numerical
#' integration. Internally, `simulate()` is split up into several functions
#' dedicated to particular models, e.g. one for GUTS and one for Lemna type
#' models. The package will take care of using the correct function
#' for each model when `simulate()` is called.
#'
#' Simulation results are returned as a time-series for each state
#' variable. Some models provide additional properties describing the model state,
#' e.g. the internal concentration of a toxicant within the organism. Refer
#' to the respective [scenario] for more information.
#'
#' Additional arguments to `simulate()` will be passed on to [deSolve::ode()]
#' which enables control of the numerical integration parameters.
#'
#' ### Output times and windows
#' The minimum and maximum of given time points generally define the simulated
#' period. Function argument `times` overrides settings of the scenario, i.e.
#' time points set in `scenario@times`. However, the simulation can be limited to
#' a subset of time points by enabling a moving exposure window, see [set_window()].
#'
#' Results will be returned for each time point. Precision of the numeric solver
#' may be affected by chosen output times in certain cases. Hence, small deviations
#' in results should be expected if different output times are set. This effect
#' can be mitigated by either defining are sufficiently small time step for the solver
#' using argument `hmax` or by decreasing the error tolerances `atol` and `rtol`.
#' These arguments are passed to the solver, see e.g. [deSolve::lsoda()] for details.
#'
#' ### Numerical precision and stability
#' Each model was assigned a default ODE solver which handles most of the
#' occurring inputs well. In most cases, this will be an explicit numerical
#' scheme of the Runge-Kutta family with variable step width. For certain extreme
#' parameters settings, such as very high uptake/permeability of the contaminant
#' or exposure series which represent step functions, the numerical approximation
#' might deteriorate and yield invalid results. In this case try to decrease the
#' allowed max step width by setting the argument `hmax` with various values.
#' Start with `hmax=1` and decrease the value by orders of 10. It is not
#' possible or desirable to reduce `hmax` to extremely small values, as the
#' ODE solver will require more CPU time and simulation will become inefficient.
#'
#' Oftentimes, it will be computational more efficient to adapt the solver's
#' error tolerances `atol` and `rtol` than reducing the step width `hmax` to achieve
#' stable numerics. Start by decreasing deSolve's default values by
#' orders of ten until the simulation yields acceptable results, see e.g.
#' [deSolve::lsoda()] for more information on error tolerances.
#'
#' As an alternative to adapting solver parameters, it might be worthwhile to
#' try other numerical schemes which can handle stiff ODEs, such as Radau, LSODA,
#' or LSODES. To change solvers, set the `method` argument.
#' To select e.g. the Radau scheme, set `method="radau"`. For LSODA, set `method="lsoda"`.
#' Radau performs better than LSODA in some cases, as the latter method can return
#' biologically nonsensical results without raising an error. See [deSolve::ode()]
#' for details on available ODE solvers.
#'
#'
#' @param x [scenario] to simulate
# @param times optional `numeric` vector of time points for which results are returned,
# overrides settings of the scenario
#' @param ... additional parameters passed on to ODE solver
#'
#' @return A `data.frame` with the time-series of simulation results
#' @export
#' @examples
#' minnow_sd %>% simulate() # tidy syntax
#' simulate(minnow_sd) # base R syntax
#'
#' # Set new output times
#' minnow_sd %>% simulate(times=c(0,4))
#'
#' # Modify simulated time frame
#' minnow_sd %>% simulate(times=c(0,10))
#'
#' # Use an alternative exposure profile than defined in the scenario
#' minnow_sd %>% set_exposure(data.frame(t=0,c=10), reset_times=FALSE) %>% simulate()
#'
#' ##
#' ## Precision of results
#'
#' # A large number of output times forces smaller solver time steps
#' minnow_it %>% simulate(times=seq(0,1,0.001)) %>% tail()
#'
#' # Defining only two output times allows for larger solver time steps
#' minnow_it %>% simulate(times=c(0,1))
#'
#' # The difference between results can be reduced by limiting the solver's
#' # maximum step length
#' minnow_it %>% simulate(times=c(0,1), hmax=0.001)
#'
#' # The same numerical precision can be achieved by switching to
#' # the Radau scheme
#' minnow_it %>% simulate(times=c(0,1), method="radau")
#'
#' # A very small step length may reduce the difference even further but may
#' # also exceed the allowed number of steps per output interval. The following
#' # simulation will be aborted with a solver error:
#' try(
#'   minnow_it %>% simulate(times=c(0,1), hmax=0.0001)
#' )
#'
#' # However, the solver's max number of allowed steps can be increased:
#' minnow_it %>% simulate(times=c(0,1), hmax=0.0001, maxsteps=10^5)
setGeneric("simulate", function(x, ...) standardGeneric("simulate"), signature="x")

#' default for all models using sliding exposure windows
#' @rdname simulate
#' @include class-EffectScenario.R
setMethod("simulate", "EffectScenario", function(x, ...) solver(scenario=x, ...))

#' @rdname simulate
#' @include class-Transferable.R
setMethod("simulate", "Transferable", function(x, ...) simulate_transfer(scenario=x, ...))

#' @rdname simulate
#' @include class-ScenarioSequence.R
setMethod("simulate", "ScenarioSequence", function(x, ...) simulate_seq(seq=x, ...))

# Simulate a scenario where (biomass) quantities are transferred and reset
# at certain time points
#
# The base scenario is split at each transfer time point and each period
# is simulated individually. After each transfer, biomass and other
# state variables are modified to a new initial state where simulation
# continues in the next period.
#
# @param scenario EffectScenario to simulate
# @param times optional numerical vector overriding the scenario's output times
# @param in_sequence optional logical, set to TRUE if simulated scenario is part
#   of a scenario sequence, then it will return a proposed initial state for
#   simulate_seq to use
# @param ... additional parameters passend on to [solver()]
# @return data.frame
simulate_transfer <- function(scenario, times, in_sequence=FALSE, ...) {
  if(!has_transfer(scenario)) # shortcut if no transfers were defined
    return(solver(scenario, times=times, ...))

  if(missing(times))
    times <- scenario@times
  if(length(times)<2)
    stop("at least two output times required")

  t_min <- min(times)
  if(t_min < 0)
    stop("output times must not be negative")
  t_max <- max(times)

  # find transfer time points that occur during the simulated period
  tr_points <- vector("numeric")
  ends_on_transfer <- FALSE
  if(has_regular_transfer(scenario)) {
    # sim has to be long enough for transfers to occur
    if(t_max >= scenario@transfer.interval) {
      tr_points <- seq(scenario@transfer.interval, t_max, scenario@transfer.interval)
    }
  } else {
    tr_points <- scenario@transfer.times
  }
  if(length(tr_points) > 0)
    ends_on_transfer <- tail(tr_points, 1) == t_max
  # limit vector to transfer time points which occur during the simulated period.
  # but exclude the starting time as a potential transfer
  tr_points <- tr_points[tr_points > t_min & tr_points <= t_max]

  # biomass level after each transfer
  if(length(scenario@transfer.biomass) == 1)
    tr_biomass <- rep(scenario@transfer.biomass, times=length(tr_points))
  else if(length(scenario@transfer.biomass) == length(tr_points))
    tr_biomass <- scenario@transfer.biomass
  else
    stop("length of biomass and transfer times vectors do not match", call.=FALSE)

  # if no transfers occur -> early exit
  if(length(tr_points) == 0 & !ends_on_transfer)
    return(solver(scenario, times=times, ...))

  # transfer time points must be contained in output time points
  if(length(setdiff(tr_points, times)) > 0)
    stop(paste("transfer time points missing in output times:", paste(setdiff(tr_points, times), sep=",")))

  df <- data.frame()
  t_start <- t_min
  #tr_points <- c(tr_points, t_max)
  #tr_biomass <- c(tr_biomass, NA_real_) # dummy value
  # simulate population until next transfer to new medium
  for(i in seq_along(tr_points)) {
    t <- tr_points[i]
    period <- times[times >= t_start & times <= t]
    # simulate
    out <- solver(scenario, times=period, ...)

    # append results to output data.frame
    if(i==1) {
      df <- rbind(df, out)
    } else {
      # append simulation results, but exclude values for the starting time point
      # where the transfer occurred. the starting time point `t` could be included more
      # than once in the `times` or `period` vector
      df <- rbind(df, out[-seq(sum(period == t)),])
    }

    # Transfer a fixed number of biomass to a new medium:
    # use last state as starting point
    init <- unlist(tail(out, 1)[names(scenario@init)])
    # scale internal toxicant mass in proportion to new biomass
    BM.fac <- tr_biomass[i] / init[[scenario@transfer.comp.biomass]]
    # reset biomass
    init[[scenario@transfer.comp.biomass]] <- tr_biomass[i]
    # scale other compartments relative to new biomass
    init[scenario@transfer.comp.scaled] <- init[scenario@transfer.comp.scaled] * BM.fac
    scenario <- set_init(scenario, init)
    t_start <- t
  }
  # simulate the remainder of the scenario if any exists
  if(t_max > max(tr_points)) {
    period <- times[times >= t_start]
    out <- solver(scenario, times=period, ...)
    df <- rbind(df, out[-1,])
  }

  # add metadata at which system state the subsequent simulation should start
  # if they were simulated in a sequence, cf. simulate_seq()
  if(in_sequence) {
    if(ends_on_transfer)
      attr(df, "next_init") <- init
    else
      attr(df, "next_init") <- tail(df, 1)[names(scenario@init)]
  }
  rownames(df) <- NULL
  df
}

# Simulate a sequence of scenarios
#
# The sequence of scenarios is simulated end to end, i.e. the next scenario
# starts where the last one ended. Start and end time points must match
# exactly, state variables of all scenarios must be identical
#
# @return data.frame
simulate_seq <- function(seq, times, ...) {
  if(length(seq@scenarios)==0)
    stop("no scenarios in sequence")

  df <- data.frame() # result table
  init <- seq@scenarios[[1]]@init
  t.start <- seq@scenarios[[1]]@times[1]
  nms <- names(init)

  for(i in seq(length(seq@scenarios))) {
    # get current scenario and re-set init state
    sc <- set_init(seq@scenarios[[i]], init)
    # if explicit time points were provided, try to accommodate them with the
    # scenario's settings
    if(!missing(times)) {
      t.end <- sc@times[length(sc@times)]
      sc.times <- times[times>=t.start & times<=t.end]
      if(t.start < sc.times[1])
        sc.times <- c(t.start, sc.times)
      sc <- set_times(sc, sc.times)
    }
    # check if current scenario starts where the previous ended
    if(sc@times[1] != t.start)
      stop("scenarios are not on a continuous time scale")
    t.start <- tail(sc@times,1)
    # we simulate each scenario as is
    out <- simulate(sc, times, in_sequence=TRUE, ...)
    # append results to output data.frame
    if(i==1)
      df <- rbind(df, out)
    else
      df <- rbind(df, out[-1,])

    # which system state to use as initial state for next scenario
    # in sequence?
    # a) if simulate() returned suitable metadata then use that
    # b) otherwise, use last system state of last scenario
    init <- attr(out, "next_init", exact=TRUE)
    if(is.null(init))
      init <- tail(out,1)[nms]
  }
  attr(df, "next_init") <- NULL
  rownames(df) <- NULL
  df
}

#' Batch simulation of several effect scenarios
#'
#' An effect scenario contains only one exposure level. Consequently, the
#' simulation of an effect scenario (e.g. metsulfuron %>% simulate will return
#' the results for one exposure level only). However, in a laboratory experiment
#' examining the effects of different exposure levels on a biological system,
#' a batch simulation approach would involve running multiple simulations with
#' varying exposure or treatment conditions. To illustrate, if the objective is
#' to examine the impact of a substance on cell growth, the simulation model
#' could be designed to replicate the cell growth dynamics under varying
#' concentrations of the substance. Each simulation run would represent a
#' specific exposure level, ranging from low to high concentrations of the
#' chemical. To simulate such a laboratory experiment, the simulate_batch
#' function can be used. All exposure series are saved in the treatment argument.
#' The first column contains the time, the second column the concentration, and
#' the third column the trial name (exposure level, e.g. 'T1', 'T2', 'T3').
#'
#' @param model_base effect scenario object with mean parameters
#' @param treatments treatments exposure levels as data frame (time, conc, trial)
#' @param param_sample data.frame with parameter sample
#' @return a `data.frame` with simulation results
#' @export
#' @examples
#'
#' exposure <- data.frame(time = Schmitt2013$t,
#'             conc = Schmitt2013$conc,
#'            trial = Schmitt2013$ID)
#'
#' sim_result <- simulate_batch(model_base = metsulfuron,
#'                             treatments = exposure)
#'
#'
#'
#'
simulate_batch <- function(model_base,
                           treatments,
                           param_sample = NULL
                           ) {
  # Check if columns 'time' and 'conc' exist
  if(!("time" %in% colnames(treatments)) || !("conc" %in% colnames(treatments))) {
    stop("Columns 'time' and/or 'conc' not found in treatments dataframe.")
  }
  exp_levels <- unique(treatments[,3])
  treatments[,3] <- factor(treatments[,3], levels = exp_levels)
  # simulate scenarios with a defined step width in time
  t_max <- max(treatments[, 1]) # 1st column should contain time
  sim_times <- seq(0, t_max, 0.1) # magic number: 0.1
  # simulate base scenario (mean line)
  # Initialize an empty list to store data frames
  list_of_effect_sets <- list()
  # create list of effect scenario objects for each trial
  list_of_effect_sets <- lapply(split(treatments, treatments$trial), function(x){
    model_base %>%
      set_exposure(data.frame(time = x[,1], conc = x[,2]))
  })
                                                # time, conc

  # simulate
  df <- data.frame()
  df_one_run <- data.frame()
  if (is.null(param_sample)) { # run best fit params for all treatments
    simulated_results <- purrr::map(list_of_effect_sets, ~ .x %>%
                                      simulate(sim_times = sim_times))
    # add trial name
    simulated_results <- purrr::map2(
      simulated_results,
      exp_levels,
      ~ dplyr::mutate(.x, trial = .y)
    )
    df <- dplyr::bind_rows(simulated_results)
  } else { # run parameter sample for all treatments
    for (i in 1:nrow(param_sample)) {
      simulated_results <- purrr::map(list_of_effect_sets, ~ .x %>%
                                        set_param(param_sample[i, ]) %>%
                                        simulate(sim_times = sim_times))
      # add trial name
      simulated_results <- purrr::map2(
        simulated_results,
        exp_levels,
        ~ dplyr::mutate(.x, trial = .y)
      )
      # bind
      df_one_run <- dplyr::bind_rows(simulated_results)
      df <- rbind(df, df_one_run)
    }
  }
  return(df)
}
