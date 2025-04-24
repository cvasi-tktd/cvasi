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
#' The minimum and maximum of given time points define the simulated
#' period. However, the simulation can also be limited to
#' a subset of time points by enabling a moving exposure window, see [set_window()].
#'
#' Results will be returned for each output time point. Precision of the numeric solver
#' may be affected by chosen output times in certain cases. Hence, small deviations
#' in results should be expected if different output times are set. This effect
#' can be mitigated by either defining are sufficiently small time step for the solver
#' using argument `hmax` or by decreasing the error tolerances `atol` and `rtol`.
#' These arguments are passed to the solver, see e.g. [deSolve::lsoda()] for details.
#'
#' ### Optional output variables
#' Some models support adding intermediary model variables to the return value
#' of `simulate()`. Analyzing the additional outputs may be helpful to understand
#' model behavior and support finding potential issues in model parameterization.
#'
#' Optional outputs are enabled by setting the parameter `nout` to a value greater
#' than zero. If `nout` is set to `n`, then the first `n` optional output columns
#' will be returned along the normal simulation result.
#'
#' Which optional outputs are available depends on the model/scenario at hand.
#' Please refer to the model documentation for details.
#' As an example, the [GUTS-RED-IT][GUTS_RED_IT()] model supports adding the
#' external toxicant concentration to the output by setting `nout=1`:
#'
#' `minnow_it %>% simulate(nout=1)`
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
#   overrides settings of the scenario
#' @param ... additional parameters passed on to ODE solver
#'
#' @return A `data.frame` with the time-series of simulation results
#' @export
#' @examples
#' # base R syntax
#' simulate(minnow_sd)
#' # tidy syntax with the same result
#' minnow_sd %>% simulate()
#'
#' # Extend the simulated time frame to the interval [0, 10]
#' minnow_sd %>%
#'   set_times(seq(0, 10)) %>%
#'   simulate()
#'
#' # Use an alternative exposure profile, but keep the original output times
#' minnow_sd %>%
#'   set_exposure(data.frame(t=0, c=10), reset_times=FALSE) %>%
#'   simulate()
#'
#' ##
#' ## Precision of results
#'
#' # A large number of output times forces smaller solver time steps
#' minnow_it %>%
#'   set_times(seq(0, 10, 0.001)) %>%
#'   simulate() %>%
#'   tail()
#'
#' # Defining only two output times allows the ODE solver to make larger steps
#' # in time during numerical integration. However, results can become
#' # imprecise.
#' minnow_long <- minnow_it %>% set_times(c(0, 10))
#' minnow_long %>% simulate()
#'
#' # Numerical precision of results can be increased by limiting the solver's
#' # maximum step length in time using argument `hmax`.
#' minnow_long %>% simulate(hmax=0.005)
#'
#' # A similar numerical precision can be achieved by switching to an alternative
#' # numerical integration scheme, such as the Radau scheme, without limiting
#' # the step length.
#' minnow_long %>% simulate(method="radau")
#'
#' # Reducing the step length even further may increase numerical precision, but
#' # may exceed the solver's allowed number of integration steps per output interval.
#' # The following simulation will be aborted with a solver error:
#' try(
#'   minnow_long %>% simulate(hmax=0.001)
#' )
#'
#' # However, the solver's maximum number of allowed steps can be increased,
#' # if needed, using the argument `maxsteps`:
#' minnow_long %>% simulate(hmax=0.001, maxsteps=10^5)
setGeneric("simulate", function(x, ...) standardGeneric("simulate"), signature="x")

#' Default for all models using moving exposure windows
#' @rdname simulate
#' @include class-EffectScenario.R
setMethod("simulate", "EffectScenario", function(x, ...) simulate_scenario(x, ...))

#' @rdname simulate
#' @include class-Transferable.R
setMethod("simulate", "Transferable", function(x, ...) simulate_transfer(scenario=x, ...))

#' @rdname simulate
#' @include sequence.R
setMethod("simulate", "ScenarioSequence", function(x, ...) simulate_seq(seq=x, ...))

#' @rdname simulate
#' @include batch.R
setMethod("simulate", "SimulationBatch", function(x, ...) simulate_batch2(batch=x, ...))


# Wrapper for solver function to enforce setting of a S3 class for all simulation
# results.
#' @importFrom utils capture.output
simulate_scenario <- function(x, times, .suppress=FALSE, ...) {
  # set times vector, mostly for backwards compatibility
  if(!missing(times)) {
    x <- set_times(x, times)
  }

  # listof conditions raised by deSolve
  has_error <- FALSE
  has_warning <- FALSE
  conds <- list()
  out <- capture.output(
           withCallingHandlers(
             rs <- withRestarts(solver(scenario=x, ...), muffleStop=function(x) data.frame() ),
             error=function(x) {
               conds <<- append(conds, list(c("error", x$call, x$message)))
               has_error <<- TRUE
               invokeRestart("muffleStop")
             },
             warning=function(x) {
               conds <<- append(conds, list(c("warning", x$call, x$message)))
               has_warning <<- TRUE
               tryInvokeRestart("muffleWarning")
             })
           )
  # convert deSolve return type to an actual data.frame
  if(is(rs, "deSolve")) {
    rs <- ode2df(rs)
  }
  class(rs) <- c("cvasi_simulate", "data.frame")
  was_aborted <- num_aborted(rs)
  attr(rs, "cvasi_status") <- "success"

  # suppress any conditions, this simplifies the calling code and also
  # drastically reduces the time spent in this function
  if(.suppress)
  {
    if(was_aborted)
      attr(rs, "cvasi_status") <- "aborted"
    else if(has_error)
      attr(rs, "cvasi_status") <- "error"
    else if(has_warning)
      attr(rs, "cvasi_status") <- "warning"
  }
  else
  {
    # save any output messages from deSolve, i.e. error output
    attr(rs, "desolve_output") <- out
    attr(rs, "desolve_conds") <- conds
    # set custom class for usability features such as plotting

    attr(rs, "cvasi_status") <- "success"
    if(was_aborted) {
      attr(rs, "cvasi_status") <- "aborted"
      warn1 <- which.min(sapply(conds, function(lst) lst[[1]] == "warning"))
      if(length(warn1) == 0) {
        conds <- list(list("", "", "An unknown error occurred."))
        warn1 <- 1
      }
      warn(c("Simulation aborted.", conds[[warn1]][[3]]),
           footer = "Please run `num_info()` on result to get help on numerical issues.")
    }
    else if(has_error) {
      # find all error messages
      err <- sapply(conds, function(lst) ifelse(lst[[1]] == "error", unlist(tail(lst, n=1)), NA_character_))
      abort(c("Simulation failed", err[!is.na(err)]), class="cvasi_error")
    }
    else if(has_warning) {
      attr(rs, "cvasi_status") <- "warning"
      warn1 <- which.min(sapply(conds, function(lst) lst[[1]] == "warning"))
      warn(c("Issues during simulation.", conds[[warn1]][[3]]),
           footer = "Please run `num_info()` on result to get help on numerical issues.")
    }
  }
  rs
}

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
# @param .in_sequence optional logical, set to TRUE if simulated scenario is part
#   of a scenario sequence, then it will return a proposed initial state for
#   simulate_seq to use
# @param ... additional parameters passend on to [solver()]
# @return data.frame
simulate_transfer <- function(scenario, times, .in_sequence=FALSE, ...) {
  if(!has_transfer(scenario)) # shortcut if no transfers were defined
    return(simulate_scenario(scenario, times=times, ...))
  if(!missing(times))
    scenario <- set_times(scenario, times)

  times <- scenario@times
  t_min <- min(times)
  t_max <- max(times)

  # find transfer time points that occur during the simulated period
  tr_points <- vector("numeric")
  ends_on_transfer <- FALSE
  if(has_regular_transfer(scenario)) {
    seq_start <- ceiling(t_min / scenario@transfer.interval) * scenario@transfer.interval
    seq_end <- t_max
    if(seq_start <= seq_end) # avoid errors in case no transfer occurs
      tr_points <- seq(seq_start, seq_end, scenario@transfer.interval)
  } else {
    tr_points <- scenario@transfer.times
  }
  # does simulation period end on a transfer?
  if(length(tr_points) > 0)
    ends_on_transfer <- tr_points[length(tr_points)] == t_max
  # limit vector to transfer time points which occur during the simulated period.
  # but exclude the starting time as a potential transfer
  tr_points <- tr_points[tr_points > t_min & tr_points <= t_max]

  # if no transfer occurs during simulated period -> early exit
  if(length(tr_points) == 0 & !ends_on_transfer)
    return(simulate_scenario(set_times(scenario, times), ...))

  # add all transfer time points to the output times vector
  tr_inserted <- setdiff(tr_points, times)
  if(length(tr_inserted) > 0)
    times <- sort(c(times, tr_inserted))

  # biomass level after each transfer
  if(length(scenario@transfer.biomass) == 1)
    tr_biomass <- rep(scenario@transfer.biomass, times=length(tr_points))
  else if(length(scenario@transfer.biomass) == length(tr_points))
    tr_biomass <- scenario@transfer.biomass
  else
    stop("length of biomass and transfer times vectors do not match", call.=FALSE)

  df <- data.frame()
  t_start <- t_min
  # add last time point to also simulate the remainder of the scenario, if it exists
  if(!ends_on_transfer) {
    tr_points <- c(tr_points, t_max)
    # repeat the last value, although it will never be used
    tr_biomass <- c(tr_biomass, tail(tr_biomass, n=1))
  }
  # simulate population until next transfer to new medium
  for(i in seq_along(tr_points))
  {
    tr <- tr_points[i]
    period <- times[times >= t_start & times <= tr]
    out <- simulate_scenario(set_times(scenario, period), ...)

    # select rows/time points:
    # 1) that do NOT occur directly after a transfer, i.e. the start of a simulation
    selector <- i == 1 | period != t_start
    # 2) that were NOT inserted for technical reasons
    if(length(tr_inserted) > 0) {
      selector <- selector & !(period %in% tr_inserted)
    }
    df <- rbind(df, out[selector, ])

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
    t_start <- tr
  }

  # add metadata at which system state the subsequent simulation should start
  # if they were simulated in a sequence, cf. simulate_seq()
  if(.in_sequence) {
    if(ends_on_transfer)
      attr(df, "next_init") <- init
    else
      attr(df, "next_init") <- tail(df, 1)[names(scenario@init)]
  }
  rownames(df) <- NULL
  class(df) <- c("cvasi_simulate", "data.frame")
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
  if(length(seq) == 0)
    stop("No scenarios in sequence")
  if(!missing(times)) {
    seq <- set_times(seq, times)
  }
  # for backwards compatibility, include all breaks in output times
  # since v1.5.0
  if(methods::.hasSlot(seq, "inc_start")) {
    inc_start <- seq@inc_start
    inc_end <- seq@inc_end
  } else {
    inc_start <- c(TRUE, rep(FALSE, length(seq) - 1))
    inc_end <- rep(TRUE, length(seq))
  }

  df <- data.frame() # result table
  init <- seq[[1]]@init
  t_start <- NA_real_
  nms <- names(init)

  for(i in seq_along(seq))
  {
    # skip scenario if no output times defined
    if(length(seq[[i]]@times) == 0) {
      next
    }
    # get the first output time from the sequence
    if(is.na(t_start)) {
      t_start <- seq[[i]]@times[1]
    }

    # get current scenario and re-set init state
    sc <- set_init(seq[[i]], init)
    # check if current scenario starts where the previous ended
    if(sc@times[1] != t_start)
      stop("Scenarios are not on a continuous time scale")
    # we simulate each scenario as is
    out <- simulate(sc, .in_sequence=TRUE, ...)

    # which system state to use as initial state for next scenario
    # in sequence?
    # a) if simulate() returned suitable metadata then use that
    # b) otherwise, use last system state of last scenario
    init <- attr(out, "next_init", exact=TRUE)
    if(is.null(init)) {
      init <- tail(out, 1)[nms]
    }

    # filter certain output times from result if they are not supposed to be reported
    if(!inc_start[[i]]) {
      out <- out[sc@times != t_start, ]
    }
    if(!inc_end[[i]]) {
      t_end <- sc@times[length(sc@times)]
      out <- out[out[, 1] != t_end, ]
    }

    t_start <- tail(sc@times, 1)
    # reported result
    df <- rbind(df, out)
  }
  attr(df, "next_init") <- NULL
  rownames(df) <- NULL
  class(df) <- c("cvasi_simulate", "data.frame")
  df
}

# Simulate a batch of scenarios
#
# A batch consist of a base scenario and number of exposure series that are
# simulated with said base scenario. This is intended to replicate the setup
# of ecotox effect studies.
#
# This function is not intended to be invoked by the user, but the user is
# expected to make a call such as `simulate(batch(myscenario, list(1, 2, 3)))`.
# Parameters that influence the format of the return value are documented
# by [batch()].
simulate_batch2 <- function(batch, times, ...) {
  # some basic checks
  if(!is.character(batch@id_col))
    stop("id column is missing")
  if(batch@format != "long" & batch@format != "wide")
    stop("invalid format selected")
  if(!missing(times))
    batch@scenarios <- lapply(batch@scenarios, set_times, times)

  # TODO parallelization strategy should be controllable by user
  df <- furrr::future_map2_dfr(
    batch@scenarios,
    names(batch@scenarios),
    function(sc, nm, id_col) {
      rs <- simulate(sc, ...)
      rs[[id_col]] <- nm
      rs
    },
    id_col=batch@id_col
  )

  # if the user specified one or more columns to select, then we will select
  # a subset from the simulation results, but always time (1st colum), and
  # the (trial) ids as well
  if(!is.null(batch@select)) {
    # magic value 1: we assume that the first column contains time
    df <- dplyr::select(df, 1, !c(1, batch@id_col) & batch@select, batch@id_col)
  }
  # pivot to wide format?
  if(batch@format == "wide") {
    df <- tidyr::pivot_wider(df, id_cols=1, names_from=batch@id_col, values_from=!c(1, batch@id_col))
  }

  df
}

#' Batch simulation using multiple exposure series
#'
#' `r lifecycle::badge("deprecated")`
#'
#' A convenience function to simulate a single base scenario with one or more
#' exposure series. This aims at reproducing the setup and results of common
#' effect studies.
#'
#' A scenario contains only one exposure series. However, laboratory experiments
#' commonly examine the effects of multiple exposure levels on a biological system.
#' A batch simulation approach would involve running multiple simulations with
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
#' @param param_sample *deprecated* parameter, no longer in use
#' @param ... additional parameters passed through to [simulate()]
#' @return a `data.frame` with simulation results
#' @export
#' @examples
#' t1 <- data.frame(time=0:10, conc=0, trial="control")  # 1st treatment level
#' t2 <- data.frame(time=0:10, conc=1, trial="T1")       # 2nd treatment level
#' treatments <- rbind(t1, t2)
#'
#' metsulfuron %>%
#'   simulate_batch(treatments)
simulate_batch <- function(model_base, treatments, param_sample=deprecated(), ...) {
  #lifecycle::deprecate_soft("1.4.0", "simulate_batch()", details="Please use `simulate(batch())` instead")
  if(is_present(param_sample)) {
    if(!is.null(param_sample)) {
      lifecycle::deprecate_stop("1.3.0", "simulate_batch(param_sample)")
    }
  }
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
  simulated_results <- purrr::map(list_of_effect_sets, ~ .x %>% simulate(), ...)
  # add trial name
  simulated_results <- purrr::map2(
    simulated_results,
    exp_levels,
    ~ dplyr::mutate(.x, trial = .y)
  )
  df <- dplyr::bind_rows(simulated_results)
  class(df) <- c("cvasi_simulate", "data.frame")
  return(df)
}
