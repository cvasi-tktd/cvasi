#' Effect level
#'
#' Derives the effect level due to toxicant exposure in the supplied scenarios.
#' Either relative to a control scenario or derived directly from model
#' endpoints, depending on model type. For scenarios with moving exposure windows,
#' the maximum effect is returned.
#'
#' By default, only the maximum effect in all moving exposure windows will
#' be returned. If argument `max_only=FALSE` is set, the returned table will
#' be converted to long-format and will contain effect levels for each
#' assessed exposure window.
#'
#' ## Calculation
#'
#' Effects are calculated similarly to *relative errors*, i.e. the difference
#' between control and treatment scenarios is divided by the absolute value
#' of the control. Effects are usually in the interval `[0,1]`, but values
#' larger than one or smaller than zero can occur. As a special case, if the
#' endpoint from the control scenario is zero, then the effect is either
#' - zero, if also the treatment is zero
#' - positive infinity, if the treatment is smaller than zero
#' - negative infinity, if the treatment is greater than zero
#'
#' As an example, a control scenario achieves a biomass of
#' 1.0 and the treatment scenario achieves a biomass of 0.9, the effect will
#' be equal to 0.1 or 10%. However, effects can take on any real value. If,
#' for example, the biomass of the previously mentioned treatment scenario drops
#' below zero, then an effect larger than 1.0 will be calculated If, instead,
#' the biomass in the treatment scenario is greater than in the control, then the
#' effect will be negative.
#'
#' ### Output formatting
#' Start and end time of exposure windows can be disabled by setting `ep_only=TRUE`.
#' Effect levels smaller than a certain threshold can be automatically set to
#' zero (`0.0`) to avoid spurious effect levels introduced by numerical errors.
#' Set `marginal_effect` to an adequate value less than 1%.
#'
#' ### Computational efficiency
#' Calculations can be sped up by providing a `data.frame` of pre-calculated
#' control scenarios for each assessed time window. As control scenarios are
#' by definition independent of any exposure multiplication factor, they can
#' be reused for repeated calculations, e.g. to derive effect profiles or
#' dose-response relationships.
#'
#' @param x a [scenario] objects
#' @param factor optional numeric value which scales the exposure time-series
#' @param ep_only logical, if TRUE only effect endpoints are returned as a vector
#' @param max_only `logical`, if `TRUE` only the maximum effect is returned, else
#' results for all effect windows are reported
#' @param marginal_effect `numeric`, if set, any effect smaller than this threshold will
#' be reported as zero to exclude pseudo-effects originating from small numerical
#' errors
#' @param ... additional parameters passed on to [simulate()]
#'
#' @return a `tibble`, by default containing scenarios, effect levels, and the
#' exposure window where the maximum effect level occurred. The number of columns
#' depends on the enabled effect endpoints and function arguments.
#'
#' By default, the first column, named `scenarios`, contains the original scenario
#' objects that were the basis of the calculation. For each effect endpoint, it
#' will be followed by one column with the maximum effect level and two columns
#' containing start and end time of the associated exposure window. If exposure
#' windows are disabled, the columns will just contain the start and end time of
#' the simulation. The effect level column will have the name of the effect
#' endpoint, start and end time will additionally have the suffixes `.dat.start`
#' and `.dat.end`, respectively.
#' @export
setGeneric("effect", function(x, ...) standardGeneric("effect"), signature="x")


#' @autoglobal
effect_scenario <- function(x, factor=1, max_only=TRUE, ep_only=FALSE, marginal_effect, .cache, ...) {
  if(is.vector(x))
    stop("vectors of scenarios not supported")
  eps <- get_endpoints(x)
  if(length(eps) == 0)
    warning("no endpoints selected", call.=FALSE)

  # apply multiplication factor
  if(is_sequence(x)) {
    for(i in seq_along(x))
      x[[i]]@exposure@series[, 2] <- x[[i]]@exposure@series[, 2] * factor
  } else {
    x@exposure@series[, 2] <- x@exposure@series[, 2] * factor
  }

  # is a cache set? it would contain window candidates as well as results of
  # control simulations (i.e. with zero exposure)
  if(missing(.cache)) {
    .cache <- cache_windows(x, ...)
  }
  ctrl_req <- is_control_required(x)

  # calculate effects for all windows
  efx <- dplyr::bind_rows(lapply(seq_along(.cache$windows), function(i) {
    # current exposure window
    win <- .cache$windows[[i]]
    # derive endpoints rel to ctrl if required, otherwise use values as they are
    rs <- fx(clip_scenario(x, window=win), ...)
    if(ctrl_req) {
      rs <- calc_effect(rs, .cache$controls[[i]])
    }
    # return effects including exposure window
    c(win, rs)
  }))

  # set effects with an absolute value smaller than the marginal effect
  # threshold to zero
  if(!missing(marginal_effect)) {
    if(marginal_effect>0.01)
      warning("marginal effect threshold is larger than 1%")

    efx <- dplyr::mutate_at(efx, -c(1,2), ~ifelse(abs(.) < marginal_effect, 0, .))
  }

  # find maximum of all endpoints and corresponding time points
  if(max_only) {
    # check if all endpoints are present in result set
    if(length(setdiff(eps, names(efx))) > 0)
      stop(paste("endpoint(s)",paste(setdiff(eps, names(efx)),collapse=","),"missing in effect result"))

    rs <- tibble::tibble(scenario=c(x))
    for(ep in eps) {
      max.efx <- dplyr::arrange(efx, dplyr::desc(.data[[ep]]), window.start)
      max.efx <- max.efx[1,c(ep,"window.start","window.end")]
      names(max.efx) <- c(ep,paste0(ep,".dat.start"),paste0(ep,".dat.end"))
      rs <- dplyr::bind_cols(rs,max.efx)
    }

    # discard timestamps and scenario?
    if(ep_only)
      rs <- unlist(rs[, eps])
    return(rs)
  }
  # return endpoints only, for all windows
  else if(ep_only) {
    return(efx[, eps])
  }

  # rename date columns, move to back
  dplyr::relocate(efx, window.start, window.end, .after=dplyr::last_col()) %>%
    dplyr::rename(dat.start=window.start, dat.end=window.end) -> efx

  # add scenario object
  efx$scenario <- list(x)
  dplyr::relocate(efx, scenario)
}

#' @describeIn effect Default for all generic [scenarios]
#' @include class-EffectScenario.R
#' @export
#setMethod("effect", "EffectScenario", function(x, ...) effect_scenario(x, ...))
setMethod("effect", "EffectScenario", effect_scenario)

#' @describeIn effect For scenario [sequences][sequence]
#' @include sequence.R
#' @export
setMethod("effect", "ScenarioSequence", function(x, ...) effect_scenario(x, ...))

# Calculates the effect based on simulation and control. Takes into
# account some edge cases where sim/control are zero which would otherwise
# introduce NaNs.
#
# @param sim Usually a scenario with non-zero exposure, i.e. a treatment
# @param control A scenario with zero exposure
# @return Any real value
calc_effect <- function(sim, control) {
  rs <- (control - sim) / abs(control) # effect relative to control
  # handle zeroes in control endpoints
  idx_c0 <- control == 0
  if(any(idx_c0)) {
    rs[idx_c0]  <- -sign(sim[idx_c0]) * Inf # infinite effect if sim <> 0 but control is zero
    rs[idx_c0 & sim == 0] <- 0      # no effect if control and sim are zero
  }
  rs
}


#### Helper functions ####
clip_scenario <- function(scenario, window) {
  if(missing(window))
    stop("nothing to clip")
  if(length(window) != 2)
    stop("window must consist of a start and end time point")
  if(any(is.na(window)))
    stop("window contains invalid values")

  # select relevant range
  times <- get_times(scenario)
  times <- times[times >= min(window) & times <= max(window)]
  # make sure that start and end of window are included
  if(length(times) == 0)
    times <- window[[1]]
  else if(times[1] > window[1])
    times <- c(window[[1]], times)
  if(tail(times,1) < window[2])
    times <- c(times, window[[2]])

  # update output times
  scenario <- set_times(scenario, times)
  # do not touch exposure & forcing time-series of sequences, it's too fragile
  if(is_sequence(scenario)) {
    return(scenario)
  }

  # clip exposure series
  exposure <- clip_forcings(scenario@exposure@series, window)
  # clip any forcings data
  forcings <- scenario@forcings
  for(fnm in names(forcings))
    forcings[[fnm]] <- clip_forcings(forcings[[fnm]], window)

  # update other scenario settings
  scenario@exposure@series <- exposure
  scenario@forcings <- forcings

  scenario
}

#
# Minimze forcing time-series to reduce numerical overhead in deSolve
#
# It's okay if the resulting data.frame does not (fully) cover the requested
# window as forcings will be interpolated and extended by the solver as needed.
# Because of subtle interpolation issues, we also have to include the row just
# before and after our window of interest.
#
clip_forcings <- function(df, window) {
  if(nrow(df)<=2)
    return(df)
  df[seq(max(1,sum(df[,1]<window[1])), min(nrow(df),sum(df[,1]<=window[2])+1)),]
}
