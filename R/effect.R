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
#' @param x vector of `EffectScenario` objects
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
#' @autoglobal
effect <- function(x, factor=1, max_only=TRUE, ep_only=FALSE, marginal_effect, ...) {
  if(is.vector(x))
    stop("vectors of scenarios not supported")
  if(length(x@endpoints) == 0)
    warning("no endpoints selected", call.=FALSE)

  # apply multiplication factor
  x@exposure@series[,2] <- x@exposure@series[,2] * factor

  # check if controls required
  if(!has_controls(x))
    x <- cache_controls(x, ...)
  ctrl_req <- is_control_required(x)

  # calculate effects for all windows
  efx <- dplyr::bind_rows(lapply(x@control, function(ctrl) {
    # current exposure window
    win <- ctrl[c(1,2)]
    # derive endpoints rel to ctrl if required, otherwise use values as they are
    rs <- fx(clip_scenario(x, window=win), window=win, ...)
    if(ctrl_req) {
      rs <- calc_effect(rs, ctrl[-c(1,2)])
    }
    # return effects including exposure window
    c(win, rs)
  }))

  # set effects with an absolute value smaller than the marginal effect
  # threshold to zero
  if(!missing(marginal_effect)) {
    if(marginal_effect>0.01)
      warning("marginal effect threshold is larger than 1%")

    efx <- dplyr::mutate_at(efx, -c(1,2),~ifelse(abs(.)<marginal_effect,0,.))
  }

  # find maximum of all endpoints and corresponding time points
  if(max_only) {
    # check if all endpoints are present in result set
    if(length(setdiff(x@endpoints, names(efx))) > 0)
      stop(paste("endpoint(s)",paste(setdiff(x@endpoints, names(efx)),collapse=","),"missing in effect result"))

    rs <- tibble::tibble(scenario=c(x))
    for(ep in x@endpoints) {
      max.efx <- dplyr::arrange(efx, dplyr::desc(.data[[ep]]), window.start)
      max.efx <- max.efx[1,c(ep,"window.start","window.end")]
      names(max.efx) <- c(ep,paste0(ep,".dat.start"),paste0(ep,".dat.end"))
      rs <- dplyr::bind_cols(rs,max.efx)
    }

    # discard timestamps and scenario?
    if(ep_only)
      rs <- unlist(rs[,x@endpoints])
    return(rs)
  }
  # return endpoints only, for all windows
  else if(ep_only) {
    return(efx[,x@endpoints])
  }

  # rename date columns, move to back
  dplyr::relocate(efx, window.start, window.end, .after=dplyr::last_col()) %>%
    dplyr::rename(dat.start=window.start, dat.end=window.end) -> efx

  # add scenario object
  efx$scenario <- list(x)
  dplyr::relocate(efx, scenario)
}

# Calculates the effect based on simulation and control. Takes into
# account some edge cases where sim/control are zero which may introduce
# NaN.
#
calc_effect <- function(sim, control) {
  rs <- 1 - sim / control # effect relative to control
  # handle zeroes in control endpoints
  if(any(is.nan(rs))) {
    idx_c0 <- control==0
    rs[idx_c0 & sim==0] <- 0 # no effect if control and sim are zero
    rs[idx_c0 & sim>0]  <- 1 # 100% effect if sim>0 but control is zero
    # everything else should stay as it is, including NaNs
  }
  # if e.g. control's endpoint>0 and current endpoint<0, the calculated effect
  # would be greater than 1.0 which we want to avoid because an effect cannot
  # be greater than 100%
  pmin(rs, 1)
}


#### Helper functions ####

#
# Returns all possible windows, i.e. simulation periods to evaluate for effects
#
window_candidates <- function(scenario, skipZeroExposure=FALSE) {
  t.start <- scenario@times[[1]]
  t.end <- tail(scenario@times,1)

  # if no window defined, then use period of output times
  if(scenario@window.length <= 0)
    return(list(c("window.start"=t.start,"window.end"=t.end)))
  if(scenario@window.interval <= 0)
    stop("window.interval has invalid value")

  # create list of candidate windows
  win.starts <- seq(t.start, max(t.end - scenario@window.length, t.start), scenario@window.interval)
  windows <- lapply(win.starts, function(ws) c("window.start"=ws,"window.end"=ws+scenario@window.length))

  # remove windows with zero exposure?
  if(skipZeroExposure) {
    sum <- sapply(windows, function(w) {
      df <- scenario@exposure@series
      sum(df[seq(max(1,sum(df[,1]<=w[[1]])), min(nrow(df),sum(df[,1]<w[[2]])+1)),][,2])
    })
    # check if at least one window has non-zero exposure
    if(any(sum>0))
      windows <- windows[sum>0]
    else # return at least one window candidate irrespective of exposure
      windows <- windows[1]
  }
  windows
}

clip_scenario <- function(scenario, window) {
  if(missing(window))
    stop("nothing to clip")
  if(length(window)!=2)
    stop("window must consist of a start and end time point")
  if(any(is.na(window)))
    stop("window contains invalid values")

  # select relevant range
  times <- scenario@times
  times <- times[times>=min(window)&times<=max(window)]
  # make sure that start and end of window are included
  if(length(times) == 0)
    times <- window[[1]]
  else if(times[1] > window[1])
    times <- c(window[[1]], times)
  if(tail(times,1) < window[2])
    times <- c(times, window[[2]])

  # clip exposure series
  exposure <- clip_forcings(scenario@exposure@series, window)
  # clip any forcings data
  forcings <- scenario@forcings
  for(fnm in names(forcings))
    forcings[[fnm]] <- clip_forcings(forcings[[fnm]], window)

  # update scenario
  scenario@times <- times
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
