
#' Cache control simulations
#'
#' @param x vector of [scenario] objects
#' @param windows `list` of window tuples
#' @param skipZeroExposure `logical`, if `TRUE`, windows with zero exposure will
#'  not be included in calculations
#' @param ... additional parameters passed on to [effect()]
#'
#' @return Modified [scenario] objects
#' @export
# TODO improve doc
cache_controls <- function(x,windows,skipZeroExposure=FALSE,...) {
  if(is.vector(x)) # vector arguments
    return(lapply(x, function(y) cache_controls(y,skipZeroExposure=skipZeroExposure,...)))

  # derive window candidates
  if(missing(windows))
    windows <- window_candidates(x, skipZeroExposure=skipZeroExposure)
  x@control <- windows

  # if no control required, just cache window candidates
  if(!is_control_required(x)) {
    return(x)
  }
  # set exposure to zero
  y <- set_exposure(x, data.frame(t=0,c=0), reset_times=FALSE)

  # If the control scenario has no or constant forcing functions, then we only
  # need to simulate a single control, because the results for all windows will
  # be identical anyhow
  if(has_constant_forcings(y) & length(windows)>1) {
    y <- cache_controls(y, windows=windows[1],...)
    efx <- y@control[[1]][-c(1,2)]
    x@control <- lapply(windows,function(win) {
      c(win,efx)
    })
  }
  # simulate all windows individually
  else {
    x@control <- lapply(windows, function(w) {
      # clip scenario
      sc <- clip_scenario(y, window=w)
      # disable moving window
      sc <- set_window(sc, length=-1)
      # disable control calculation
      sc@control.req <- FALSE
      sc@control <- list(w)
      #   derive effect
      efx <- effect(sc, ep_only=TRUE,...)
      c(w,efx)
    })
  }
  x
}
