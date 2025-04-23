# Creates a cache of data required for effect calculation
#
# Will store all relevant moving windows as well as effects from control
# runs, if needed. All data is stored in an environment object, because these
# are the only objects which are passed by reference by R. This should allow
# some efficiency gains w.r.t. to memory usage and processing time.
#
# @param x scenario object
# @param ... additional parameters passed through to [simulate()]
# @return `environment` object with two lists: `windows` and `controls`
cache_windows <- function(x, ...) {
  if(is.vector(x))
    stop("Argument `x` must be a scenario")

  env <- new.env(hash=FALSE, size=10)
  # determine moving windows
  env$windows <- window_candidates(x)
  env$controls <- list()

  if(!is_control_required(x)) {
    return(env)
  }

  # set exposure to zero
  y <- set_noexposure(x)

  # If the control scenario has no or constant forcing functions, then we only
  # need to simulate a single control, because the results for all windows will
  # be identical anyhow
  if(has_constant_forcings(y))
  {
    y <- y %>%
      clip_scenario(env$windows[[1]]) %>%
      set_window(length=-1)
    y@control.req <- FALSE
    efx <- effect(y, ep_only=TRUE, ...)
    env$controls <- lapply(env$windows, FUN=function(win) return(efx))
  }
  # simulate all windows individually
  else
  {
    # disable control calculation
    if(is_sequence(y))
      y[[1]]@control.req <- FALSE
    else
      y@control.req <- FALSE
    # derive effect
    df <- effect(y, max_only=FALSE, ep_only=TRUE, .cache=env, ...)
    env$controls <- lapply(seq(1, nrow(df)), function(i) unlist(df[i, ]))
  }

  env
}

#' Cache control simulations
#'
#' `r lifecycle::badge("deprecated")`
#'
#' Handling of cached control simulations has been modified and is solely
#' managed by package functions. `cache_controls()` is no longer needed and
#' will raise an error if called.
#'
#' @param x parameter not used
#' @param ... parameters not used
#' @return nothing
#' @export
cache_controls <- function(x, ...) {
  lifecycle::deprecate_stop("1.5.0", "cache_controls()", details="Caching of control scenarios is handled by package functions")
}

# Returns all possible windows, i.e. simulation periods to evaluate for effects
# @param scenario a [scenario] or [sequence]
window_candidates <- function(scenario) {
  times <- get_times(scenario)
  if(length(times) == 0)
    stop("Scenario has no output times")
  t_start <- times[[1]]
  t_end <- times[[length(times)]]

  # if no moving windows defined, then use whole period as single window
  win <- get_window(scenario)
  if(win$length <= 0)
    return(list(c("window.start"=t_start, "window.end"=t_end)))
  if(win$interval <= 0)
    stop("Scenario's window.interval has invalid value")

  # create list of candidate windows
  win.starts <- seq(t_start, max(t_end - win$length, t_start), win$interval)
  lapply(win.starts, function(ws) c("window.start"=ws, "window.end"=ws + win$length))
}
