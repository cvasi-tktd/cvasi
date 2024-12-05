#' Set window length
#'
#' Exposure windows are defined as a period of time at the scale of the exposure series.
#' As an example: if an exposure series has an hourly time step, a window length
#' of `24` will consider the exposure within 24 hours intervals for effect
#' calculation. The same applies for the window interval, i.e. the period between
#' considered exposure windows. Set `length=-1` to disable moving windows.
#'
#' @param x vector of `EffectScenario` objects
#' @param length `numeric`, length of exposure window to consider for effect
#'  calculation, set `length=-1` to disable moving windows
#' @param interval `numeric`, interval between considered exposure windows
#'
#' @return modified `EffectScenario` objects
#' @include class-EffectScenario.R
#' @export
#' @examples
#' # Calculate the maximum effect for all windows of 10 days length
#' metsulfuron %>%
#'   set_window(length=10, interval=1) %>%
#'   effect()
#'
#' # Disable moving exposure windows
#' metsulfuron %>%
#'   set_nowindow() %>%
#'   effect()
set_window <- function(x, length, interval) {
  # vectorized input
  if(is.vector(x)) {
    return(sapply(x, function(sc) set_window(sc, length=length, interval=interval)))
  }

  if(!missing(length)) {
    if(!is.numeric(length) | base::length(length) > 1)
      stop("length must be a scalar")
    if(length == 0)
      stop("interval must be larger than zero")
    if(length < 0) {
      length <- -1
      x@window.interval <- -1
    }

    x@window.length <- length
  }
  if(!missing(interval)) {
    if(!is.numeric(interval) | base::length(interval) > 1)
      stop("interval must be a scalar")
    if(interval <= 0) {
      interval <- -1
      x@window.length <- -1
    }

    x@window.interval <- interval
  }

  if(x@window.interval > 0 & x@window.length < 0)
    stop("window interval defined, but window length is missing")
  if(x@window.interval < 0 & x@window.length > 0)
    stop("window length defined, but window interval is missing")
  x
}

#' @describeIn set_window Disable moving windows
#' @export
set_nowindow <- function(x) {
  # vectorized input
  if(is.vector(x)) {
    return(sapply(x, set_nowindow))
  }

  set_window(x, length=-1, interval=-1)
}
