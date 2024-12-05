#' Set exposure time-series
#'
#' *Exposure* refers to the toxicant concentration an organism is exposed to.
#' In case of aquatic organisms, this would commonly be the concentration of a
#' toxicant in water. Other interpretations are possible depending on model
#' assumptions.
#'
#' Exposure time-series are generally represented by a `data.frame` containing two
#' columns. The first column for time, the second representing the
#' exposure level. The ordering of columns is mandatory. The column names
#' are non-relevant but sensible names may help documenting the
#' scenario and its data. The `data.frame`'s rows must be ordered chronologically.
#' A time-series can consist of only a single row; in this case it will represent constant
#' exposure.
#'
#' For convenience, a time-series with zero exposure can be set using
#' [set_noexposure()].
#'
#' ### Computational efficiency
#' Handling time-series is a costly task for the ODE solver due to consistency
#' checks and interpolation between time steps. How the solver interpolates
#' the time-series can be controlled by optional arguments to functions
#' such as [simulate()] and [effect()]. Please refer to [simulate()] for a brief
#' overview and [deSolve::forcings] for a detailed description.
#'
#' Exposure time-series should be kept as short as possible and as complex as
#' needed for optimal computational efficiency.
#'
#' ### Output times
#' By default, the exposure time-series' time points will also be used as output
#' times of the scenario. Any output times previously set by [set_times()] will
#' be lost. If this behavior is undesired, set the function argument
#' `reset_times=FALSE`.
#'
#' ### Multiple exposure series and scenarios
#' The functions supports modifying multiple scenarios at once: by
#' calling it with lists of [scenario] and [ExposureSeries]
#' objects. The cartesian product of all scenarios and exposure series will
#' be returned, iff the parameter `expand = TRUE` is set.
#'
#' As an example for the *expand* mode, two scenarios `A` and `B` and one
#' exposure series `g` will result in two scenarios `Ag` and `Bg`, both
#' using exposure series `g`. Two scenarios `A` and `B` as well as two
#' exposure seres `g` and `h` will result in four scenarios `Ag`,`Ah`,`Bg`,
#' and `Bh`.
#'
#' @param scenarios `vector` of [scenarios]
#' @param series `vector` of  [ExposureSeries] objects or a single `data.frame`
#' @param reset_times `logical`, if `TRUE`, the exposure time-series' time points
#'  will be set as output times. Defaults to `TRUE`
#' @param ... additional arguments
#'
#' @return `list` of `EffectScenario` objects
#' @include class-EffectScenario.R class-ExposureSeries.R
#' @export
#' @examples
#' # set a data.frame as exposure series
#' Lemna_Schmitt() %>% set_exposure(data.frame(time=c(0, 1, 2, 3), conc=c(1, 1, 0, 0)))
#'
#' # set one ExposureSeries
#' es1 <- ExposureSeries(data.frame(time=0, conc=0))
#' Lemna_Schmitt() %>% set_exposure(es1)
#'
#' # set two ExposureSeries to create two scenarios
#' es2 <- ExposureSeries(data.frame(time=5:10, conc=1))
#' Lemna_Schmitt() %>% set_exposure(c(es1, es2))
#'
#' # set one ExposureSeries without resetting existing output times
#' Lemna_Schmitt() %>%
#'   set_times(0:5) %>%
#'   set_exposure(es1, reset_times=FALSE)
setGeneric("set_exposure",
           function(scenarios, series, ...) standardGeneric("set_exposure"),
           signature = c("scenarios", "series")
)

#' @rdname set_exposure
#' @export
setMethod("set_exposure", c("ANY","ANY"), function(scenarios, series, ...) {
  stop("argument types not supported")
})

# single data.frame object, could be a time-series or a table containing
# metadata + ExposureSeries objects
#' @noRd
set_exposure_dfr <- function(scenarios, series, ...) {
  # check if its a data.frame containing ExposureSeries objects?
  if("series" %in% names(series)) {
    if(all(is_exp_series(series[["series"]])))
      return(set_exposure(scenarios, series[["series"]], ...))
  }
  # is it a data.frame containing a basic time-series?
  if(length(series) == 2) {
    # coerce to data.frame to avoid issues with data.frame-like types
    series <- as.data.frame(series)
    return(set_exposure(scenarios, ExposureSeries(series), ...))
  }
  stop("data.frame does not contain a valid time-series")
}

# single ExposureSeries object
#' @noRd
set_exposure_exs <- function(scenarios, series, reset_times=TRUE) {
  # coerce to data.frame to avoid issues with data.frame-like types
  df <- as.data.frame(series@series)
  # check data.frame structure
  if(length(df) != 2)
    stop("exposure series must have exactly two columns")
  if(!is.numeric(df[,1]) | !is.numeric(df[,2]))
    stop("exposure series columns must be numeric")

  # remove any `units` information to avoid issues
  if(any(has_units(df[,1]) | has_units(df[,2])))
    df <- units::drop_units(df)

  # assign checked data.frame to scenario but keep also any additional data from
  # exposure series object
  series@series <- df
  scenarios@exposure <- series
  if(reset_times) {
    if(nrow(series@series) >= 2) {
      scenarios <- set_times(scenarios, series@series[,1])
    } else {
      warning("exposure series is too short to be used as output times")
    }
  }

  scenarios
}

# list of exposure series
#' @noRd
set_exposure_lst <- function(scenarios, series, ...) {
  lapply(series, function(sc, exs) set_exposure(sc, exs, ...), sc=scenarios)
}

#' @rdname set_exposure
#' @export
setMethod("set_exposure", c("EffectScenario","data.frame"), set_exposure_dfr)
#' @rdname set_exposure
#' @export
setMethod("set_exposure", c("EffectScenario","ExposureSeries"), set_exposure_exs)
#' @rdname set_exposure
#' @export
setMethod("set_exposure", c("EffectScenario","list"), set_exposure_lst)
#' @rdname set_exposure
#' @export
setMethod("set_exposure", c("list","list"), function(scenarios, series, ...) {
  unlist(lapply(scenarios, set_exposure, series=series, ...))
})
#' @rdname set_exposure
#' @export
setMethod("set_exposure", c("list","ANY"), function(scenarios, series, ...) {
  lapply(scenarios, set_exposure, series=series, ...)
})

#' Set zero exposure
#'
#' The scenarios current exposure is replaced by a constant exposure time-series
#' of value zero(`0.0`). Output times are unaffected.
#'
#' @param x vector of [scenarios]
#'
#' @return vector of [scenarios]
#' @export
#'
#' @examples
#' # Derive effect size in sample scenario without toxicant exposure
#' minnow_it %>%
#'   set_noexposure() %>%
#'   effect()
set_noexposure <- function(x) {
  if(is.vector(x)) {
    return(sapply(x, function(sc) set_noexposure(sc)))
  }

  set_exposure(x, no_exposure(), reset_times=FALSE)
}
