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
#' # Set a data.frame as exposure series
#' df <- data.frame(time=c(0, 1, 2, 3), conc=c(1, 1, 0, 0))
#' Lemna_Schmitt() %>% set_exposure(df)
#'
#' # Create and set an ExposureSeries object
#' es1 <- ExposureSeries(df)
#' Lemna_Schmitt() %>% set_exposure(es1)
#'
#' # By default, the time points of the exposure series will also be used as
#' # as output times. To avoid overriding existing output times, set reset_times=FALSE
#' Lemna_Schmitt() %>%
#'   set_times(0:10) %>%
#'   set_exposure(es1, reset_times=FALSE)
#'
#' # Setting two series with one function call, creates two scenarios
#' es2 <- ExposureSeries(data.frame(time=5:10, conc=1))
#' Lemna_Schmitt() %>% set_exposure(c(es1, es2))
setGeneric("set_exposure",
           function(scenarios, series, ...) standardGeneric("set_exposure"),
           signature = c("scenarios", "series")
)

#' @rdname set_exposure
setMethod("set_exposure", c("ANY","ANY"), function(scenarios, series, ...) {
  stop("argument types not supported")
})

# single data.frame object, i.e. a time-series
#' @noRd
set_exposure_dfr <- function(scenarios, series, reset_times=TRUE) {
  if(is(series[, 1], "units") | is(series[, 2], "units")) {
    series <- units::drop_units(series)
  }
  scenarios@exposure <- ExposureSeries(series)
  if(reset_times) {
    scenarios <- set_times(scenarios, dplyr::pull(series, 1))
  }
  scenarios
}

# single ExposureSeries object
#' @noRd
set_exposure_exs <- function(scenarios, series, reset_times=TRUE) {
  # coerce to data.frame to avoid issues with data.frame-like types
  df <- as.data.frame(series@series)
  # remove any `units` information to avoid issues for now. this is not ideal but we
  # would need to check and/or remove units in many places where the series
  # is used.
  # TODO check if we can keep units
  if(is(df[, 1], "units") | is(df[, 2], "units")) {
    df <- units::drop_units(df)
  }

  # assign checked data.frame to scenario but keep also any additional data from
  # exposure series object
  series@series <- df
  scenarios@exposure <- series
  if(reset_times) {
    scenarios <- set_times(scenarios, dplyr::pull(series@series, 1))
  }

  scenarios
}

# "No exposure series" object, make sure that we do not try to reset the
# output times while updating the scenario's series.
#' @noRd
set_exposure_noexs <- function(scenarios, series, ...) {
  set_exposure_exs(scenarios, series, reset_times=FALSE)
}

# list of exposure series
#' @noRd
set_exposure_lst <- function(scenarios, series, ...) {
  lapply(series, function(sc, exs) set_exposure(sc, exs, ...), sc=scenarios)
}

#' @rdname set_exposure
setMethod("set_exposure", c("EffectScenario","data.frame"), function(scenarios, series, ...) set_exposure_dfr(scenarios, series, ...))

#' @rdname set_exposure
setMethod("set_exposure", c("EffectScenario","ExposureSeries"), set_exposure_exs)

#' @rdname set_exposure
setMethod("set_exposure", c("EffectScenario","NoExposureSeries"), set_exposure_noexs)

#' @rdname set_exposure
setMethod("set_exposure", c("EffectScenario","list"), set_exposure_lst)

#' @rdname set_exposure
setMethod("set_exposure", c("list","list"), function(scenarios, series, ...) {
  unlist(lapply(scenarios, set_exposure, series=series, ...))
})

#' @rdname set_exposure
setMethod("set_exposure", c("list","ANY"), function(scenarios, series, ...) {
  lapply(scenarios, set_exposure, series=series, ...)
})

#' @rdname set_exposure
setMethod("set_exposure", c("ScenarioSequence","ANY"), function(scenarios, series, ...) {
  scenarios@scenarios <- set_exposure(scenarios@scenarios, series, ...)
  scenarios
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


# Check if object represents a valid exposure series
# @param series a data.frame or ExposureSeries object
# @return none
check_exposure <- function(series) {
  # unbox
  if(is(series, 'ExposureSeries')) {
    series <- series@series
  }
  if(!is.data.frame(series)) {
    stop("Argument `series` must be a `data.frame` or `ExposureSeries` object.")
  }

  warn <- c()
  err <- c()
  if(nrow(series) == 0) {
    err <- c(err, "Exposure series must have at least one row.")
  }
  if(length(series) < 2) {
    err <- c(err, "Exposure series must have at least two columns.")
  }
  else {
    if(length(series) > 2)
      warn <- c(warn, "Exposure series has more than two columns, additional columns are ignored.")

    time <- dplyr::pull(series, 1)
    if(!all(is.numeric(time)))
      err <- c(err, "First column (time) must contain numeric values only.")
    else
    {
      if(is(time, "units"))
        time <- units::drop_units(time)
      # no missing, infinite, or NaN values
      if(any(is.na(time) | is.nan(time) | is.infinite(time)))
        err <- c(err, "First column (time) contains invalid values (e.g. NA, NaN, Inf).")
      # time must be in ascending order
      if(any(tidyr::replace_na(diff(time), 0) < 0))
        err <- c(err, "First column (time) must be in ascending order.")
    }

    exp <- dplyr::pull(series, 2)
    if(!all(is.numeric(exp)))
      err <- c(err, "Second column (exposure) must contain numeric values only.")
    else {
      # no missing, infinite, or NaN values
      if(any(is.na(exp) | is.nan(exp) | is.infinite(exp)))
        err <- c(err, "Second column (exposure) contains invalid values (e.g. NA, NaN, Inf).")
    }
  }

  if(length(warn) > 0) {
    if(length(warn) > 1)
      warn <- paste("*", warn)
    warning(warn, call.=FALSE)
  }
  if(length(err) > 0) {
    err <- c("Exposure series is malformed.", paste("*", err))
    stop(err, call.=FALSE)
  }
}
