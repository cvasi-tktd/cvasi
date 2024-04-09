# Dummy scenario class used for unit testing without the need to do actual
# time-consuming simulations

#' @noRd
#' @include class-EffectScenario.R
setClass("DummyScenario", contains="EffectScenario")

DummyScenario <- function(simresult=NA_real_, effect=NA_real_, slope=NA_real_) {
  new("DummyScenario",
      name = "Dummy",
      tag = "tag",
      param = list(simresult=simresult, effect=effect, slope=slope),
      endpoints = c("L"),
      init = c(simresult=NA_real_),
      control.req=FALSE
  ) %>%
    set_exposure(data.frame(time=1:10, conc=1))
}

# Simulation results will be the value of parameter 'simresult'
#' @noRd
#' @include solver.R
setMethod("solver", "DummyScenario", function(scenario, times, ...) solver_Dummy(scenario, times, ...))

solver_Dummy <- function(scenario, times, ...) {
  if(missing(times))
    times <- scenario@times
  data.frame(time=times, simresult=scenario@param$simresult)
}

# Effect of any dummy scenario will either be the value of parameter 'effect'
# or the first value of its exposure time-series
#' @noRd
#' @include effect.R
setMethod("fx", "DummyScenario", function(scenario, ...) fx_Dummy(scenario, ...))

fx_Dummy <- function(scenario, window, ...) {
  if(!is.na(scenario@param$effect))
    result <- scenario@param$effect
  else if(!is.na(scenario@param$slope))
    result <- min(1, scenario@exposure@series[[1,2]] * scenario@param$slope)
  else
    result <- scenario@exposure@series[[1,2]]
  setNames(rep(result, length(scenario@endpoints)), scenario@endpoints)
}



#' Dummy scenario that always fails
#' @noRd
DummyFails <- function() {
  new("DummyFails",
      name = "DummyFails",
      endpoints = "L") %>%
    set_exposure(data.frame(time=1:10, conc=1))
}
#' @noRd
setClass("DummyFails", contains="EffectScenario")
#' @noRd
#' @include solver.R
setMethod("solver", "DummyFails", function(scenario, times, ...) stop("dummy scenario failed"))
#' @noRd
setMethod("fx", "DummyFails", function(scenario, ...) stop("dummy scenario failed"))


#' Dummy scenario with inconsistent effect values
#' @noRd
DummyInconsistent <- function() {
  new("DummyInconsistent",
      name = "DummyInconsistent",
      endpoints="L") %>%
    set_exposure(data.frame(time=1:10, conc=1))
}
#' @noRd
setClass("DummyInconsistent", contains="EffectScenario")
#' @noRd
#' @include solver.R
setMethod("solver", "DummyInconsistent", function(scenario, times, ...) {
  if(missing(times))
    times <- scenario@times
  data.frame(time=times, simresult=NA_real_)
})
#' @noRd
#' @importFrom stats runif
setMethod("fx", "DummyInconsistent", function(scenario, ...) {
  c("L" = runif(1))
})

