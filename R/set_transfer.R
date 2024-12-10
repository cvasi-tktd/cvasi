#' Set transfer events
#'
#' A *transfer* refers to an event where a certain amount of biomass
#' is moved to a new medium after a period of time. Effectively, this resets
#' the scenario's state variable representing biomass and re-scales all state
#' variables which are correlated with biomass, such as adsorbed chemical mass.
#' This feature replicates a procedure occurring e.g. in *Lemna* effect studies
#' and may be necessary to recreate study results.
#'
#' If a transfer occurs, simulation results of that time point will report the model state
#' **before** the transfer. Be aware that if transfers are defined using the
#' `interval` argument, the transfers will always occur relative to time point
#' zero (`t = 0`). As an example, setting a regular transfer of seven days,
#' `interval =  7`, will result at transfers occurring at time points which are
#' integer multiplicates of seven, such as `t=0`, `t=7`, `t=14` and so forth.
#' The starting and end times of a scenario do not influece **when** a regular
#' transfer occurs, only **if** it occurs.
#'
#' ### Transferred biomass
#'
#' At each transfer, a defined amount of biomass is transferred to a new medium.
#' This is modeled by interrupting the simulation at a transfer time point, modifying
#' the biomass level `BM`, and scaling affected compartments according to new
#' biomass levels. Scaling of compartments depending on biomass, such as
#' *internal toxicant mass*, is necessary to correctly reflect mass balances and
#' concentrations over time.
#'
#' Transferred biomass is set using the `biomass` parameter. Is is either a
#' single numerical value in which case the same biomass level is set at each
#' transfer. Or it is a vector of numerical values with the same length as the
#' `times` parameter in which case a custom biomass level can be set for each
#' transfer. Multiple biomass levels can only be set in conjunction with
#' custom transfer time points.
#'
#' Some scenario types define default values for transferred biomass based
#' on common study set ups.
#'
#' ### Regular and custom transfer time points
#'
#' Transfers can occur either in regular intervals of time or at selected, custom
#' time points. For regular intervals, the parameter `interval` is set to a single
#' numeric value which has the same unit as the scenario's time dimension. As an
#' example: if a scenario uses the unit of *days* for time, the transfer interval
#' is also specified in *days*:
#'
#' Transfers occurring at custom time points are set by passing a numerical vector
#' to the parameter `times`. The time points' units must match with the unit of
#' time in the scenario. A custom transfer time point **must not occur at the
#' starting time point of a simulation**.
#'
#' ### Affected compartments
#'
#' Some compartments depend on biomass to correctly reflect mass balances and
#' concentrations over time, such as *internal toxicant mass*. These compartments
#' need to be scaled linearly to reflect the change in biomass levels.
#' The parameter `scaled_comp` accepts a character vector of compartment names
#' which are scaled at each transfer. This parameter should only be used with
#' custom, user-defined models. If no compartment needs to be scaled, set or
#' use the default value of `character(0)`.
#'
#'
#' @param x vector of `EffectScenario` objects
#' @param times optional `numeric` vector of time points where transfers occur
#' @param interval optional `numeric`, interval in time units of the scenario,
#'  set to `-1` to disable transfers.
#' @param biomass optional `numeric` vector, amount of biomass that is being
#'    transferred at each transfer
#' @param scaled_comp optional `character` vector of affected compartments
#'    that are scaled according to new biomass levels
#' @seealso [Lemna-models]
#' @return Modified [scenario] objects
#' @export
#' @include class-EffectScenario.R
#' @rdname set_transfer
#' @examples
#' # Simulate biomass transfer of 50 *g/m²* at a regular interval of 7 *days*
#' metsulfuron %>%
#'   set_transfer(interval=7, biomass=50) %>%
#'   simulate()
#'
#' # Simulate irregular biomass transfers occuring at days 5, 10, and 12
#' metsulfuron %>%
#'   set_transfer(times=c(5, 10, 12), biomass=50) %>%
#'   simulate()
#'
#' # Simulate irregular transfers with changing amounts of transferred biomass
#' metsulfuron %>%
#'   set_transfer(times=c(5, 10, 12), biomass=c(50, 20, 10)) %>%
#'   simulate()
#'
#' # Disable all biomass transfers
#' metsulfuron %>%
#'   set_notransfer() %>%
#'   simulate()
setGeneric("set_transfer",
           function(x, interval, times, biomass, scaled_comp) standardGeneric("set_transfer"),
           signature = "x"
)

#' @rdname set_transfer
setMethod("set_transfer", "ANY", function(x, interval, times, biomass, scaled_comp) {
  stop("type/scenario does not support transfers")
})

#' @rdname set_transfer
setMethod("set_transfer","Transferable", function(x, interval, times, biomass, scaled_comp) set_transfer2(x, interval, times, biomass, scaled_comp) )

# Function body was moved here from generic in order to be able to debug in
# RStudio. Otherwise, breakpoints would not be hit.
set_transfer2 <- function(x, interval, times, biomass, scaled_comp) {
  if(!missing(interval)) {
    if(!is.numeric(interval))
      stop("interval must be numeric")
    if(is.nan(interval) | interval <= 0)
      interval <- -1
  }
  if(!missing(times)) {
    if(length(times) == 0) # no date == empty vector
      times <- numeric(0)
    else {
      times <- unlist(times)
      if(any(is.na(times) | is.infinite(times) | !is.numeric(times)))
        stop("argument `times` contains invalid values")
      times <- sort(unique(times))
    }
  }
  if(!missing(interval) | !missing(times)) {
    if(missing(interval))
      interval <- -1
    if(missing(times))
      times <- numeric(0)
    # check  if settings are consistent
    if(interval > 0 & length(times) > 0)
      stop("arguments `interval` and `times` cannot be used at the same time")

    x@transfer.interval <- interval
    x@transfer.times <- times
  }

  if(!missing(biomass)) {
    biomass <- unlist(biomass)
    if(!is.numeric(biomass))
      stop("biomass has invalid value")
    if(any(is.nan(biomass)) | any(biomass <= 0))
      stop("biomass has invalid value")
    if(length(biomass) > 1 & length(x@transfer.times) != length(biomass))
      stop("length of biomass and transfer times vectors do not match")
    x@transfer.biomass <- biomass
  }
  if(!missing(scaled_comp)) {
    if(length(scaled_comp) == 0)
      stop("list of scaled compartments must not be empty")
    if(length(setdiff(unname(scaled_comp), names(x@init))) > 0)
      stop("scaled compartments must be state variables")
    x@transfer.comp.scaled <- scaled_comp
  }
  if(any(is.na(x@transfer.comp.biomass))) {
    stop("biomass state variable `transfer.comp.scaled` not set yet")
  }
  if(length(x@transfer.comp.biomass) != 1) {
    stop("biomass state variable `transfer.comp.scaled` must have length 1")
  }
  x
}

#' @export
#' @describeIn set_transfer Disable biomass transfers
set_notransfer <- function(x) {
  if(length(x) > 1)
    return(sapply(x, set_notransfer))

  set_transfer(x, interval=-1)
}
