#' Set output times
#'
#' Minimum and maximum output times define the simulated period for a scenario.
#' Simulation results will be returned for each output time, see [simulate()].
#'
#' Be aware that output times may be modified by [set_exposure()]. Precision of
#' simulation results may be influenced by chosen output times, see [simulate()]
#' for more information.
#'
#' @seealso [simulate()],  [get_times()]
#' @param x vector of [scenarios]
#' @param times `numerical` vector
#' @return Vector of modified [scenarios]
#' @aliases set_times,ANY-method set_times,list-method set_times,EffectScenario-method
#'   set_times,ScenarioSequence-method
#' @include class-EffectScenario.R sequence.R
#' @export
#' @examples
#' # Set simulated period to [2,4] with output intervals of length 1
#' minnow_it %>% set_times(c(2,3,4))
#'
#' # Decrease output interval length to 0.1
#' minnow_it %>% set_times(seq(2, 4, 0.1))
setGeneric("set_times", function(x, times) standardGeneric("set_times"), signature="x")

#' @export
setMethod("set_times", "ANY", function(x, times) stop("Object type is not supported"))

#' @export
setMethod("set_times", "list", function(x, times) lapply(x, set_times, times=times))

#' @export
setMethod("set_times", "EffectScenario", function(x, times) set_times_scenario(x, times))

#' @export
setMethod("set_times", "ScenarioSequence", function(x, times) set_times_sequence(x, times))

set_times_scenario <- function(x, times) {
  if(is(times, "units")) {
    times <- units::drop_units(times)
  }
  if(!is.vector(times) | !is.numeric(times)) {
    stop("Argument `times` has invalid type")
  }
  if(length(times) < 2) {
    stop("Argument `times` is too short, must have at least two elements")
  }

  x@times <- times
  x
}

set_times_sequence <- function(x, times) {
  # for backwards compatibility
  if(!methods::.hasSlot(x, "breaks")) {
    attr(x, "breaks") <- breaks_from_sequence(x)
  }
  # assign times to each element in sequence
  for(i in seq_along(x@scenarios)) {
    x@scenarios[[i]] <- set_times(x@scenarios[[i]], times)
  }
  # modify sequence to consider breaks between elements
  x <- split_sequence(x, .messages=FALSE)
  x
}
