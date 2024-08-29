#' Calibration set
#'
#' A *calibration set* combines a [scenario], observed data, and an
#' optional weighting factor into one object. The *calibration set* is used to fit
#' model parameters to observed data using [calibrate()].
#'
#' A *calibration set* usually represents a single experiment or trial.
#' Multiple experimental replicates can be combined into a single *set*, if model
#' parameters are identical between trials.
#' If model parameters were modified during a trial, e.g. a pump failure occurred
#' or flow rates changed, this can be represented by using a *scenario sequence*
#' instead of a basic [scenario]. Please refer to [sequence()] for details.
#'
#' ### Weighting
#' If more than one *calibration set* is used for fitting, then an optional
#' weighting factor can be used to scale the error term of the affected *set*.
#' @usage caliset(scenario, data, weight = 1.0)
#'
#' @param scenario a [scenario] describing conditions during the experiment
#' @param data a `data.frame` with observed data in long format containing two
#'   columns: the 1st column with `numeric` time points and 2nd column with
#'   `numeric` data to fit to.
#' @param weight optional `numeric` weight to be applied to this dataset's error
#'   term during fitting, default values is 1.0.
#'
#' @returns `caliset()` returns a *calibration set* object
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' # Get observed biomass during control experiment by Schmitt et al. (2013)
#' observed <- Schmitt2013 %>%
#'   filter(ID == "T0") %>%
#'   select(t, BM=obs)
#'
#' # Create a scenario that represents conditions during experiment
#' scenario <- metsulfuron %>%
#'   set_param(c(k_phot_fix=TRUE, k_resp=0, Emax=1)) %>%
#'   set_init(c(BM=12)) %>%
#'   set_noexposure()
#'
#' # Create a calibration set
#' cs <- caliset(scenario, observed)
#'
#' # Fit parameter 'k_phot_max' to observed biomass growth from experiment
#' calibrate(
#'   cs,
#'   par=c(k_phot_max=1),
#'   output="BM",
#'   method="Brent", # Brent is recommended for one-dimensional optimization
#'   lower=0,        # lower parameter boundary
#'   upper=0.5       # upper parameter boundary
#' ) -> fit
#' fit$par
#'
#' @name CalibrationSet
#' @aliases CalibrationSet-class caliset
NULL

#' @export
#' @aliases CalibrationSet
setClass("CalibrationSet",
  slots=list(
   scenario="ANY",
   data="data.frame",
   weight="numeric"
  )
)

# Creates a *calibration set*
#' @export
caliset <- function(scenario, data, weight=1) {
  if(!is.data.frame(data))
    stop("parameter 'data' must be a data.frame")
  if(length(data) < 2)
    stop("parameter 'data' must have at least two columns")
  if(!is.numeric(weight))
    stop("parameter 'weight' must be numeric")
  if(is.infinite(weight) | is.na(weight) | is.nan(weight))
    stop("parameter 'weight' must be finite numeric value")

  new("CalibrationSet",
      scenario=scenario,
      data=as.data.frame(data), # get rid of tibbles, which can cause issues
      weight=weight)
}

# Alias for `caliset()`
#' @export
CalibrationSet <- function(...) {
  caliset(...)
}
