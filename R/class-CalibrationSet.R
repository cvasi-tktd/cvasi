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
#' An optional weighting factor can be used to scale the error term of a
#' whole *set* or of individual data points when fitting parameters using e.g.
#' [calibrate()].
#'
#' The vector of weights must either be of length one or have the same length
#' as the dataset. In the former case, the same weight will be applied to all
#' values in the dataset. In the latter, individual weights are applied
#' for each data point.
#'
#' @usage caliset(scenario, data, weight = 1.0)
#'
#' @param scenario a [scenario] describing conditions during the experiment
#' @param data a `data.frame` with observed data in long format containing two
#'   columns: the 1st column with `numeric` time points and 2nd column with
#'   `numeric` data to fit to.
#' @param weight optional `numeric` weight to be applied when calculating the
#'  error term for each data point. Default value is `1.0`, i.e. no weighting.
#' @param tag optional value to identify the data, e.g. a study number
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
   weight="numeric",
   tag="ANY"
  )
)

# Creates a *calibration set*
#' @export
caliset <- function(scenario, data, weight=1, tag=NULL) {
  if(!is.data.frame(data))
    stop("argument 'data' must be a data.frame")
  if(length(data) < 2)
    stop("argument 'data' must have at least two columns")
  if(!is.numeric(weight))
    stop("argument 'weight' must be numeric")
  if(any(is.infinite(weight) | is.na(weight) | is.nan(weight)))
    stop("argument 'weight' must be finite numeric value")
  # apply the same weight to all data points in dataset
  if(length(weight) == 1) {
    weight <- rep(weight, times=nrow(data))
  }
  # else: length of weight vector must match the dataset
  if(length(weight) != nrow(data)) {
    stop("argument 'weight' must have the same length as 'data'")
  }
  # get rid of tibbles and other tabular data structures which differ from
  # data.frame in subtle ways when selecting subsets with base R
  data <- as.data.frame(data)

  new("CalibrationSet",
      scenario=scenario,
      data=data,
      weight=weight,
      tag=tag
      )
}

# Outdated alias for `caliset()`
#' @export
CalibrationSet <- function(...) {
  lifecycle::deprecate_soft("1.2.0", "CalibrationSet()", "caliset()")
  caliset(...)
}
