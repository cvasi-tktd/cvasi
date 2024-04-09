
#' CalibrationSet class
#'
#' @slot scenario any kind of `EffectScenario` or [sequence()]
#' @slot data `data.frame` with two columns, 1st column with time points and
#' 2nd column with numerical data to fit to
#' @slot weight `numeric` weight to be applied to this datasets' error term
#'  during fitting
#'
#' @export
setClass("CalibrationSet",
         slots=list(
           scenario="ANY",
           data="data.frame",
           weight="numeric"
         )
)

#' Create a CalibrationSet object
#'
#' @description `CalibrationSet()` creates an object of the `CalibrationSet`
#'   class, which combines an `EffectScenario` and the corresponding effect data
#'   into one object. The created `CalibrationSet` object can subsequently be
#'   used for calibration of a model.
#'
#'   Instead of an `EffectScenario`, a `sequence` can also be used,
#'   e.g., when during an experiment a change in conditions is created which
#'   should not be captured by the model (e.g., a pump failure, a sudden change
#'   in temperature, or the removal of some experimental organisms from the
#'   system to avoid overcrowding, see [sequence()]).
#'
#'   Optionally, a weight can be added to the `CalibrationSet`, in case one
#'   `CalibrationSet` should have more weight in the subsequent model
#'   calibration compared to other `CalibrationSets`.
#'
#' @param scenario any kind of `EffectScenario` or [sequence()].
#' @param data a `data.frame` with effect data in long format containing two
#'   columns: the 1st column with `numeric` time points and 2nd column with
#'   `numeric` effect data to fit to.
#' @param weight optional `numeric` weight to be applied to this datasets' error
#'   term during fitting, default values is 1.
#'
#' @returns `CalibrationSet()` returns a `CalibrationSet` object with three
#'   slots containing `scenario`, `data`, and `weight`. Warnings are returned
#'   when input data are not a `data.frame` with 2 columns`.`
#'
#' @examples
#' # Lemna example with `EffectScenario` ----------------------------------------------
#' # 1st get effect data
#' eff_df <- Schmitt2013 %>%
#'   dplyr::filter(ID == "T0") %>%
#'   dplyr::select(t, obs)
#' # 2nd, create `CalibrationSet`
#' Cal_set <- CalibrationSet(scenario = metsulfuron, data = eff_df)
#' # optionally, add a weight to the data
#' Cal_set2 <- CalibrationSet(scenario = metsulfuron, data = eff_df, weight = 4)
#' # look at what is returned
#' Cal_set@data
#' Cal_set@scenario
#' Cal_set@scenario@name
#' Cal_set@weight
#'
#' # Lemna example with `sequence` ----------------------------------------------
#' # Let's change the end of the exposure scenario in the `metsulfuron` `EffectScenario`
#' metsulfuron@exposure
#' altered_exposure_end <- data.frame(
#'   time = c(5:10),
#'   c = rep(1.5, 6)
#' )
#' metsulfuron %>%
#'   set_times(c(0:4)) -> metsulfuron_1sthalf
#' metsulfuron %>%
#'   set_exposure(altered_exposure_end) %>%
#'   set_times(c(5:10)) -> metsulfuron_2ndhalf
#' # then, create a scenario sequence
#' Sequence <- sequence(
#'   c(metsulfuron_1sthalf, metsulfuron_2ndhalf)
#' )
#' # finally, create a `CalibrationSet` with the sequence
#' obs32 <- Schmitt2013 %>%
#'   dplyr::filter(ID == "T0.32") %>%
#'   dplyr::select(t, obs)
#' Cal_set3 <- CalibrationSet(Sequence, obs32)
#' # look at what is returned
#' Cal_set3@scenario@scenarios[[1]]
#'
#' @export
CalibrationSet <- function(scenario,data,weight=1) {
  if(!is.data.frame(data))
    stop("Data must be a data.frame")
  if(length(data)!=2)
    stop("Data must have two columns")
  if(!is.numeric(weight))
    stop("weight must be numeric")

  new("CalibrationSet",
      scenario=scenario,
      data=data,
      weight=weight)
}
