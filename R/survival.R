#' Survival rate
#'
#' Derives the survival rate of individuals for *Reduced GUTS* models
#'
#' The survival rate describes the survival probability at each
#' time point. The function simulates the *GUTS* scenario and appends a column
#' `survival` to the simulation result. A value of one (`1.0`) denotes that
#' all individuals survive. A value of zero (`0.0`) denotes that no individuals
#' survived.
#'
#' Only available for *Reduced GUTS* models, see [GUTS-RED-models].
#' The equations were described by EFSA (2018).
#'
#' @param scenario an `EffectScenario` to simulate
#' @param ... additional parameters passed on to [simulate()]
#'
#' @return a `data.frame` containing simulation results
#'
#' @export
#' @inherit GUTS-RED-models references
#' @seealso [GUTS-RED-models]
#'
#' @examples
#' # calculate survival rate
#' minnow_it %>% survival()
#'
#' # plot survival over time based on a random exposure profile
#' minnow_sd %>%
#'   set_exposure(data.frame(t=1:100, c=runif(100)*10)) %>%
#'   survival() -> df
#' plot(df$time, df$survival, "l")
survival <- function(scenario, ...) {
  if(length(scenario) > 1 | is.data.frame(scenario))
    stop("multiple scenarios supplied")

  df <- simulate(scenario, ...)

  if(is_GUTS_IT(scenario)) {
    # EFSA Scientific Opinion on TKTD models, p. 33
    # doi:10.2903/j.efsa.2018.5377
    FS <- (1 / (1 + (cummax(df$D) / scenario@param[["alpha"]])^(-scenario@param[["beta"]])))
    df$survival <- (1 - FS) * exp(-df$H)
  } else if(is_GUTS_SD(scenario)) {
    # EFSA Scientific Opinion on TKTD models, p. 33
    # doi:10.2903/j.efsa.2018.5377
    df$survival <- exp(-df$H) # background hazard rate included in H (if enabled)
  } else
    stop("model not supported")
  df
}
