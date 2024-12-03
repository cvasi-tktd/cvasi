# for internal use only
#' @noRd
setClass("SimulationBatch",
  slots=list(
   scenarios="list",
   id_col="character",
   format="character",
   select="ANY"
  )
)

#' Batch simulation of multiple exposure levels
#'
#' `r lifecycle::badge("experimental")`
#'
#' A convenience function to simulate a single base scenario with one or more
#' exposure levels. The functions aims at reproducing the setup and result format
#' of common effect studies.
#'
#' Simulating a [scenario] is generally limited to assessing a single exposure
#' series. However, laboratory experiments commonly examine the effects of
#' multiple exposure levels on a biological system.
#' A *batch simulation* approach involves running multiple simulations with
#' varying exposure or *treatment* conditions. To illustrate: if the objective is
#' to examine the impact of a chemical on cell growth, multiple scenarios need
#' to be simulated to reproduce the cell growth dynamics under varying
#' concentrations of the assessed chemical. Each simulation run will represent a
#' specific exposure level, ranging from low to high concentrations of the
#' chemical.
#'
#' To simulate the conditions of such a laboratory experiment, the scenarios and
#' exposure levels can either be created and simulated individually, or the
#' `batch()` function can be used for ease of use.
#'
#' ### Exposure series
#' An exposure level can be represented by one of the following types:
#'
#' - A single constant value
#' - A `data.frame` with two columns, the first column representing time, the
#'   second representing the exposure level, e.g. a concentration
#' - An [ExposureSeries] object
#'
#' Each exposure level will be simulated using the base scenario. If the exposure
#' levels are provided as a named list, the names will also appear in the return
#' value of `simulate()`. This behavior can be used, for example, to define unique
#' study IDs for particular exposure levels.
#'
#' ### Study / exposure IDs
#' The list of exposure levels can be supplied as a named list. The names
#' will be used as unique (study) IDs, so that the simulation results belonging
#' to any exposure levels can be identified in the output.
#' If no IDs are defined by the user, generic IDs of the form `'trial{n}'` will
#' be assigned, with `{n}` being replaced by consecutive integers starting at one.
#'
#' If the batch is passed on to [simulate()]`, the IDs will be contained in its
#' return value, e.g. as a dedicated column (long format) or as part of the
#' column names (wide format).
#'
#' ### Output format
#' The return value of [simulate()] is by default in long format, i.e. it will
#' contain one row for each output time and exposure level. It is possible to
#' pivot the tabular data to wide format, by setting the argument
#' `format = 'wide'`.
#'
#' In wide format, the output columns of each exposure level are pasted
#' next to each other. If more than one column is pivoted per exposure level,
#' then the exposure or study ID is added as a suffix to column names.
#' If the output per exposure level contains only a single column (besides time
#' and the exposure ID itself), then original column name is dropped and only
#' exposure IDs are used. See the examples section for reference.
#'
#' ### Select output columns
#' Often, only a single output column is of interest in batch simulations,
#' such as the number of surviving individuals. To ease the interpretation and
#' handling of the output of batch simulations, the columns contained in the
#' output of each simulated exposure level can be filtered. One or more columns
#' can be selected. By default, no filtering of output columns is conducted.
#'
#' As an example, to create an overview of survival probabilities (*S*) in the
#' GUTS-RED-IT example scenario `minnow_it`:
#'
#' ```
#' minnow_it %>%
#'   batch(exposure=list(0, 5, 10), select="S", format="wide") %>%
#'   simulate()
#' ```
#'
#' @param scenario a [scenario] object
#' @param exposure a named `list()` with one or more exposure levels, a level
#'    can be a constant `numeric`, a `data.frame` time series with two columns,
#'    or an [ExposureSeries] object
#' @param id_col `character`, name of column in resulting ´data.frame` which
#'    contains a trial's name or ID
#' @param format `character`, set to `'long'` for long tabular format, or
#'    `'wide'` for wide format
#' @param times_from `character`, set to `'scenario'` to use output times from
#'    scenario, or `'exposure'` to take output times from each exposure series
#' @param select optional `character` vector to select columns from the
#'    simulation output
#' @return a simulation batch object
#' @export
#' @examples
#' # Simulate a batch experiment with three constant exposure levels of
#' # 0.0, 2.0, and 5.0 µmol/L
#' simulate(batch(minnow_it, list(0, 2, 5)))
#'
#' # Alternatively, in tidyr style syntax
#' minnow_it %>%
#'   batch(exposure=list(0, 2, 5)) %>%
#'   simulate()
#'
#' # Assign unique IDs to each exposure level
#' minnow_it %>%
#'   batch(exposure=list(Control=0, TrialA=2, TrialB=5)) %>%
#'   simulate()
#'
#' # Limit simulation output to column 'S' (survival probability)
#' minnow_it %>%
#'   batch(exposure=list(Control=0, TrialA=2, TrialB=5), select="S") %>%
#'   simulate()
#'
#' # Return data in wide-format, unique IDs will be used as column names
#' minnow_it %>%
#'   batch(exposure=list(Control=0, TrialA=2, TrialB=5), select="S", format="wide") %>%
#'   simulate()
batch <- function(scenario, exposure, id_col="trial", format=c("long", "wide"),
                  times_from=c("scenario", "exposure"), select=NULL) {
  # make sure that it's a scenario
  if(!all(is_scenario(scenario)))
    stop("argument 'scenario' must contain a scenario object")
  if(length(scenario) != 1)
    stop("argument 'scenario' must contain only a single element")

  # verify types of exposure series, convert where necessary
  for(i in seq_along(exposure)) {
    val <- exposure[[i]]
    if(is(val, "ExposureSeries")) {
      # nothing else to do
    } else if(is.data.frame(val)) {
      exposure[[i]] <- ExposureSeries(val)
    } else if(is.numeric(val) & length(val) == 1) { # atomic value
      exposure[[i]] <- ExposureSeries(data.frame(time=0, conc=val))
    }
  }

  # assign ids for exposure series where necessary
  if(missing(exposure))
    stop("argument 'exposure' is missing")
  if(!is.list(exposure))
    stop("argument 'exposure' must be a list")
  if(length(exposure) == 0)
    stop("argument 'exposure' must not be empty")

  ids <- names(exposure)
  # some or all ids missing
  if(is.null(ids)) {
    message("Elements in 'exposure' are not named, assigning generic ids 'trial{n}'")
    names(exposure) <- paste0("trial", seq(length(exposure)))
  }

  ids_empty <- which(names(exposure) == "")
  if(length(ids_empty) > 0) {
    message("Some elements in 'exposure' are not named, assigning generic ids 'trial{n}'")
    nms <- names(exposure)
    nms[ids_empty] <- paste0("trial", ids_empty)
    names(exposure) <- nms
  }

  # other arguments
  format <- match.arg(format)
  times_from <- match.arg(times_from)

  # create list of scenarios
  lst <- lapply(exposure, function(exp) {
    set_exposure(scenario, exp, reset_times=(times_from == "exposure"))
    })

  new("SimulationBatch",
    scenarios=lst,
    id_col=id_col,
    format=format,
    select=select
  )
}
