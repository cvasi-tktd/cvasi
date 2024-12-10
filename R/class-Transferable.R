#' Biomass transfer class
#'
#' By inheriting from class `Transferable`, a scenario's behavior can be
#' extended to support transfer and reset of biomass at dedicated points during
#' simulation.
#'
#' @section Biomass transfer:
#' Models supporting biomass transfer can be instructed to move a fixed amount
#' of biomass to a new medium after a period of time. This feature replicates
#' a procedure occurring in e.g. *Lemna* effect studies and may be necessary to
#' recreate study results.
#'
#' The biomass transfer feature assumes that always a fixed amount of
#' biomass is transferred. Transfers can occur at any fixed point in time or
#' in regular intervals. During a transfer, the biomass is reset to the
#' transferred amount and additional compartments can be scaled 1:1 accordingly,
#' to e.g. reflect the change in internal toxicant mass when biomass is modified.
#' Transfer settings can be modified using [set_transfer()].
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
#' @seealso [set_transfer()]
#' @slot transfer.times  `numeric`, vector of custom time points at which transfers occur,
#'  e.g. `c(2,5,14)`
#' @slot transfer.interval `numeric`, length of regular interval until biomass transfer to new
#'  medium, regular transfers always occur relative to time point zero
#' @slot transfer.biomass `numeric`, amount of biomass transferred to new medium
#' @slot transfer.comp.biomass `character` state variable which describes
#'   biomass
#' @slot transfer.comp.scaled `character` vector of state variable which will
#'   be scaled 1:1 when biomass is modified, e.g. internal toxicant mass
#' @name Transferable
#' @family scenario-related
#' @aliases Biomass-transfer Transferable-class
#' @examples
#' # Simulation without biomass transfers
#' metsulfuron %>%
#'   set_noexposure() %>%
#'   set_notransfer() %>%
#'   simulate()
#'
#' # With biomass transfer every 7 days, biomass is reset to 50 *g/m²* on transfer
#' metsulfuron %>%
#'   set_noexposure() %>%
#'   set_transfer(interval=7, biomass=50) %>%
#'   simulate()
NULL

#' @export
setClass("Transferable",
         slots=list(
           transfer.times="numeric",
           transfer.interval="numeric",
           transfer.biomass="numeric",
           transfer.comp.biomass="character",
           transfer.comp.scaled="character"
         ),
         prototype=list(
           transfer.times=vector("numeric"),
           transfer.interval=-1,
           transfer.biomass=NA_real_,
           transfer.comp.biomass=NA_character_,
           transfer.comp.scaled=character(0)
         )
)
