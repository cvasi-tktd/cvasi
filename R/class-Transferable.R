#' Biomass transfer class
#'
#' By inheriting from class `Transferable`, a scenario's behavior can be
#' extended to support transfer and reset of biomass at dedicated points during
#' simulation.
#'
#' @section Biomass transfer:
#' Models supporting biomass transfer can be instructed to move a fixed amount
#' of biomass to a new medium after a period of time. This feature replicates
#' a procedure occurring in e.g. Lemna effect studies and may be necessary to
#' recreate study results.
#'
#' The biomass transfer feature assumes that always a fixed amount of
#' biomass is transferred. Transfers can occur at any fixed point in time or
#' in regular intervals. During a transfer, the biomass is reset to the
#' transferred amount and additional compartments can be scaled 1:1 accordingly,
#' to e.g. reflect the change in internal toxicant mass when biomass is modified.
#' Transfer settings can be modified using [set_transfer()].
#'
#' Any transfer time point must also be an output time point. If a transfer
#' occurs, simulation results of that time point will report the model state
#' **before** the transfer. Be aware that in order to use transfers at regular
#' intervals, the simulation must start at time point zero.
#'
#' @seealso [set_transfer()]
#' @slot transfer.times  `numeric`, vector of time points at which transfers occur,
#'  e.g. `c(7,10,14)`
#' @slot transfer.interval `numeric`, interval length until frond transfer to new
#'  medium
#' @slot transfer.biomass `numeric`, amount of biomass transferred to new medium
#' @slot transfer.comp.biomass `character` state variable which describes
#'   biomass
#' @slot transfer.comp.scaled `character` vector of state variable which will
#'   be scaled 1:1 when biomass is modified, e.g. internal toxicant mass
#' @name Transferable
#' @family scenarios
#' @aliases Biomass-transfer Transferable-class
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
