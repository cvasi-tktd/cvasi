########################
## Organizing man pages
########################

#' Lemna models
#'
#' Overview of supported *Lemna* models
#'
#' - [Lemna_Schmitt()] by Schmitt *et al.* (2013)
#' - [Lemna_SETAC()] by Klein *et al.* (2021)
#'
#' @inheritSection Transferable Biomass transfer
#' @name Lemna-models
#' @family Lemna models
#' @family models
#' @seealso [Macrophyte-models]
#' @aliases Lemna-class
NULL

# Generic Lemna class
#' @include class-EffectScenario.R class-Transferable.R
#' @export
setClass("Lemna", contains=c("Transferable", "EffectScenario"))
