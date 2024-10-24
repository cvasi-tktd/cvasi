#' Dynamic Energy Budget (DEB) models
#'
#' Supported models:
#' * [DEB_abj]
#' * [DEBtox]
#'
#' @name DEB-models
#' @family DEB models
#' @family models
#' @aliases Deb-class
NULL

# Parent class for all DEB related models/scenarios
#' @export
setClass("Deb", contains="EffectScenario")

