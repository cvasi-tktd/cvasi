#' Set custom boundaries for model parameters
#'
#' Modifies the boundaries for parameters for one or more [scenario] or
#' [CalibrationSet] objects.
#'
#'
#' @param x object(s) to modify
#' @param pars_bound named list of lists, where the first level lists the parameters
#' by name, and the second level lists the lower and upper boundary
#' @return [scenario] or [CalibrationSet] with modified parameter boundaries
#' @export
#' @include class-EffectScenario.R
#' @examples
#' metsulfuron %>%
#'    set_param_bounds(pars_bound = list(k_phot_max = list(0,30),
#'                                      k_resp = list(0,10)))
#'
setGeneric("set_param_bounds", function(x, pars_bound) standardGeneric("set_param_bounds"))

#' @rdname set_param_bounds
#' @export
setMethod("set_param_bounds", c("EffectScenario","list"), function(x, pars_bound) {

  if(length(x@param.req) > 0) {
    unused <- setdiff(names(pars_bound),x@param.req)
    if(length(unused)>0)
      warning(paste("unused parameters:",paste(unused,collapse=",")))
  }

  x@param.low[names(pars_bound)] <- purrr::map(pars_bound, 1)
  x@param.up[names(pars_bound)] <- purrr::map(pars_bound, 2)

  return(x)

})

# CalibrationSet, single parameter_set
#' @rdname set_param_bounds
#' @export
setMethod("set_param_bounds", c("CalibrationSet","list"), function(x, pars_bound) {
  xscenario <- x@scenario
  xdata <- x@data
  xscenario <- set_param_bounds(xscenario, pars_bound)
  x <- CalibrationSet(xscenario, xdata)
})

# multiple scenarios, single parameter_set
#' @rdname set_param_bounds
#' @export
setMethod("set_param_bounds", c("list","list"), function(x, pars_bound) {
  sapply(x, set_param_bounds, pars_bound)

})
