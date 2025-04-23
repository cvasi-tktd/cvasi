get_endpoints <- function(x) {
  if(is.vector(x))
    return(lapply(x, get_endpoints))

  if(is_sequence(x))
    return(get_endpoints(x[[1]]))
  x@endpoints
}

# Get forcings
get_forcings <- function(scenario) {
  if(is.vector(scenario)) stop("vector arguments not supported")
  return(scenario@forcings)
}

# Get names of required forcings
get_req_forcings <- function(scenario) {
  if(is.vector(scenario)) stop("vector arguments not supported")
  return(scenario@forcings.req)
}

# Get names of state variables
get_vars <- function(x) {
  if(is.vector(x)) stop("vector arguments not supported")
  return(names(x@init))
}

# Get settings for moving windows
get_window <- function(x) {
  if(is.vector(x) & length(x) > 1) {
    return(lapply(x, get_window))
  }
  if(is_scenario(x)) {
    return(list(length=x@window.length, interval=x@window.interval))
  } else if(is_sequence(x)) {
    return(get_window(x[[1]]))
  }
  stop("Type not supported")
}

#' Get model name
#'
#' Returns the unique model name that is associated with a scenario, e.g.
#' `GUTS-RED-IT`. The function supports vectorized arguments.
#'
#' @param x (vector of) [scenarios] or [`parameter_set`] objects
#'
#' @return vector of `character`
#' @aliases get_model,ANY-method get_model,list-method get_model,EffectScenario-method
#'   get_model,ParameterSet-method get_model_name
#' @export
#'
#' @examples
#' # returns `GUTS-RED-IT`
#' get_model(minnow_it)
setGeneric("get_model", function(x) standardGeneric("get_model"))
#' @export
setMethod("get_model", "ANY", function(x) stop("model name not supported for given object type"))
#' @export
setMethod("get_model", "list", function(x) sapply(x, get_model))
#' @export
setMethod("get_model", "EffectScenario", function(x) return(x@name))
#' @export
setMethod("get_model", "ParameterSet", function(x) return(x@model))
# get_model-method Deprecated alias for `get_name()`
#' @export
get_model_name <- function(x) {
  # todo add lifecycle notice
  return(get_model(x))
}

