
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

#' Get scenario tag
#'
#' Returns the user-defined, custom tag of a scenario, if available. Tags
#' can be helpful to quickly distinguish scenarios of the same model type.
#' The function supports vectorized inputs.
#'
#' @param x (vector of) [scenarios] or [`parameter_set`] objects
#'
#' @return vector of `character`
#' @aliases get_tag,ANY-method get_tag,list-method get_tag,EffectScenario-method
#'   get_tag,ParameterSet-method
#' @seealso [set_tag()]
#' @export
#'
#' @examples
#' # returns `fathead minnow`
#' get_tag(minnow_it)
#'
#' # update or set a tag
#' myscenario <- minnow_it %>% set_tag("My Custom Tag")
#' # returns `My Custom Tag`
#' get_tag(myscenario)
setGeneric("get_tag", function(x) standardGeneric("get_tag"))
#' @export
setMethod("get_tag", "ANY", function(x) stop("scenario tag not supported for given object type"))
#' @export
setMethod("get_tag", "list", function(x) sapply(x, get_tag))
#' @export
setMethod("get_tag", "EffectScenario", function(x) return(x@tag))
#' @export
setMethod("get_tag", "ParameterSet", function(x) return(x@tag))
