#' Get scenario parameters
#'
#' For scenario [sequences][sequence], only the parameters of the first scenario
#' in the sequence is returned. To access parameters of a specific scenario in
#' the sequence, use `get_param()` on the individual scenario object.
#'
#' @param x object to fetch parameters from
#' @return (list of) list(s) with key-value pairs
#' @aliases get_param,ANY-method
#' @seealso [set_param()]
#' @export
#' @examples
#' minnow_it %>% get_param()
setGeneric("get_param", function(x) standardGeneric("get_param"))
#' @export
setMethod("get_param", "ANY", function(x) stop("Object type is not supported"))
#' @export
#' @describeIn get_param Returns a list of parameter lists (if applicable)
setMethod("get_param", "list", function(x) lapply(x, get_param))
#' @export
#' @describeIn get_param Returns a list parameters for a single [scenario]
setMethod("get_param", "EffectScenario", function(x) return(x@param))
#' @export
#' @describeIn get_param Returns a list of parameter lists, one for each scenario in the sequence
setMethod("get_param", "ScenarioSequence", function(x) return(get_param(x[[1]])))
#' @export
#' @describeIn get_param Returns a list of parameters for a single [parameter_set]
setMethod("get_param", "ParameterSet", function(x) return(x@param))

# Returns the required parameters of a scenario or scenario-like object
setGeneric("get_rparam", function(x) standardGeneric("get_rparam"))
setMethod("get_rparam", "ANY", function(x) stop("Object type is not supported"))
setMethod("get_rparam", "list", function(x) lapply(x, get_rparam))
setMethod("get_rparam", "EffectScenario", function(x) return(x@param.req))
setMethod("get_rparam", "ScenarioSequence", function(x) return(get_rparam(x[[1]])))


