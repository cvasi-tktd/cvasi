

get_forcings <- function(scenario) {
  if(is.vector(scenario)) stop("vector arguments not supported")
  return(scenario@forcings)
}

get_req_forcings <- function(scenario) {
  if(is.vector(scenario)) stop("vector arguments not supported")
  return(scenario@forcings.req)
}

get_vars <- function(x) {
  if(is.vector(x)) stop("vector arguments not supported")
  return(names(x@init))
}



#' Returns a model name
#'
#' The function supports vectorized inputs. It will raise an error if
#' non-supported objects are passed as arguments.
#'
#' @param x \linkS4class{EffectScenario} or [`parameter_set`] objects
#'
#' @return `character` code identifying a model, e.g. `GUTS-RED-IT`
#' @export
#'
#' @examples
#' # returns `GUTS-RED-IT`
#' get_model_name(minnow_it)
setGeneric("get_model_name", function(x) standardGeneric("get_model_name"))
#' @rdname get_model_name
#' @export
setMethod("get_model_name", "ANY", function(x) stop("model name not supported for given object type"))
#' @rdname get_model_name
#' @export
setMethod("get_model_name", "list", function(x) sapply(x, get_model_name))
#' @rdname get_model_name
#' @export
setMethod("get_model_name", "EffectScenario", function(x) return(x@name))
#' @rdname get_model_name
#' @export
setMethod("get_model_name", "parameter_set", function(x) return(x@model))

#' @rdname get_model_name
#' @export
get_model <- get_model_name

#' Returns a scenario tag
#'
#' The function supports vectorized inputs. It will raise an error if
#' non-supported objects are passed as arguments.
#'
#' @param x \linkS4class{EffectScenario} or [`parameter_set`] objects
#'
#' @return `character` custom tag identifying a scenario
#' @export
#'
#' @examples
#' # returns `fathead minnow`
#' get_tag(minnow_it)
setGeneric("get_tag", function(x) standardGeneric("get_tag"))
#' @rdname get_tag
#' @export
setMethod("get_tag", "ANY", function(x) stop("scenario tag not supported for given object type"))
#' @rdname get_tag
#' @export
setMethod("get_tag", "list", function(x) sapply(x, get_tag))
#' @rdname get_tag
#' @export
setMethod("get_tag", "EffectScenario", function(x) return(x@tag))
#' @rdname get_tag
#' @export
setMethod("get_tag", "parameter_set", function(x) return(x@tag))
