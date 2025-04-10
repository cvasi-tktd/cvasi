#' Get output times
#'
#' @param x (vector of) [scenario] objects
#'
#' @return (list of) times vector
#' @aliases get_times,ANY-method get_times,list-method  get_times,EffectScenario-method
#'   get_times,ScenarioSequence-method
#' @seealso [set_times()]
#' @include class-EffectScenario.R sequence.R
#' @export
#'
#' @examples
#' # Create a scenario
#' myscenario <- GUTS_RED_IT() %>% set_times(0:5)
#' # Returns the defined output times
#' get_times(myscenario)
setGeneric("get_times", function(x) standardGeneric("get_times"))

#' @export
setMethod("get_times", "ANY", function(x) stop("Object type is not supported"))

#' @export
setMethod("get_times", "list", function(x) lapply(x, get_times))

#' @export
setMethod("get_times", "EffectScenario", function(x) return(x@times))

#' @export
setMethod("get_times", "ScenarioSequence", function(x) return(get_times(x@scenarios)))
