#' Get scenario tag
#'
#' Returns the user-defined, custom tag of a scenario, if available.
#' Tags can be helpful to quickly distinguish scenarios by e.g. a user-specified
#' string. The function supports vectorized inputs. If more
#' than one scenario is supplied in argument `x`, then a list of tags is
#' returned.
#'
#' @param x (vector of) [scenarios] or [parameter_set] objects
#'
#' @return (list of) tag(s), returns `NA` if no tag was set
#' @aliases get_tag,ANY-method
#' @seealso [set_tag()]
#' @export
#'
#' @examples
#' # returns `fathead minnow`
#' get_tag(minnow_it)
#'
#' # update or set a tag
#' myscenario <- GUTS_RED_IT() %>% set_tag("My Custom Tag")
#' # returns `My Custom Tag`
#' get_tag(myscenario)
setGeneric("get_tag", function(x) standardGeneric("get_tag"))
#' @export
setMethod("get_tag", "ANY", function(x) stop("Object type is not supported"))
#' @export
#' @describeIn get_tag Returns a list of tags (if applicable)
setMethod("get_tag", "list", function(x) lapply(x, get_tag))
#' @export
#' @describeIn get_tag Returns the tag of a single [scenario]
setMethod("get_tag", "EffectScenario", function(x) return(x@tag))
#' @export
#' @describeIn get_tag Returns a list of tags, one for each scenario in the sequence
setMethod("get_tag", "ScenarioSequence", function(x) return(get_tag(x@scenarios)))
#' @export
#' @describeIn get_tag Returns the tag of a single [parameter_set]
setMethod("get_tag", "ParameterSet", function(x) return(x@tag))
