#' Class: scenario sequence
#'
#' Represents a sequence of time-continuous scenarios. For more information, please
#' refer to [sequence()].
#'
#' @slot scenarios list of \linkS4class{EffectScenario} objects
#' @export
#' @inherit sequence examples
setClass("ScenarioSequence",
   slots=list(
     scenarios="list"
   )
)

#' Access elements in a scenario sequence
#'
#' `NULL` values may be used
#'
#' @param x a *scenario sequence* object
#' @return list of scenarios
#' @rdname sequence_accessors
#' @seealso [sequence()]
#' @export
#' @examples
#' # access scenarios individually
#' sq <- sequence()
#' scenario(sq, 1) <- minnow_it %>% set_times(0:3)
#' scenario(sq, 2) <- minnow_it %>% set_times(3:6)
#'
#' # retrieve all scenarios in a sequence
#' scenarios(sq)
#' # set all scenarios at once
#' scenarios(sq) <- list(minnow_it, NULL)
setGeneric("scenarios", function(x) standardGeneric("scenarios"))

#' @rdname sequence_accessors
#' @export
setGeneric("scenarios<-", function(x, value) standardGeneric("scenarios<-"))

#' @param i numeric index of an element
#' @param value new scenario(s) to set
#' @rdname sequence_accessors
#' @export
setGeneric("scenario<-", function(x, i, value) standardGeneric("scenario<-"))

#' @rdname sequence_accessors
setMethod("scenarios", "ScenarioSequence", function(x) x@scenarios)
#' @rdname sequence_accessors
setMethod("scenarios<-", "ScenarioSequence", function(x, value) {
  if(is.list(value)) {
    if(all(is_scenario(value) | sapply(value, is.null))) {
      x@scenarios <- value
      sequence_check(x)
      return(x)
    }
  }
  stop("argument must be a list containing effect scenarios or NULL")
})
#' @rdname sequence_accessors
setMethod("scenario<-", "ScenarioSequence", function(x, i, value) {
  if(!any(is_scenario(value), is.null(value))) {
    stop("argument must be an effect scenario or NULL")
  }
  x@scenarios[[i]] <- value
  sequence_check(x)
  x
})
