
#' Sequence of scenarios
#'
#' A sequence of scenarios is treated as a single scenario and each scenario
#' is simulated one after the other. If scenario `n` in a sequence was simulated,
#' scenario `n+1` will start off in the model state where `n` had ended.
#' Scenario sequences can be used to e.g. implement changes in model parameters
#' over time.
#'
#' ### Requirements
#' All scenarios in a sequence must fulfill the following requirements:
#'
#' * All scenarios must have identical state variables
#' * The *output times* of all scenarios must represent a continuous time series
#'   without gaps or overlaps
#'
#' ### Limitations
#'
#' Only simulation of sequences are supported, at the moment.
#' Effects and effect profiles (EPx values) cannot be derived, yet.
#' @return an S4 object of type [ScenarioSequence-class]
#' @examples
#' # create two scenarios that need to be simulated one after the other
#' scen1 <- minnow_it %>% set_times(0:3)
#' scen2 <- minnow_it %>% set_times(3:6) %>% set_param(c(kd=0))
#'
#' # create a sequence and assign scenarios
#' sq <- sequence(list(scen1, scen2))
#'
#' # simulate the sequence
#'
#' simulate(sq)
# # the sequence can also be used to derive effect endpoints
# effect(sq)
#'
#' @name sequence
#' @aliases ScenarioSequence-class sequence
# @aliases ScenarioSequence-class scenario<-,ScenarioSequence-method scenarios,ScenarioSequence-method
#   scenarios<-,ScenarioSequence-method sequence
NULL

# Scenario sequence class
#' @export
#' @aliases sequence
setClass("ScenarioSequence",
   slots=list(
     scenarios="list"
   )
)

# check validity of sequence elements
sequence_check <- function(seq) {
  lst <- scenarios(seq)
  if(!is.list(lst)) {
    stop("sequence does not contain a list of effect scenarios")
  }
  # check if start and end of output times match between subsequent scenarios
  for(i in seq_along(lst)) {
    cur <- lst[[i]]
    if(is.null(cur)) {
      next
    }
    if(length(cur@times) == 0) {
      warning(sprintf("scenario #%d has no output times", i))
      next
    }
    # skip further check for first element in sequence
    if(i == 1) {
      next
    }

    prev <- lst[[i-1]]
    if(is.null(prev)) { # nothing to compare to
      next
    }
    if(length(prev@times) == 0) {
      next
    }

    end <- tail(prev@times, 1)
    start <- head(cur@times, 1)
    if(end < start) {
      warning(sprintf("output time gap between scenario #%d  and #%d: [_, %g], [%g, _]", i-1, i, end, start))
    } else if(start < end) {
      warning(sprintf("output time overlap between scenario #%d and #%d: [_, %g], [%g, _]", i-1, i, end, start))
    }
  }
}

#
# Accessor functions
#

# @export
# @rdname sequence
setGeneric("scenarios", function(x) standardGeneric("scenarios"))

# @export
# @rdname sequence
setGeneric("scenarios<-", function(x, value) standardGeneric("scenarios<-"))

# @export
# @rdname sequence
setGeneric("scenario<-", function(x, i, value) standardGeneric("scenario<-"))

# @export
# @rdname sequence
setMethod("scenarios", "ScenarioSequence", function(x) x@scenarios)

# @export
# @rdname sequence
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

# @export
# @rdname sequence
setMethod("scenario<-", "ScenarioSequence", function(x, i, value) {
  if(!any(is_scenario(value), is.null(value))) {
    stop("argument must be an effect scenario or NULL")
  }
  x@scenarios[[i]] <- value
  sequence_check(x)
  x
})
