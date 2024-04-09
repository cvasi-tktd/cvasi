
#' Sequence of scenarios
#'
#' A sequence of scenarios is treated as a single scenario and each scenario
#' is simulated one after the other. If scenario `n` in a sequence was simulated,
#' scenario `n+1` will start off in the state where `n` has ended.
#' Scenario sequences can be used to implement changes in e.g. model parameters
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
#' Only simulation of sequences supported, yet. Calculation of effects or EPx
#' values not possible.
#'
#' @param seq optional `vector` of \linkS4class{EffectScenario} objects
#' @export
#' @examples
#' # create two scenarios that need to be simulated one after the other
#' scen1 <- minnow_it %>% set_times(0:3)
#' scen2 <- minnow_it %>% set_times(3:6) %>% set_param(c(kd=0))
#'
#' # create an empty sequence
#' sq <- sequence()
#' # assign scenarios to the sequence one by one
#' scenario(sq, 1) <- scen1
#' scenario(sq, 2) <- scen2
#'
#' # simulate the sequence
#' simulate(sq)
# # the sequence can also be used to derive effect endpoints
# effect(sq)
#'
#' # create a sequence and assign scenarios
#' sq <- sequence(list(scen1, scen2))
#' # assign list of scenarios
#' scenarios(sq) <- list(scen1, scen2)
sequence <- function(seq) {
  obj <- new("ScenarioSequence",
             scenarios=sequence_init(seq)
  )
  sequence_check(obj)
  obj
}

# initialize the list of a scenario sequence with a number of elements
sequence_init <- function(seq) {
  if(missing(seq)) {
    return(list())
  }

  if(is.list(seq)) {
    if(all(is_scenario(seq) | sapply(seq, is.null)))
      return(seq)
    else
      stop("argument must only contain effect scenarios or NULL")
  }
  else if(is_scenario(seq)) { # convenience function
    return(list(seq))
  }
  stop("argument must be a list of effect scenarios")
}

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
      warning(glue::glue("scenario at index {i} has no output times"))
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
      warning(glue::glue("output time gap between scenario {i-1}  and {i}: [_, {end}], [{start}, _]"))
    } else if(start < end) {
      warning(glue::glue("output time overlap between scenario {i-1} and {i}: [_, {end}], [{start}, _]"))
    }
  }
}
