# Scenario sequence class
#' @export
#' @aliases sequence
setClass("ScenarioSequence", slots=list(scenarios="list"))

#' Sequence of scenarios
#'
#' Scenario sequences can be used to e.g. implement changes in model parameters
#' over time, which otherwise would remain constant for the duration of a simulation.
#' A sequence of scenarios is treated as a single scenario and each scenario
#' is simulated one after the other. If scenario `n` in a sequence was simulated,
#' scenario `n+1` will start off in the model state where `n` had ended.
#'
#' ### Requirements
#' All scenarios in a sequence must fulfill the following requirements:
#'
#' * All scenarios must have identical state variables
#' * The *output times* of all scenarios must represent a continuous time series
#'   without gaps or overlaps
#'
#' Using the `breaks` argument, the function can split up the scenarios'
#' output times at the given break points. The break points must be within
#' the interval defined by the superset of all output times in the sequence.
#'
#' ### Limitations
#'
#' Only simulation of sequences are supported, at the moment.
#' Effects and effect profiles (EPx values) cannot be derived, yet.
#'
#' @param seq list of [scenario] objects
#' @param breaks optional vector of *numerics*, scenarios' output times will
#'   be modified so that one scenario ends at the break and the next one begins
#' @return an S4 object of type [ScenarioSequence-class]
#' @examples
#' # Create a scenario with background mortality only
#' scen1 <- minnow_it %>%
#'   set_noexposure() %>%
#'   set_times(0:10)
#' # Modify a scenario parameter, e.g. set background mortality to zero
#' scen2 <- scen1 %>% set_param(c(hb=0))
#'
#' # Create a sequence of scenarios, scenario #1 will be simulated for the
#' # time period [0, 4], and #2 for [4, 10]
#' sq <- sequence(list(scen1, scen2), breaks=c(4))
#'
#' # Simulate the sequence: the mortality stops after t=4.0, due to scenario #2
#' # being simulated after t=4.0, which disabled the background mortality
#' simulate(sq)
# # the sequence can also be used to derive effect endpoints
# effect(sq)
#'
#' @name sequence
#' @aliases ScenarioSequence-class
#' @seealso [sequence.extract]
#' @export
sequence <- function(seq, breaks=NULL) {
  if(missing(seq))
    stop("argument 'seq' is missing")
  if(!is.list(seq))
    stop("argument 'seq' must be of type list")
  if(length(seq) == 0)
    stop("sequence is empty")
  if(length(seq) == 1)
    warning("sequence has ony a single element")
  if(any(!is_scenario(seq)))
     stop("sequence must only contain scenario objects")
  if(!is.null(breaks)) {
    if(any(!is.numeric(breaks)))
      stop("argument 'breaks' must be vector of numerics")
    if(length(breaks) + 1 != length(seq))
      stop("length of 'breaks' does not fit to length of sequence")
  }

  obj <- new("ScenarioSequence", scenarios=sequence_init(seq, breaks))
  check_sequence(obj)
  obj
}

# initialize the list of a scenario sequence with a number of elements
sequence_init <- function(seq, breaks) {
  if(!is.null(breaks))
  {
    message("Modifying sequence to consider breaks ...")
    for(i in seq(1, length(seq) - 1)) {
      cur <- seq[[i]]
      nxt <- seq[[i + 1]]
      br <- breaks[[i]]
      last <- i == length(seq) - 1

      # current scenario ends at break
      times <- c(cur@times[cur@times < br], br)
      if(length(times) < 2)
        stop(paste0("  Scenario #", i, " has too few output times for t<", br))
      cur <- set_times(cur, times)
      # next scenario starts at break
      times <- c(br, nxt@times[nxt@times > br])
      if(last & length(times) < 2)
        stop(paste0("  Scenario #", i+1, " has too few output times for t>", br))
      nxt <- set_times(nxt, times)

      message(paste0("  Scenario #", i, ": simulated period [", min(cur@times), ", ", max(cur@times), "]"))
      if(last)
        message(paste0("  Scenario #", i+1, ": simulated period [", min(nxt@times), ", ", max(nxt@times), "]"))

      seq[[i]] <- cur
      seq[[i + 1]] <- nxt
    }
  }
  unname(seq)
}

#' Extract and replace elements of a sequence
#'
#' The array accessor generics allow extracting and replacing scenarios within
#' am existing sequence. `[` and `[[` work identical to
#'
#' @param x [sequence]
#' @param i index of elements to extract or replace
#' @param j *not used*
#' @param value new scenario
#' @name sequence.extract
#' @rdname sequence.extract
#' @return various
#' @examples
#' # create a sequence
#' seq <- sequence(list(minnow_it, minnow_it), breaks=3)
#'
#' seq[1]       # first element, as a list of scenarios
#' seq[c(1)]    # the same
#' seq[c(1, 2)] # both elements as a list of scenarios
#' seq[[1]]     # first element as a scenario
#'
#' # replacing single elements
#' seq[[1]] <- minnow_sd %>% set_times(1:3)
NULL


#' @export
#' @describeIn sequence.extract Returns a list of scenarios from the sequence.
setMethod("[", c("ScenarioSequence","numeric","missing","missing"), function(x, i) {
  if(missing(i))
    stop("Argument `i` is missing.")
  if(any(i < 1 | i > length(x@scenarios)))
    stop("Index is out of bounds.")

  x@scenarios[i]
})

#' @export
#' @describeIn sequence.extract Returns a single scenario from the sequence.
setMethod("[[", c("ScenarioSequence", "numeric"), function(x, i) {
  if(missing(i))
    stop("Argument `i` is missing.")
  if(length(i) != 1)
    stop("Index must be of length one.")
  if(any(i < 1 | i > length(x@scenarios)))
    stop("Index is out of bounds.")

  x@scenarios[[i]]
})

#' @export
#' @describeIn sequence.extract Returns the number of scenarios in the sequence.
setMethod("length", "ScenarioSequence", function(x) {
  length(x@scenarios)
})

#' @export
#' @describeIn sequence.extract Replaces a single scenario in the sequence.
setMethod("[[<-", c("ScenarioSequence","numeric","missing","EffectScenario"), function(x, i, j, value) {
  if(missing(i))
    stop("Argument `i` is missing.")

  if(length(i) != 1)
    stop("Index must be of length one.")
  if(any(i < 1 | i > length(x@scenarios)))
    stop("Index is out of bounds.")
  if(!all(is_scenario(value)))
    stop("New value must be a scenario.")
  if(length(value) != 1)
    stop("New value must be of length one.")

  x@scenarios[[i]] <- value
  check_sequence(x)
  x
})

# check validity of sequence elements
check_sequence <- function(seq) {
  lst <- seq@scenarios
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
      stop(sprintf("scenario #%d has no output times", i))
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
      stop(sprintf("output time gap between scenario #%d  and #%d: [_, %g], [%g, _]", i-1, i, end, start))
    } else if(start < end) {
      stop(sprintf("output time overlap between scenario #%d and #%d: [_, %g], [%g, _]", i-1, i, end, start))
    }
  }
}
