# Scenario sequence class
#' @export
#' @aliases sequence
#' @include sequence.R
setClass("ScenarioSequence",
         slots=list(scenarios="list",
                    breaks="numeric",
                    inc_start="logical",
                    inc_end="logical"))

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
    stop("Argument 'seq' is missing")
  if(!is.list(seq))
    stop("Argument 'seq' must be of type list")
  if(length(seq) == 0)
    stop("Argument `seq` is empty")
  if(length(seq) == 1)
    warning("Argument `seq` has ony a single element")
  if(any(!is_scenario(seq)))
     stop("Argument `seq` must contain scenario objects only")
  if(!is.null(breaks)) {
    if(any(!is.numeric(breaks)))
      stop("Argument 'breaks' must be vector of numerics")
    if(length(breaks) + 1 != length(seq))
      stop("Length of argument 'breaks' does not fit to length of sequence")
  }

  obj <- new("ScenarioSequence", scenarios=seq)
  obj@inc_start <- c(TRUE, rep(FALSE, length(obj) - 1))
  obj@inc_end <- rep(TRUE, length(obj))
  if(!is.null(breaks)) {
    obj@breaks <- breaks
    obj <- split_sequence(obj, .messages=FALSE)
  } else {
    obj@breaks <- breaks_from_sequence(obj)
  }
  check_sequence(obj)
  obj <- mock_as_scenario(obj)
  obj
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
#' @aliases [[<-,ScenarioSequence,numeric,ANY,ANY-method
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
  if(any(is.na(i)))
    stop("Argument `i` must not contain NAs")
  if(any(i < 1 | i > length(x@scenarios)))
    stop("Index is out of bounds.")

  x@scenarios[i]
})

#' @export
#' @describeIn sequence.extract Returns a single scenario from the sequence.
setMethod("[[", c("ScenarioSequence", "numeric"), function(x, i) {
  if(length(i) != 1)
    stop("Index must be of length one.")
  if(any(i < 1 | i > length(x@scenarios)))
    stop("Index is out of bounds.")

  x@scenarios[[i]]
})

#' @export
#' @describeIn sequence.extract Replaces a single scenario in the sequence.
setMethod("[[<-", c("ScenarioSequence","numeric","missing","EffectScenario"), function(x, i, j, value) {
  if(any(is.na(i))) {
    stop("Argument `i` must not contain NAs")
  }

  if(length(i) != 1) {
    stop("Index must be of length one")
  }
  if(any(i < 1 | i > length(x@scenarios))) {
    stop("Index is out of bounds")
  }

  x@scenarios[[i]] <- value
  check_sequence(x)
  if(i == 1) {
    x <- mock_as_scenario(x)
  }

  x
})

# do not export, re-assigning scenarios has complicated consequences
# @describeIn sequence.extract Replaces a single scenario in the sequence.
setMethod("[[<-", c("ScenarioSequence","numeric","ANY","ANY"), function(x, i, j, value) {
  stop("Assigned type not supported")
})

#' @export
#' @describeIn sequence.extract Returns the number of scenarios in the sequence.
setMethod("length", "ScenarioSequence", function(x) {
  length(x@scenarios)
})

breaks_from_sequence <- function(seq) {
  if(any(!is_sequence(seq))) {
    stop("Argument `seq` must be a sequence")
  }

  brks <- numeric(0)
  for(i in seq_along(seq@scenarios)) {
    brks <- c(brks, tail(get_times(seq@scenarios[[i]]), n=1))
  }
  # drop last item
  head(brks, n=-1)
}

split_sequence <- function(x, .messages=FALSE) {
  # check for backwards compatibility
  if(!methods::.hasSlot(x, "breaks")) {
    stop("Sequence has outdated format, please create a new sequence object")
  }

  breaks <- x@breaks
  n <- length(x)
  if(n == 1) {
    return(x)
  }

  if(.messages) {
    cli::cli_text("Splitting sequence at {n} break{?s} ...")
  }

  x@inc_start <- rep(TRUE, n)
  x@inc_end <- rep(TRUE, n)
  is_relevant <- rep(FALSE, n)
  for(i in seq_along(x))
  {
    is_last <- i == length(x)
    t_start <- if(i == 1) -Inf else breaks[[i - 1]]
    t_end <- if(is_last) Inf else breaks[[i]]
    times <- get_times(x[[i]])

    # is there overlap of the scenario and the time period we are selecting for?
    if(min(times) < t_end & max(times) > t_start) {
      times <- times[times >= t_start & times <= t_end] # reframe output times
      is_relevant[[i]] <- TRUE
    } else { # no overlap
      times <- numeric(0)
      x@inc_start[[i]] <- FALSE
      x@inc_end[[i]] <- FALSE
    }
    x@scenarios[[i]]@times <- times
  }

  # check if start and end time need to be included in the output times
  for(i in seq_along(x))
  {
    if(is_relevant[[i]])
    {
      t_start <- if(i == 1) -Inf else breaks[[i - 1]]
      t_end <- if(i == n) Inf else breaks[[i]]
      times <- get_times(x[[i]])
      any_before <- any(head(is_relevant, i - 1))
      any_after <- any(tail(is_relevant, n - i))

      # continue where previous scenario ended
      if(length(times) == 0) {
        times <- t_start
        x@inc_start[[i]] <- FALSE
        x@inc_end[[i]] <- FALSE
      }
      if(any_before)
      {
        x@inc_start[[i]] <- FALSE
        if(times[[1]] != t_start) {
          times <- c(t_start, times)
        }
      }
      if(any_after && times[[length(times)]] != t_end) {
        times <- c(times, t_end)
        x@inc_end[[i]] <- FALSE
      }
      x@scenarios[[i]]@times <- times
    }

    if(.messages)
    {
      if(is_relevant[[i]])
        cli::cli_text(" Scenario #", i, ": simulated period [", min(times), ", ", max(times), "]")
      else
        cli::cli_text(" Scenario #", i, ": not simulated")
    }
  }

  x
}

# check validity of sequence elements
check_sequence <- function(seq) {
  if(!all(is_scenario(seq@scenarios))) {
    stop("sequence does not contain a list of effect scenarios")
  }
  # check if start and end of output times match between subsequent scenarios
  for(i in seq_along(seq)) {
    cur <- seq[[i]]
    if(is.null(cur)) {
      next
    }
    if(length(cur@times) == 0) {
      next
    }
    # skip further check for first element in sequence
    if(i == 1) {
      next
    }

    prev <- seq[[i-1]]
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

# Sets a scenario's slot values as attributes of a sequence.
#
# This emulates or mocks the existence of slots in the sequence
# object, at least for read-only access. Writing to these emulated slots will
# fail with an error.
#
# This is a workaround to make code work with sequence objects, which is not
# aware of sequences and cannot handle them, e.g. some model-specific `fx()`
# functions. Will not mock the contents of exposure and forcings slots, as these
# are too difficult to handle as the actual time-series may also depend on
# solver settings.
#
# @param sq [sequence] where slots are emulated
# @param scenario [scenario] to mock slots from
# @return modified [sequence]
mock_as_scenario <- function(sq, scenario=NULL) {
  if(!is_sequence(sq))
    stop("Argument `sq` must be a sequence")
  if(is.null(scenario))
    scenario <- sq[[1]]
  if(!is_scenario(scenario))
    stop("Argument `scenario` must be a scenario")

  # these slots/attributes will never be set/mocked
  protected <- slotNames(new("ScenarioSequence"))
  exclude <- c(protected, "times", "exposure", "forcings")

  nms <- slotNames(scenario)
  if(any(protected %in% nms)) {
    warning("Mocked scenario contains slots with protected names: ", paste(intersect(nms, protected), collapse=", "))
  }
  nms <- setdiff(nms, exclude)
  # mock slots by setting an attribute of the same name, this hack works because S4
  # slots are implemented by R using attributes.
  for(nm in nms) {
    attr(sq, nm) <- slot(scenario, nm)
  }
  # special handling for output times
  attr(sq, "times") <- get_times(sq)
  # A sequence's exposure and forcings should never ever be accessed by using slots.
  # Setting them to NA should make sure that any code accessing them fails.
  attr(sq, "exposure") <- NA
  attr(sq, "forcings") <- NA

  sq
}
