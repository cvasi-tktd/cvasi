#' @include class-ScenarioSequence.R
#' @export
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
