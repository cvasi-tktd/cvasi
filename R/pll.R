#' Disable parallelization for debugging
#'
#' In certain cases it might be beneficial to disable parallel execution
#' of e.g. effect profile calculations. By disabling, all processes run
#' sequentially and instantly pass messages to the console which would be
#' delayed during parallel processing. This makes it easier to pinpoint
#' problems within the data or algorithm.
#'
#' @param state `logical`, if `TRUE` then parallelization is disabled
#' @return no return value
#' @export
pll_debug <- function(state=TRUE) {
  if(state)
    message("cvasi: parallelization is disabled")
  else
    message("cvasi: parallelization can be enabled")
  options(cvasi.pll.off=state)
}
