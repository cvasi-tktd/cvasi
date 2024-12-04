#' Generic to calculate effects for a particular scenario
#'
#' @param scenario [scenario] object
#' @param ... additional parameters
#' @return numeric named vector
#' @export
setGeneric("fx", function(scenario, ...) standardGeneric("fx"), signature = "scenario")

# Default effects on state variables at end of the simulated period
#' @describeIn fx Use state variables at end of simulation
setMethod("fx", "ANY", function(scenario, ...) fx_default(scenario, ...))

# Use value of state variable at end of simulation to derive effect
fx_default <- function(scenario, ...) {
  row <- tail_nm(simulate(scenario, ...))
  # setNames() is required if some endpoints are not present in the output
  # of simulate(). in this case the return value would contain columns named
  # `<NA>`, which we want to avoid.
  setNames(row[scenario@endpoints], scenario@endpoints)
}

# return the last row of a data.frame or matrix as a vector and assures that
# column names are retained
#' @importFrom utils head tail
tail_nm <- function(data) {
  if(is.data.frame(data))
    row <- unlist(tail(data,1))
  else if(is.matrix(data)) {
    row <- as.vector(tail(data,1))
    names(row) <- colnames(data)
  } else {
    stop("unknown type")
  }
  row
}
