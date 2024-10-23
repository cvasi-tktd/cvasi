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

# Handle special model types
#' @include class-Myriophyllum.R
#' @describeIn fx Effect at end of simulation of [Macrophyte-models]
setMethod("fx", "Myriophyllum", function(scenario, ...) fx_Lemna(scenario, ...))
#' @include class-Algae.R
#' @describeIn fx Effect at end of simulation of [Algae-models]
setMethod("fx", "Algae", function(scenario, ...) fx_Algae(scenario, ...))


# Use value of state variable at end of simulation to derive effect
fx_default <- function(scenario, ...) {
  row <- tail_nm(simulate(scenario, ...))
  endpoints <- intersect(names(scenario@init), scenario@endpoints) # use only state var endpoints

  row[endpoints]
}

# Calculate effect of Algae scenario
fx_Algae <- function(scenario, ...) {
  efx_r <- "r" %in% scenario@endpoints
  # TODO move to a validate_scenario function, this takes precious time on every effect() call
  if(efx_r & has_transfer(scenario))
    stop("endpoint r is incompatible with biomass transfers")

  out <- simulate(scenario, ...)

  efx <- c("A"=tail(out$A, 1))
  if(efx_r) # we skip the log() operation if we can
    efx["r"] <- log(tail(out$A,1) / out$A[1]) / (tail(out[,1],1) - out[1,1])

  efx
}
