#' Calls ODE solver for a particular model
#'
#' Please refer to the *Modeling Howto* vignette on how to implement custom
#' models by overloading the `solver` function.
#'
#' Some solvers may set reasonable default values for e.g. maximum step
#' length in time (`hmax`), but not all do. Please check the model documentation for
#' details.
#'
#' @param scenario [scenario] object
#' @param ... additional parameters passed on to [deSolve::ode()]
#' @param method string, numerical solver used by [deSolve::ode()]
#' @param hmax numeric, maximum step length in time, see [deSolve::ode()]
#' @return `data.frame` with simulation results
#' @export
setGeneric("solver",
           function(scenario, ...) standardGeneric("solver"),
           signature = "scenario"
)

# Default solver which uses the model's name to switch between solver calls
solver_default <- function(scenario, ...) {
  stop("cannot simulate unknown model type, solver missing")
}

#' @describeIn solver Default solver, raises an error
setMethod("solver", "ANY", solver_default)
