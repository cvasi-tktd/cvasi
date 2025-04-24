# Dummy scenario class used for unit testing without the need to do actual
# time-consuming simulations
setClass("DummyScenario", contains=c("EffectScenario","Transferable"),
  slots=list(
    fx="ANY",
    simulate="ANY",
    solver="ANY"
  ),
  prototype=list(
    fx=NULL,
    simulate=NULL,
    solver=data.frame(time=0:10, A=1, B=2),

    name="DummyScenario",
    param=list(baz=42),
    param.req=c("baz"),
    param.bounds=list(baz=c(0, 1)),
    init=c(A=1, B=2),
    times=0:10,
    endpoints=c("foo"),
    exposure=ExposureSeries(data.frame(t=0, c=1)),
    control.req=FALSE
  )
)

# Mock results from [solver()]
setMethod("solver", "DummyScenario", function(scenario, ...) solver_dummy(scenario, ...))
solver_dummy <- function(scenario, ...) {
  x <- scenario@solver
  if(is.function(x))
    return(x(scenario, ...))
  if(is.data.frame(x))
    return(x)
  stop("invalid return type for dummy solver")
}

# Mock results from [simulate()]
setMethod("simulate", "DummyScenario", function(x, ...) simulate_dummy(scenario=x, ...))
simulate_dummy <- function(scenario, ...) {
  x <- scenario@simulate
  if(is.null(x))
    return(simulate_scenario(scenario, ...))
  if(is.function(x))
    return(x(scenario, ...))
  if(is.data.frame(x))
    return(x)
  stop("invalid return type for dummy simulation")
}

# Mock results from [fx()]
setMethod("fx", "DummyScenario", function(scenario, ...) fx_dummy(scenario, ...))
fx_dummy <- function(scenario, ...) {
  x <- scenario@fx
  if(is.function(x))
    return(x(scenario, ...))
  if(is.numeric(x))
    return(x)
  stop("invalid return type for dummy fx")
}
