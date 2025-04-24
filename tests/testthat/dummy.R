# Dummy scenario class used for unit testing without the need to do actual
# time-consuming simulations
setClass("DummyScenario", contains=c("EffectScenario","Transferable"),
  slots=list(
    fx="ANY",
    simulate="ANY"
  ),
  prototype=list(
    fx=c(foo=0, bar=1),
    simulate=data.frame(time=0:10, A=1, B=2),

    name="DummyScenario",
    param=list(baz=42),
    param.req=c("baz"),
    param.bounds=list(baz=c(0, 1)),
    init=c(A=1, B=2),
    times=0:10,
    endpoints=c("foo", "bar"),
    exposure=ExposureSeries(data.frame(t=0, c=1)),
    control.req=FALSE
  )
)

# Simulation results will be the value of parameter 'simresult'
setMethod("solver", "DummyScenario", function(scenario, times, ...) solver_dummy(scenario, ...))

solver_dummy <- function(scenario, ...) {
  x <- scenario@simulate
  if(is.function(x))
    return(x(scenario, ...))
  if(is.data.frame(x))
    return(x)
  stop("invalid return type for dummy simulation")
}

# Simulation results will be the value of parameter 'simresult'
setMethod("fx", "DummyScenario", function(scenario, ...) fx_dummy(scenario, ...))

fx_dummy <- function(scenario, ...) {
  x <- scenario@fx
  if(is.function(x))
    return(x(scenario, ...))
  if(is.numeric(x))
    return(x)
  stop("invalid return type for dummy fx")
}
