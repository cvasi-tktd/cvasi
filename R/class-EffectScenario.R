#' Effect scenario classes
#'
#' The `EffectScenario` class is the base for all of the basic scenario
#' types and models. It contains slots for data and settings that are
#' required by most models such as a vector of model parameters and a vector
#' of initial states. For each particular model, the class's slots are
#' filled with certain default or fixed values. Some models derive from this
#' class and add slots to store additional data.
#'
#' Certain behaviors that are required to model complex processes cannot be
#' represented by a single EffectScenario. As an example, the parameters of a scenario
#' are generally fixed during the simulated time period. In order to represent a
#' change in parameter values, the original scenario would
#' need to split into two scenarios *A* and *B* which differ by parameter values
#' and simulated time period. By combining these scenarios to a *scenario sequence*,
#' the sequence would be treated as a single, complex scenario. See
#' [sequence()] for more information.
#'
#' ### Parameters
#' Most parameters are represented by numerical types but other types
#' are possible depending on model. Please refer to the model description
#' which parameters are required and in which unit. Some or all parameters may
#' be required to start a simulation. If required parameters are missing,
#' simulation will fail with an error message.
#'
#' ### Initial state
#' The *initial state* represents the starting values of state variables when
#' starting a simulation. A scenario's default initial state may be insufficient
#' to get sensible results. It is advisable to set an initial state explicitly
#' when creating a new scenario, see [set_init()].
#'
#' In theory, a scenario's state variables can be renamed by modifying the names
#' of the initial state vector. However, this is strongly
#' discouraged as this will affect other routines such as [effect()] and [epx()]
#' and may render results useless.
#'
#' ### Exposure
#' *Exposure* refers to the concentration of toxicant an organism is exposed to.
#' In case of aquatic organisms, this would commonly be the concentration of a
#' toxicant in water. Other interpretations are possible depending on model
#' assumptions.
#'
#' Exposure time-series are generally represented by a `data.frame` containing two
#' columns. The first column representing time, the second representing the
#' exposure level. The ordering of columns is mandatory. The column names
#' are essentially irrelevant but sensible names may help documenting the
#' scenario and its data. The rows must be ordered chronologically. A time-series
#' can consist of only a single row; in this case it will represent constant
#' exposure. Exposure time-series are set to a scenario using [set_exposure()].
#'
#' Handling time-series is a costly task for the ODE solver due to consistency
#' checks and interpolation between time steps. How the solver interpolates
#' the time-series can be controlled by certain arguments to functions
#' such as [simulate()] and [effect()]. Please refer to [simulate()] for a brief
#' overview and [deSolve::forcings] for a detailed description.
#'
#' Exposure time-series should be kept as short as possible and as complex as
#' needed for optimal computational efficiency.
#'
#' ### Environmental forcings
#' *Forcings* generally refer to model parameters that change over time as part
#' of an external function such as environmental temperature and exposure levels.
#' Due to the importance of exposure in regulatory assessments, this R package
#' explicitly distinguishes between environmental forcings and exposure. However,
#' the same restrictions and features apply to both of them.
#'
#' Forcing time-series are handled the same way as exposure time-series, i.e.
#' they are represented by a `data.frame` containing two columns. The first column
#' representing time, the second representing the parameter that is a function of
#' time. The ordering of columns is mandatory. The rows must be ordered
#' chronologically. Forcings time-series are set using [set_forcings()].
#' Please refer to the *Exposure* section for more information on how time-series
#' are handled.
#'
#' ### Output times
#' A scenario's simulated time period is defined by its minimum and maximum output
#' time. Simulation results will only be returned for the defined output times
#' even though the ODE solver may use smaller time steps between output times.
#' Output times can be explicitly set using [set_times()]. The number and
#' distance of output times may have influence on the precision of simulation
#' results and numerical stability, cf. [simulate()].
#'
#' Be aware that [set_exposure()] will overwrite previously defined output times
#' if not requested otherwise.
#'
#' ### Effects
#' Generally, all state variables can be used as effect endpoints but models may
#' provide additional endpoints. Use [set_endpoints()] to enable or disable
#' endpoints for a scenario.
#'
#' Some scenarios or models require control runs to calculate effects under
#' exposure. Generally, control simulations will run automatically
#' where needed. However, when conducting a large number of repeated simulations, e.g.
#' when calculating effect profiles (EPx values) or simulating moving exposure
#' windows, it may be computational efficient to run control simulations only
#' once and cache their results within the scenario. Please refer to [cache_controls()]
#' for details.
#'
#' ### Moving exposure windows
#' The time frame relevant for effects may be much shorter than the assessed
#' exposure time-series for certain organisms. This fact can be represented
#' by moving exposure windows which divide a long time period in a number of
#' consecutive windows of the same length. Each window is simulated individually and effects are
#' calculated. By default, methods such as [effect()] will only return the
#' maximum effect of all considered windows but detailed results can be presented
#' on demand.
#'
#' To use moving exposure windows, the exposure time-series must be regular,
#' i.e. must have an equidistant step length in time. The length of the window
#' is defined as the number of time steps of the exposure time-series. As an
#' example, assume the time-series has daily granularity and a moving window
#' of seven days length is required. In this case, the moving window must have
#' a length of seven (7) time steps. If the exposure time-series had hourly granularity,
#' the same window would need to have a length of 168 (=7*24) time steps. Please
#' refer to [set_window()] for details.
#'
#' @slot name `character`, unique model name
#' @slot tag `character`, an optional identifier
#' @slot param `list` of parameter key-value pairs
#' @slot param.req `character` vector of required parameters
#' @slot forcings `list` of `data.frame`s representing forcing time-series
#' @slot forcings.req `character` vector or required model forcings data, e.g. temperature
#' @slot init `list` of initial model states
#' @slot times `numeric` vector of output times, beginning and end also define
#'  the simulated period
#' @slot endpoints `character` vector of endpoints to calculate results for
#' @slot exposure `data.frame` with two columns representing an exposure time-series
#' @slot control `list` of named numerical vectors, contains the control values
#' for all relevant moving windows
#' @slot control.req `logical`, if `TRUE` then control values are required to
#'   calculate effects
#' @slot window.length `numeric`, maximum length of the simulated period, if
#'   `window.length` is shorter than the exposure pattern, then all possible
#'   exposure sub-patterns are evaluated for effect calculation. This is also
#'   referred to as a moving window approach.
#' @slot window.interval `numeric`, interval determining distance between moving
#'   windows during effect calculation. First window starts at first time point
#'   in exposure pattern.
#'
#' @name Scenarios
#' @aliases scenario scenarios EffectScenario-class
#' @family scenarios
NULL



#' @include class-ExposureSeries.R
#' @export
setClass("EffectScenario",
   slots = list(
     name="character",
     param="list",
     param.req="character",
     forcings="list",
     forcings.req="character",
     init="numeric",
     times="numeric",
     endpoints="character",
     exposure="ExposureSeries",
     control="ANY",
     control.req="logical",
     window.length="numeric",
     window.interval="numeric",
     tag="character"
   ),
   prototype = list(
     name="undefined",
     control.req=TRUE,
     window.length=-1,
     window.interval=-1,
     tag=NA_character_
   )
)
