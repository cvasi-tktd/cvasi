#' Set forcings time-series
#'
#' *Forcings* generally refer to model parameters that change over time as part of an
#' external function such as a time-series of environmental temperature.
#'
#' Forcing time-series are always represented by a
#' `data.frame` containing two columns. The first column representing time,
#' the second representing the parameter that is a function of time. The
#' ordering of columns is mandatory. The column names are essentially irrelevant
#' but may help documenting the scenario and its data. The rows must be
#' ordered chronologically. A time-series can consist of only a single row; in
#' this case it will represent constant conditions.
#'
#' Handling forcing time-series is a costly task for the ODE solver due to consistency
#' checks and interpolation between timesteps. How the solver interpolates
#' the forcing time-series can be controlled by certain arguments to functions
#' such as [simulate()] and [effect()]. Please refer to [simulate()] for a brief
#' overview and [deSolve::forcings] for a detailed description.
#'
#' Forcing time-series should be kept as short as possible and as complex as
#' needed for optimal computational efficiency.
#'
#' @param x `EffectScenario` object or vector thereof
#' @param ... named argument list to set forcing series
#'
#' @return A modified `EffectScenario` objects
#' @export
#' @include class-EffectScenario.R
#' @rdname set_forcings
#' @examples
#' # constant values will be automatically converted to a data.frame
#' Lemna_Schmitt() %>% set_forcings(temp=20) -> lemna
#' lemna@forcings
#'
#' # setting multiple forcings at one
#' df <- data.frame(t=0:14,tmp=20+rnorm(15)) # random noisy series
#' Lemna_Schmitt() %>% set_forcings(temp=df, rad=15000) -> lemna
#' lemna@forcings
#'
#' # forcings can also be supplied as a named list
#' Lemna_Schmitt() %>% set_forcings(list(temp=20, rad=15000)) -> lemna
#' lemna@forcings
setGeneric("set_forcings", function(x,...) standardGeneric("set_forcings"), signature="x")

# For EffectScenario objects
#' @rdname set_forcings
setMethod("set_forcings", "EffectScenario", function(x, ...) set_forc_default(x, ...))
# For vectorized input/scenarios
#' @rdname set_forcings
setMethod("set_forcings", "list", function(x, ...) set_forc_list(x, ...))

#
# Set forcings for any kind of EffectScenario object
#
#' @importFrom rlang enquos eval_tidy
set_forc_default <- function(x, ...) {
  lst <- list()
  qqs <- rlang::enquos(...)
  nms <- names(qqs)
  # use standard or non-standard evaluation?
  # if 1st and only argument is a list but not a data.frame, then use std eval
  if(length(qqs)==1 &
     is.list(rlang::eval_tidy(qqs[[1]])) &
     !is.data.frame(rlang::eval_tidy(qqs[[1]]))) {
    lst <- rlang::eval_tidy(qqs[[1]])
    nms <- names(lst)
  } else { # otherwise look for any number of named arguments
    for(i in 1:length(qqs))
      lst <- append(lst, list(rlang::eval_tidy(qqs[[i]])))
    names(lst) <- nms
  }

  unused <- setdiff(nms, get_req_forcings(x))
  if(length(unused)>0)
    warning(paste("unused forcing series:",paste(unused,sep=",",collapse=",")))
  ints <- intersect(nms, get_req_forcings(x))
  if(length(lst)-length(unused)-length(ints)>0)
    warning("unnamed forcing series were ignored")

  for(nm in ints) {
    data <- lst[[nm]]
    if(is.numeric(data) & length(data) == 1) {
      data <- data.frame(t=c(0), V1=c(data))
      names(data) <- c("time", nm)
    }
    if(!is.data.frame(data))
      stop(paste("forcing series",nm,"has invalid type"))
    if(length(data)!=2)
      stop(paste("forcing series",nm,"must have two columns"))
    x@forcings[[nm]] <- data
  }
  x
}

set_forc_list <- function(x, ...) {
  return(lapply(x, function(sc) set_forcings(sc, ...)))
}
