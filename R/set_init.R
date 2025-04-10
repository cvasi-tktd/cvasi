#' Set initial state
#'
#' The *initial state* represents the starting values of a scenario's state
#' variables when starting a simulation. A scenario's default initial state
#' may be insufficient to get sensible results.
#'
#' In theory, a scenarios's state variables can be renamed by modifying the names
#' of the initial state vector. However, this is strongly
#' discouraged as this will affect other routines such as [effect()] and [epx()]
#' and may render results useless.
#'
#' @param x vector of `EffectScenario` objects
#' @param init named numeric vector
#'
#' @return modified `EffectScenario` objects
#' @export
#' @include class-EffectScenario.R
#' @rdname set_init
#' @examples
#' # Set initial biomass to 1.0
#' metsulfuron %>% set_init(c(BM=1.0)) %>% simulate()
setGeneric("set_init", function(x, init) standardGeneric("set_init"), signature = "x")

## TODO support non-standard evaluation?
#' @rdname set_init
setMethod("set_init","EffectScenario", function(x, init) {
  if(missing(init))
    stop("Argument `init` is missing")

  if(length(init) == 0) {
    return(x)
  }
  if(is.list(init)) {
    init <- unlist(init)
  }

  # if state vars were defined before, check if these conform to the provided values
  if(length(get_vars(x)) > 0)
  {
    nms <- names(init)
    unused <- setdiff(nms, get_vars(x))
    if("" %in% nms | is.null(nms))
      stop("Argument `init` contains unnamed elements.")
    if(length(unused) > 0)
      warning(paste("Argument `init` contains names which are not state variables:", paste(unused, collapse=", ")))
    ints <- intersect(nms, get_vars(x))
    init <- init[ints]

    if(any(is.na(init) | is.nan(init) | is.infinite(init)))
      stop("Argument `init` contains invalid values such as NA, NaN, or Inf.")
    if(!is.numeric(init))
      stop("Argument `init`: initial states must be numeric.")
    x@init[ints] <- init[ints]
  }  # otherwise, just use values as they are
  else {
    x@init[names(init)] <- init
  }
  x
})

# set initial condition of scenario sequences
#' @rdname set_init
setMethod("set_init", "ScenarioSequence", function(x, init) {
  for(i in seq_along(x@scenarios)) {
    x@scenarios[[i]] <- set_init(x@scenarios[[i]], init)
  }
  x
})

# convenience function for lists of scenarios
#' @rdname set_init
setMethod("set_init", "vector", function(x, init) {
  sapply(x, set_init, init)
})
