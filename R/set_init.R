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
setGeneric("set_init",
           function(x,init) standardGeneric("set_init"),
           signature = "x"
)

#' @rdname set_init
setMethod("set_init", "vector", function(x, init) {
  sapply(x, set_init, init)
})

## TODO support non-standard evaluation
#' @rdname set_init
setMethod("set_init","EffectScenario", function(x, init) {
  if(missing(init))
    stop("init argument missing")

  if(length(init) == 0)
    return(x)
  if(is.list(init))
    init <- unlist(init)
  if(!is.numeric(init))
    stop("init values must be numeric vector")

  # if state vars were defined before, check if these conform to the provided values
  if(length(get_vars(x)) > 0) {
    nms <- names(init)
    unused <- setdiff(nms, get_vars(x))
    if("" %in% nms | is.null(nms))
      warning("init vector contains unnamed values")
    if(length(unused)>0)
      warning(paste("unused init variables:",paste(unused,collapse=", ")))
    ints <- intersect(nms, get_vars(x))
    x@init[ints] <- init[ints]
  } else { # otherwise, just use values as they are
    x@init[names(init)] <- init
  }
  x
})

# todo set_init for scenario sequences?
