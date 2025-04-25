#' Set boundaries of model parameters
#'
#' Modifies the boundaries of model parameters for one or more [scenario] or
#' [caliset] objects.
#'
#'
#' @param x vector of [scenario] or [caliset] objects
#' @param bounds named list of numerical vectors, where the first level lists the parameters
#' by name, and the second level lists the lower and upper boundary
#' @return [scenario] or [caliset] with modified parameter boundaries
#' @export
#' @include class-EffectScenario.R
#' @examples
#' metsulfuron %>%
#'    set_bounds(list(k_phot_max = c(0, 30),
#'                    k_resp = c(0, 10)))
#'
setGeneric("set_bounds", function(x, bounds) standardGeneric("set_bounds"))

#' @rdname set_bounds
#' @export
setMethod("set_bounds", c("EffectScenario","list"), function(x, bounds) {
  check_bounds(bounds)

  # if required parameters are set, check if arguments conform
  if(length(x@param.req) > 0) {
    unused <- setdiff(names(bounds), x@param.req)
    if(length(unused)>0)
      cli::cli_warn("unknown parameter{?s}, ignoring boundaries for '{unused}'")
    # limit bounds to known/required parameters
    ints <- intersect(x@param.req, names(bounds))
    bounds <- bounds[ints]
  }

  # convert lists to vectors
  for(nm in names(bounds)) {
    if(is.list(bounds[[nm]]))
      bounds[[nm]] <- unlist(bounds[[nm]])
  }

  x@param.bounds[names(bounds)] <- bounds
  x
})

# CalibrationSet, single parameter_set
#' @rdname set_bounds
#' @export
setMethod("set_bounds", c("CalibrationSet","list"), function(x, bounds) {
  check_bounds(bounds)
  x@scenario <- set_bounds(x@scenario, bounds)
  x
})

# multiple scenarios, single parameter_set
#' @rdname set_bounds
#' @export
setMethod("set_bounds", c("list","list"), function(x, bounds) {
  check_bounds(bounds)
  sapply(x, set_bounds, bounds)
})

#' @rdname set_bounds
#' @export
setMethod("set_bounds", c("ScenarioSequence","list"), function(x, bounds) {
  check_bounds(bounds)
  for(i in seq_along(x)) {
    x[[i]] <- set_bounds(x[[i]], bounds)
  }
  x
})

get_bounds <- function(x) {
  if(is.vector(x) & length(x) != 1)
    stop("Argument `x` must be of length one")
  if(is_sequence(x))
    return(get_bounds(x[[1]]))
  if(is_caliset(x))
    return(get_bounds(x@scenario))
  if(!is_scenario(x))
    stop("Argument `x` must be a scenario")

  x@param.bounds
}

check_bounds <- function(bounds) {
  ## check names
  if(!is.list(bounds))
    stop("Argument `bounds` must be a list")
  if(length(bounds) == 0)
    stop("Argument `bounds` is empty")
  nms <- names(bounds)
  if(is.null(nms))
    stop("Argument `bounds` must be a named list")
  if("" %in% nms)
    stop("Argument `bounds` contains unnamed elements")

  ## check values
  errs <- FALSE
  # check that each name has a numeric vector of length two
  for(nm in nms) {
    if(length(bounds[[nm]]) != 2) {
      cli::cli_warn(paste0("boundaries for '", nm, "' are not of length two"))
      errs <- TRUE
    }
    if(is.list(bounds[[nm]]))
      bounds[[nm]] <- unlist(bounds[[nm]])
    if(!is.numeric(bounds[[nm]])) {
      cli::cli_warn(paste0("boundaries for '", nm, "' must be numeric"))
      errs <- TRUE
    }
    if(any(is.na(bounds[[nm]]))) {
      cli::cli_warn(paste0("boundaries for '", nm, "' contain NA values"))
      errs <- TRUE
    }
    if(min(bounds[[nm]]) != bounds[[nm]][[1]]) {
      cli::cli_warn(paste0("boundaries for '", nm, "' have invalid order"))
      errs <- TRUE
    }
  }
  if(errs)
    stop("Boundaries contain invalid values")
}

