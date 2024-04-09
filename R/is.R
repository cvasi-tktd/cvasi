# checks if all arguments are of type parameter_set
#' @importFrom methods is
is_parameter_set <- function(x) {
  if(is.list(x))
    return(sapply(x, is_parameter_set))
  is(x, "parameter_set")
}

# checks if a parameter_set objects matches a certain scenario
is_param_match <- function(scenario, param, ignore_tags=FALSE) {
  if(is.vector(scenario))
    stop("scenario vectors not supported")
  if(!is_scenario(scenario))
    stop("scenario must be of type EffectScenario")
  if(is.vector(param))
    return(sapply(param, function(ps) is_param_match(scenario, ps)))
  if(!is_parameter_set(param))
    stop("param must be of type parameter_set")

  # if both tags are NA, then match by name only
  if(ignore_tags | all(is.na(get_tag(c(scenario, param)))))
    return(get_model(scenario) == get_model(param))
  # if one tag is NA, then no match possible
  if(any(is.na(get_tag(c(scenario, param)))))
    return(FALSE)
  # otherwise match by name and tag
  get_model(scenario) == get_model(param) & get_tag(scenario) == get_tag(param)
}

is_scenario <- function(x) {
  if(is.list(x))
    return(sapply(x, is_scenario))
  is(x, "EffectScenario")
}

is_exp_series <- function(x) {
  if(is.list(x))
    return(sapply(x, is_exp_series))
  is(x, "ExposureSeries")
}

#' Test if scenario/model is a GUTS model
#'
#' @param x vector of `EffectScenario` objects
#'
#' @return vector of logical values
#' @export
#' @examples
#' is_GUTS(GUTS_RED_IT()) # returns TRUE
#' is_GUTS(rep(c(GUTS_RED_IT()),3)) # returns c(TRUE,TRUE,TRUE)
is_GUTS <- function(x) {
  if(is.list(x))
    return(sapply(x, is_GUTS))
  if(!is_scenario(x))
    return(FALSE)

  startsWith(get_model_name(x),"GUTS-")
}

#' Test if scenario/model is an Internal Threshold (IT) type
#'
#' @param x vector of `EffectScenario` objects
#'
#' @return vector of logical values
#' @export
#' @examples
#' is_GUTS_IT(GUTS_RED_IT())
#' # returns TRUE
is_GUTS_IT <- function(x) {
  if(is.list(x))
    return(sapply(x, is_GUTS_IT))
  if(!is_scenario(x))
    return(FALSE)

  name <- get_model_name(x)
  startsWith(name,"GUTS-") & endsWith(name,"-IT")
}

#' Test if scenario/model is a Stochastic Death (SD) type
#'
#' @param x vector of `EffectScenario` objects
#'
#' @return vector of logical values
#' @export
#' @examples
#' is_GUTS_SD(GUTS_RED_SD())
#' # returns TRUE
is_GUTS_SD <- function(x) {
  if(is.list(x))
    return(sapply(x, is_GUTS_SD))
  if(!is_scenario(x))
    return(FALSE)

  name <- get_model_name(x)
  startsWith(name,"GUTS-") & endsWith(name,"-SD")
}


#' Test if scenario/model is a DEB type
#'
#' @param x vector of `EffectScenario` objects
#' @return vector of logical values
#' @export
is_DEB <- function(x) {
  if(is.list(x))
    return(sapply(x, is_DEB))
  if(!is_scenario(x))
    return(FALSE)

  startsWith(get_model_name(x), "DEB")
}

#' Test if scenario/model is a Lemna model
#'
#' Also returns `TRUE` for `LemnaThreshold` models
#'
#' @param x vector of \linkS4class{EffectScenario} objects
#' @return vector of logical values
#' @export
is_Lemna <- function(x) {
  if(is.list(x))
    return(sapply(x, is_Lemna))
  if(!is_scenario(x))
    return(FALSE)

  is(x, "Lemna")
}

#' Test if scenario/model is a LemnaThreshold model
#'
#' @param x vector of \linkS4class{EffectScenario} objects
#' @return vector of logical values
#' @export
is_LemnaThreshold <- function(x) {
  if(is.list(x))
    return(sapply(x, is_LemnaThreshold))
  if(!is_scenario(x))
    return(FALSE)

  get_model_name(x) == "Lemna_SchmittThold"
}

# Check if an exposure series is complete, i.e. regular
#
is_regular_series <- function(x) {
  if(is(x,"ExposureSeries")) {
    if("regular.series" %in% names(x@meta))
      return(x@meta$regular.series)
    return(FALSE)
  }
  else if(is.vector(x) & is.numeric(x) & length(x) > 0) {
    x_diff <- unique(diff(x))
    if(length(x_diff) <= 1)
      return(TRUE)

    # disregard numerical issuesm i.e. very small deviations from a single value
    x_min <- min(x_diff[which(x_diff > 0)])
    return(all(1 - abs(x_diff/x_min) < 1e-10))
  }
  stop("type not supported")
}

# Check if scenarios requires a control for effect calculation
#
is_control_required <- function(x) {
  if(is.list(x))
    return(sapply(x, is_control_required))
  if(!is_scenario(x))
    return(FALSE)

  x@control.req
}

# Check if an R package is locally available
#
# The check is case-sensitive
# @param name `character`, name of an R package, e.g. 'dplyr'
# @return `logical`, `TRUE` if package is installed, else `FALSE`
#' @importFrom utils installed.packages
is_pckg_avail <- function(name) {
  tolower(name) %in% tolower(rownames(installed.packages()))
}

# Check if an exposure series represents zero/no exposure
#
# @param x `EffectScenario` or `ExposureSeries` object
# @return `logical`, `TRUE` if no/zero exposure
is_no_exposure <- function(x) {
  if(length(x) > 1)
    return(sapply(x, is_no_exposure))

  if(is.vector(x))
    x <- x[[1]] # unbox
  if(is(x, "EffectScenario"))
    x <- x@exposure
  if(!is(x, "ExposureSeries"))
    return(FALSE)
  if(nrow(x@series) == 0) {
    return(TRUE)
  } else if(nrow(x@series) == 1) {
    if(x@series[[1,2]] == 0)
      return(TRUE)
  } else {
    if(all(x@series[,2] == 0))
      return(TRUE)
  }
  return(FALSE)
}
