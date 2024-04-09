#' Set model parameters
#'
#' Modifies the parameters of one or more `EffectScenario` objects.
#'
#' Most parameters are represented by numerical types but other types
#' are possible depending on model. Please refer to the model description
#' which parameters are required and in which unit. Some or all parameters may
#' be required to start a simulation. If required parameters are missing,
#' simulation will fail with an error message.
#'
#' @param x object(s) to modify
#' @param param named numeric `vector` with parameter names and value OR a
#'   list of \linkS4class{parameter_set} objects
#' @return Vector of modified objects
#' @export
#' @include class-EffectScenario.R class-parameter_set.R
#' @examples
#' Lemna_Schmitt() %>% set_param(c(Emax=1,EC50=0.12))
setGeneric("set_param", function(x, param) standardGeneric("set_param"))

#' @rdname set_param
#' @export
setMethod("set_param", c("EffectScenario","vector"), function(x, param) {
  # Supported cases: 'param' is either
  # a) a named numerical vector of key/value pairs
  # b) a vector/list of parameter_set objects
  if(is.atomic(param) | all(sapply(param, is.atomic))) {
    # if required parameters are set, check if arguments conform
    if(length(x@param.req) > 0) {
      unused <- setdiff(names(param),x@param.req)
      if(length(unused)>0)
        warning(paste("unused parameters:",paste(unused,collapse=",")))

      ints <- intersect(x@param.req,names(param))
      x@param[ints] <- param[ints]
    } else { # otherwise, just use values as they are
      x@param[names(param)] <- param
    }
  }
  else if(all(is_parameter_set(param))) {
    x <- set_param_multi(list(x), param)[[1]]
  }
  else {
    stop("list of parameters contains inconsistent or invalid types")
  }
  x
})

# single scenarios, single parameter_set
#' @rdname set_param
#' @export
setMethod("set_param", c("EffectScenario","parameter_set"), function(x, param) {
  if(is_param_match(x, param)) {
    x <- set_param(x, param@param)
    return(x)
  }
  stop("model name & tag do not match parameter set")
})

# multiple scenarios, single parameter_set
#' @rdname set_param
#' @export
setMethod("set_param", c("list","parameter_set"), function(x, param) {
  sapply(x, set_param, param)
})

# multiple scenarios, EITHER single parameter set OR list of parameter_set objects
#' @rdname set_param
#' @export
setMethod("set_param", c("list","vector"), function(x, param) {
  # match scenarios to parameter sets?
  if(all(is_scenario(x)) & all(is_parameter_set(param)))
    return(set_param_multi(x, param))
  # default
  sapply(x, set_param, param)
})

# for a scenario sequence, apply parameters to each scenario in sequence
#' @rdname set_param
#' @export
setMethod("set_param", c("ScenarioSequence","vector"), function(x, param) {
  x@scenarios <- lapply(x@scenarios, set_param, param)
  x
})

# for a scenario sequence, apply set to each scenario in sequence
#' @rdname set_param
#' @export
setMethod("set_param", c("ScenarioSequence","parameter_set"), function(x, param) {
  x@scenarios <- lapply(x@scenarios, set_param, param)
  x
})

#' Match scenarios to parameter sets 1:1
#' @param scenarios `list` of `EffectScenario` objects
#' @param param `list` of `parameter_set` objects
#' @noRd
#' @global tag model name desc
set_param_multi <- function(scenarios, param) {
  df <- tibble::tibble(ps=param, model=get_model(param), tag=get_tag(param)) %>%
    dplyr::mutate(tag=ifelse(is.na(tag), "NA", tag))
  rs <- data.frame(model=character(0), tag=character(0), n=numeric(0))

  for(i in seq_along(scenarios)) {
    sc <- scenarios[[i]]
    # check if there's a unique parameter set available
    md <- get_model(sc)
    tg <- ifelse(is.na(get_tag(sc)), "NA", get_tag(sc))
    matches <- dplyr::filter(df, model==md & tag==tg)
    rs <- dplyr::bind_rows(rs, list(model=md, tag=tg, n=nrow(matches)))

    # only update scenario's parameters if it's a unique match
    if(nrow(matches) == 1)
      scenarios[[i]] <- set_param(sc, matches$ps[[1]])
  }
  # generate some overview of what happened
  log_msgf("+ Matching scenarios to multiple parameter sets")
  rs %>%
    dplyr::mutate(name=paste0(paste(model, tag, sep="#"), ": "),
                  desc=dplyr::case_when(n==0 ~ "no matches",
                                        n==1 ~ "unique match",
                                        n>1 ~ "ambiguous")) %>%
    dplyr::select(name, desc) %>%
    format_table() %>%
    log_msgf()
  if(any(rs$n > 1))
    stop("match of scenarios to parameter sets must be unambiguous")
  if(any(rs$n == 0))
    warning("could not match all scenarios to parameter sets")

  scenarios
}
