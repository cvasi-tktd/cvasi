#' @export
setClass("parameter_set",
         slots=list(
           model="character",
           tag="character",
           param="list"
         )
)

#' Set of model parameters
#' @param model `character`, a string containing a model name, e.g. `"GUTS-RED-IT"`
#' @param tag `character`, an optional identifier
#' @param param named `list` of model parameters
#' @slot model `character`, a string containing a model name, e.g. `"GUTS-RED-IT"`
#' @slot tag `character`, an optional identifier
#' @slot param named `list` of model parameters
#' @return an S4 object of type [parameter_set-class]
#'
#' @export
#' @aliases parameter_set-class
#' @examples
#' # create a parameter set and assign it
#' ps <- parameter_set("GUTS-RED-IT", list(kd=0.12, hb=0.3))
#' GUTS_RED_IT() %>% set_param(ps)
#'
#' # multiple scenarios can be modified at once
#' c(GUTS_RED_IT(), GUTS_RED_IT()) %>%
#'   set_param(ps)
#'
#' # model names must match, otherwise an error will be raised
#' try(GUTS_RED_SD() %>% set_param(ps))
parameter_set <- function(model, param=list(), tag=NA_character_) {
  if(!is.character(model))
    stop("model name must be of type character")
  if(!is.character(tag))
    stop("model tag must be of type character")
  if(length(dim(param)) > 0)
    stop("list of parameters must not be a matrix or data.frame")
  # convert vector to list
  if(is.vector(param) & !is.list(param)) {
    param <- as.list(param)
  }
  # check that list only contains atomic types, otherwise we might accept weird objects
  if(!all(sapply(param, is.atomic))) {
    stop("list of parameters must only  contain atomic types")
  }

  new("parameter_set",
      model=model,
      tag=tag,
      param=as.list(param)
  )
}

