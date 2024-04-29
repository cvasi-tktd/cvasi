#' Pull metadata from scenarios
#'
#' The method pulls available metadata from scenario objects and returns a
#' table with additional columns. If the argument already was a `data.frame`
#' object, the columns are appended. May overwrite existing columns of the
#' same name.
#'
#' @param x vector of [scenarios] or a `data.frame` containing a column
#'   `scenario` with `EffectScenario` objects
#' @param model `logical`, if `TRUE` then model metadata is pulled
#' @param exposure  `logical`, if `TRUE` then exposure series metadata is pulled
#'
#' @return a `data.frame`
#' @export
#' @global scenario context
#'
#' @examples
#' metsulfuron %>%
#'   pull_metadata()
pull_metadata <- function(x, model=TRUE, exposure=TRUE) {
  if(is_scenario(x))
    x <- tibble::tibble(scenario=c(x))
  else if(is.vector(x))
    x <- tibble::tibble(scenario=x)
  if(!is.data.frame(x))
    stop("argument x has unsupported type", call. = FALSE)

  # model & scenario related metadata
  if(model) {
    x %>%
      dplyr::rowwise() %>%
      dplyr::mutate(ModelName=scenario@name,
                    ScenarioTag=scenario@tag,
                    Window_Length=scenario@window.length,
                    Window_Interval=scenario@window.interval) -> x
  }
  # exposure series related metadata
  if(exposure) {
    # pull metadata from TOXSWA series
    x %>%
      dplyr::rowwise() %>%
      dplyr::mutate(context=list(scenario@exposure@context)) %>%
      tidyr::unnest_wider(context, names_repair="minimal") -> x
  }

  dplyr::ungroup(x)
}
