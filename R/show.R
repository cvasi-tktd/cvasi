show_scenario <- function(object, inline=FALSE, ...) {
  if(!inline)
    cli::cli_text("'{object@name}' scenario")

  if(object@tag != "" & !is.na(object@tag))
    cli::cli_text("tag: {object@tag}")

  if(length(object@param) > 0)
    cli::cli_text(cli::col_blue(paste0("param: ", paste(names(object@param), unlist(object@param), sep="=", collapse=", "))))
  else
    cli::cli_text(cli::col_blue("param: none"))

  if(length(object@init) > 0)
    cli::cli_text(cli::col_blue(paste("init :", paste(names(object@init), unlist(object@init), sep="=", collapse=", "))))
  else
    cli::cli_text(cli::col_blue("init : none"))

  if(length(object@endpoints) > 0)
    cli::cli_text(cli::col_blue(paste0("endpt: ", paste(object@endpoints, sep=",", collapse=", "))))
  else
    cli::cli_text(cli::col_blue("endpt: none"))

  if(length(object@times) > 0)
    cli::cli_text(cli::col_blue(paste0("times: [", min(object@times), ",", max(object@times), "] n=",
                         length(object@times), ", ", ifelse(is_regular_series(object@times), "regular", "irregular"))))
  else
    cli::cli_text(cli::col_blue("times: none"))

  cli::cli_text(cli::col_blue(paste("forcs:",ifelse(length(object@forcings) == 0, "none", paste0(names(object@forcings), collapse=", ")))))
  show_exposure(object@exposure, inline=TRUE, ...)
}

show_exposure <- function(x, inline=FALSE, show_exposure_series=TRUE) {
  color <- "green"
  nofile <- identical(x@file, character(0))
  if(!nofile) # the expression yields logical(0) for @file=character(0)
    nofile <- x@file %in% c("unknown","")

  if(!inline) {
    cli::cli_text("ExposureSeries object")
    cli::cli_text(cli::col_green("file: {ifelse(nofile, 'none', x@file)}"))
  } else {
    cli::cli_text(cli::col_green("expsr: {ifelse(nofile, '', x@file)}"))
  }

  if(nrow(x@series) > 0) {
    if(show_exposure_series) {
      print(x@series, max=20)
    }
  } else {
    cli::cli_text(cli::col_green(">> exposure series is empty"))
  }
}

#' @importFrom methods show
setMethod("show", "EffectScenario", function(object) show_scenario(object))
setMethod("show", "ExposureSeries", function(object) show_exposure(object))
