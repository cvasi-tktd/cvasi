#' @importFrom cli cat_line
show_scenario <- function(object, inline=FALSE, ...) {
  if(!inline)
    cli::cat_line("EffectScenario object")
  color <- "blue"

  tag <- ""
  if(object@tag != "" & !is.na(object@tag))
    tag <- paste0("(#", object@tag, ")")
  cli::cat_line(paste("model:",object@name,tag),col=color)

  if(length(object@param) > 0)
    cli::cat_line(paste("param:",paste(names(object@param),unlist(object@param),sep="=",collapse=", ")),col=color)
  else
    cli::cat_line("param: none",col=color)

  if(length(object@init) > 0)
    cli::cat_line(paste("init :",paste(names(object@init),unlist(object@init),sep="=",collapse=", ")),col=color)
  else
    cli::cat_line("init : none",col=color)

  if(length(object@endpoints)>0)
    cli::cat_line(paste0("endpt: ",paste(object@endpoints,sep=",",collapse=", ")),col=color)
  else
    cli::cat_line("endpt: none",col=color)

  if(length(object@times) > 0)
    cli::cat_line(paste0("times: [",min(object@times),",",max(object@times),"] n=",length(object@times),", ",ifelse(is_regular_series(object@times),"regular","irregular")),col=color)
  else
    cli::cat_line("times: none",col=color)

  cli::cat_line(paste("forcs:",ifelse(length(object@forcings)==0,"none",paste0(names(object@forcings),collapse=", "))),col=color)
  show_exposure(object@exposure, inline=TRUE, ...)
}

show_exposure <- function(x, inline=FALSE, show_exposure_series=TRUE) {
  color <- "green"
  nofile <- identical(x@file, character(0))
  if(!nofile) # the expression yields logical(0) for @file=character(0)
    nofile <- x@file %in% c("unknown","")

  if(!inline) {
    cli::cat_line("ExposureSeries object")
    cli::cat_line(paste("file:",ifelse(nofile,"none",x@file)),col=color)
  } else {
    cli::cat_line(paste("expsr:",ifelse(nofile,"none",x@file)),col=color)
  }

  if(nrow(x@series)>0) {
    if(show_exposure_series)
      print(x@series,max=20)
  } else {
    cli::cat_line(">> exposure series is empty",col=color)
  }
}

#' @importFrom methods show
setMethod("show", "EffectScenario", function(object) show_scenario(object))
setMethod("show", "ExposureSeries", function(object) show_exposure(object))
