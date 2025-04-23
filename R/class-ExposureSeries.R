#' @export
#' @aliases NoExposureSeries-class
setClass("ExposureSeries",
         slots=list(
           series="data.frame",
           dates="ANY",
           meta="list",
           context="list",
           file="character"
         )
)

#' @export
setClass("NoExposureSeries", contains="ExposureSeries")



#' Exposure time-series
#'
#' Creates an object that encapsulates an exposure time-series with its
#' metadata, such as formatted datetime strings and file name where the
#' series was loaded from. [no_exposure()] is shorthand to create a time-series
#' of constant zero exposure.
#'
#' @slot dates original time points of time-series, e.g. time stamps of the form `2000-01-01 12:00`
#' @slot file `character`, file name where data originates from, may be empty
#' @slot meta `list`, contains metadata
#' @slot context `list`, contains contextual metadata, such as project ids
#' @slot series `data.frame` containing the actual time-series
#' @param series `data.frame` with two columns containing a time-series
#' @param dates `vector`, optional original list of time stamps
#' @param file `character`, optional file name where data originates from
#' @param meta `list`, optional metadata
#' @param context `list` optional contextual metadata such as project ids
#' @return an S4 object of type [ExposureSeries-class]
#'
#' @seealso [no_exposure()]
#' @aliases ExposureSeries-class
#' @export
ExposureSeries <- function(series, dates, file, meta, context) {
  check_exposure(series)
  # coerce to data.frame to avoid issues with data.frame-like types
  series <- dplyr::select(as.data.frame(series), c(1, 2))

  if(missing(file))
    file <- "unknown"
  if(missing(meta))
    meta <- list()
  if(missing(context))
    context <- list()
  if(missing(dates))
    dates <- series[,1]

  new("ExposureSeries",
      series=series,
      dates=dates,
      file=file,
      meta=meta,
      context=context
  )
}


#' Zero exposure
#'
#' Creates an [ExposureSeries] with zero concentration. When setting
#' the zero exposure, pay attention not to accidentally reset the output times
#' of your scenario as the zero exposure series contains only a single time point.
#' See the examples.
#'
#' @return  an S4 object of type [ExposureSeries-class]
#' @seealso [set_noexposure()]
#' @export
#' @examples
#' # Set exposure to zero, but keep the original output times
#' minnow_it %>%
#'   set_noexposure() %>%
#'   simulate()
no_exposure <- function() {
  new("NoExposureSeries", series=data.frame(time=0, conc=0), file="no exposure")
}
