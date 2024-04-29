#' Exposure time-series
#'
#' The `ExposureSeries` class encapsulates an exposure time-series with its
#' metadata, such as formatted datetime strings and file name where the
#' series was loaded from. The constructor `ExposureSeries()` can be used to
#' ease object creation. `no_exposure()` is shorthand to create a time-series
#' of constant zero exposure.
#'
#' @slot dates original time points of time-series, e.g. time stamps of the form `2000-01-01 12:00`
#' @slot file `character`, file name where data originates from, may be empty
#' @slot meta `list`, contains metadata
#' @slot context `list`, contains contextual metadata, such as project ids
#' @slot series `data.frame` containing the actual time-series
#' @rdname ExposureSeries
#' @export
setClass("ExposureSeries",
         slots=list(
           series="data.frame",
           dates="ANY",
           meta="list",
           context="list",
           file="character"
         )
)



#' ExposureSeries constructor
#'
#' Eases the creation of \linkS4class{ExposureSeries} objects. Can be used
#' to initialize the object's slots.
#'
#' @param series `data.frame` with two columns containing a time-series
#' @param dates `vector`, optional original list of time stamps
#' @param file `character`, optional file name where data originates from
#' @param meta `list`, optional metadata
#' @param context `list` optional contextual metadata such as project ids
#'
#' @rdname ExposureSeries
#' @export
ExposureSeries <- function(series, dates, file, meta, context) {
  if(!is.data.frame(series))
    stop("time-series is not a data.frame")
  # coerce to data.frame to avoid issues with data.frame-like types
  series <- as.data.frame(series)
  if(ncol(series)!=2)
    stop("time-series must have two columns")
  if(any(!is.numeric(series[,1])) | any(!is.numeric(series[,2])))
    stop("time-series contains non-numeric entries")
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
#' Creates an \linkS4class{ExposureSeries} with zero concentration. When setting
#' the zero exposure, pay attention not to accidentally reset the output times
#' of your scenario as the zero exposure series contains only a single time point.
#' See the examples.
#' @seealso [set_noexposure()]
#' @export
#' @examples
#' # this will reset the output times of the sample scenario,
#' # simulate() will quit with an error
#' \dontrun{
#' minnow_it %>%
#'   set_exposure(no_exposure()) %>%
#'   simulate()}
#'
#' # set zero exposure, but keep original output times
#' minnow_it %>%
#'   set_exposure(no_exposure(), reset_times=FALSE) %>%
#'   simulate()
no_exposure <- function() {
  ExposureSeries(data.frame(time=0, conc=0), file="no exposure")
}
