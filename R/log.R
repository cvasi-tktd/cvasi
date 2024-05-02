
#' Start and stop logging
#'
#' @param file `character`, file name or path to a log file
#' @param append `logical`, if `TRUE` output will be appended to an existing log file,
#' otherwise the log file will be replaced
#' @param envir log will be automatically disabled if `environment` is exited,
#' set to `NULL` to disable
#' @return no return value
#' @export
log_enable <- function(file=NULL, append=TRUE, envir=parent.frame()) {
  if(is.character(file)) {
    #if(!is.null(envir))
    #  withr::defer(log_disable(), envir=envir)
    file = file(file, open=ifelse(append,"at","wt"))
    sink(file=file, type="output", append=append)
    options(cvasi.log.file=file)
  }

  options(cvasi.log=TRUE)
  log_msg("++ Log enabled: ", Sys.time())
  if(is.character(file)) {
    log_msg("++ Log file:    ", file)
  }
}

#' @export
#' @rdname log_enable
log_disable <- function() {
  log_msg("++ Log disabled: ",Sys.time())
  # close file connection and sink
  f <- getOption("cvasi.log.file",default=NULL)
  if(!is.null(f)) {
    sink(type="output")
    suppressWarnings(sink())
    try(close(f), silent=TRUE)
    options(cvasi.log.file=NULL)
  }

  options(cvasi.log=FALSE)
}

#' Add a log message
#'
#' Message will only appear in the console or in log file if logging was
#' enabled using `log_enable()`.
#'
#' @param ... elements will be concatenated using `paste0()`
#' @return no return value
#' @export
#' @examples
#' log_msg("this message will not appear")
#'
#' log_enable()
#' log_msg("this message will appear")
#' log_msg("a number of ","elements to ",42," concatenate")
log_msg <- function(...) {
  if(getOption("cvasi.log", default=FALSE)) {
    f <- getOption("cvasi.log.file",default=NULL)
    if(!is.null(f)) {
      write(paste0(...), stdout()) # write to log
    }
    message(paste0(...)) # write to console
  }
}

# Add a log message to console
#
# Output to log file is optional, output to console mandatory
log_msgf <- function(...) {
  if(getOption("cvasi.log", default=FALSE)) {
    f <- getOption("cvasi.log.file",default=NULL)
    if(!is.null(f)) {
      write(paste0(...), stdout()) # write to log
    }
  }
  message(paste0(...)) # write to console
}

#' Log R environment properties
#'
#' @return no return value
#' @export
#' @importFrom utils sessionInfo
log_envir <- function() {
  si <- sessionInfo()

  # imitates the behavior of print.sessionInfo() but looks nicer
  log_msg("\n++ Session info")
  log_msg(si$R.version$version.string)
  log_msg("Platform: ", si$platform)
  log_msg("Running under: ", si$running)

  log_msg("\nMatrix products: ", si$matprod)
  log_msg("Locale: ", gsub(";", ", ", si$locale))

  log_msg("\nAttached base packages:")
  log_msg(paste(si$basePkgs, sep="", collapse=", "))
  log_msg("\nOther attached packages:")
  log_msg(format_table(data.frame(name=sapply(si$otherPkgs, `[[`, "Package"), version=sapply(si$otherPkgs, `[[`, "Version"))))
  log_msg("\nLoaded via namespace (not attached):")
  log_msg(format_table(data.frame(name=sapply(si$loadedOnly, `[[`, "Package"), version=sapply(si$loadedOnly, `[[`, "Version"))))
}

# Logs the contents of a table
#
# Values are padded with white-spaces to the longest string in each column,
# the first column is left aligned, the remaining right-aligned, the table
# is sorted by the first column
#
#' @global .
format_table <- function(table) {
  table %>%
    dplyr::arrange(.[1]) %>%
    dplyr::mutate(dplyr::across(1, .fns=~ stringr::str_pad(.x, max(nchar(.x)), side="right", pad=" "))) %>%
    dplyr::mutate(dplyr::across(-1, .fns=~ stringr::str_pad(.x, max(nchar(.x)), side="left", pad=" "))) %>%
    tidyr::unite(col="V1", tidyr::everything(), sep=" ") %>%
    dplyr::pull(1) %>%
    paste(collapse="\n")
}

#' Log scenario properties
#'
#' @param x vector of `EffectScenario` objects
#' @param header `logical`, if `TRUE` a header line will be printed
#' @return unmodified argument `x`
#' @export
log_scenarios <- function(x, header=TRUE) {
  if(header)
    log_msg("\n++ Scenario overview")
  if(is.vector(x))
    sapply(x, log_scenarios, header=FALSE)
  else {
    show_scenario(x, inline=TRUE, show_exposure_series=FALSE)
    log_msg("")
  }
  x
}
