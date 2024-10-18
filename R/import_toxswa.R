
#' SWASH project exposure profile import
#'
#' Read all TOXSWA files within a SWASH project directory.
#'
#' @param swash_dir path to the SWASH project directory
#' @param ... arguments passed on to [import_toxswa()]
#'
#' @return a list of imported exposure series, see [import_toxswa()] for details
#' @export
import_swash <- function(swash_dir, ...){
  toxwa_file_regex <- "^\\d*\\.out$"
  f <- list.files(swash_dir,
                  pattern = toxwa_file_regex,
                  full.names = TRUE)
  import_toxswa(f, ...)
}

#' Import *TOXSWA* exposure series
#'
#' Read one or more *TOXSWA* exposure series from *TOXSWA*'s `.out` files. By default, the
#' concentration dissolved in water (*ConLiqWatLay*) at the end of the simulated waterbody
#' (i.e. at the maximum of the *x* dimension) is returned. The unit of the time
#' scale as well as of the imported model output variable can be scaled as needed.
#'
#' The numerical time scale is shifted to always start at time zero (0.0).
#' Numerical columns of the returned *data.frame* objects will be of type
#' [units::units]. Please be aware that the use of `units` objects may not be
#' supported by all functions in this package. However, [set_times()] and
#' [set_exposure()] can handle `units` objects safely.
#'
#' Incomplete list of alternative *TOXSWA* v5.5.3 output variables:
#' - *ConLiqWatLay*: Concentration dissolved in water (g/m3)
#' - *ConLiqSed*: Concentration in pore water sediment (g/m3)
#' - *ConSysWatLay*: Total concentration in water (g/m3)
#' - *CntSorSusSol*: Content sorbed to suspended solids (g/kg)
#' - *CntSorSed*: Content sorbed to sediment (g/kg)
#'
#' @param filepath vector of strings with absolute or relative paths to files
#' @param output_var character, single output variable from *TOXSWA* that is
#'   imported, defaults to *ConLiqWatLay*
#' @param output_unit character, target unit of the imported output variable,
#'   defaults to *ug/L*, syntax must be compatible with [units::units()]
#' @param time_unit character, target unit of the imported time scale,
#'   defaults to *days*, syntax must be compatible with [units::units()]
#' @param substance optional vector of characters, if set, only the substance
#'   codes defined in this vector are imported
#' @param split logical, if `TRUE` then one series will be returned for each
#'   substance found in the *TOXSWA* files, else all substances per file will
#'   be in one *data.frame*. Defaults to `FALSE`
#' @export
#' @autoglobal
#' @return list of *data.frame* objects with exposure series. Each *data.frame* has at
#'   least three columns:
#'   - `time`: numerical time scale, always starts at zero
#'   - `timestamp`: time as datetime objects such as `POSIXct`
#'   - one or more additional columns for each imported substance
#'
import_toxswa <- function(filepath, output_var="ConLiqWatLay", output_unit="ug/L",
                           time_unit="days", substance=NULL, split=FALSE) {
  if(length(filepath) == 0) {
    stop("filepath vector is empty")
  }
  if(length(output_var) != 1)
    stop("Exactly one output variable must be selected")
  # vectorized file list
  if(length(filepath) > 1) {
    return(sapply(filepath, import_toxswa, output_var=output_var, output_unit=output_unit,
                  time_unit=time_unit, substance=substance, split=split, USE.NAMES=FALSE))
  }

  if(any(is.na(filepath)) | !is.character(filepath))
    stop("filepath vector contains invalid values")
  if(!file.exists(filepath))
    stop("file does not exists: ", filepath)

  headers <- paste0("V", seq(1, max(utils::count.fields(filepath, sep=""))))
  df <- utils::read.table(filepath, header=FALSE, col.names=headers, fill=TRUE)

  comments <- df %>%
    dplyr::filter(V1=="*") %>%
    tidyr::unite(col="V1", dplyr::everything(), sep=" ", na.rm=TRUE) %>%
    dplyr::mutate(V1=trimws(V1)) %>%
    dplyr::pull(V1)
  data <- df %>%
    dplyr::filter(V1 != "*")
  rm(df, headers)

  # check toxswa version
  version_string <- comments[stringr::str_detect(comments, "TOXSWA version")]
  if(length(version_string) == 0)
    stop("TOXSWA version missing")
  if(!stringr::str_detect(version_string, " (4|5\\.5\\.3)$"))
    stop("TOXSWA version not supported")

  # get unit of chosen output var
  unit_str <- comments[stringr::str_detect(comments, paste0("^\\* Unit for ", output_var, " is "))]
  if(length(unit_str) == 0)
    stop("Unit of output var missing")
  if(length(unit_str) > 1)
    stop("Output variable is not unique")
  unit_str <- stringr::str_extract(unit_str, "\\((.*?)\\)", group=1)
  if(is.na(unit_str) | unit_str == "")
    stop("Unit of output var missing")
  # replace any '.' by a space to avoid issues with `units` package
  unit_str <- stringr::str_replace(unit_str, "\\.", " ")

  # is output var included in timerseries?
  data <- data %>%
    dplyr::filter(stringr::str_detect(tolower(V3), pattern=paste0("^", tolower(output_var), "(_\\w+)?$"))) %>%
    dplyr::select(dplyr::where(~ sum(!is.na(.x)) > 0)) %>% # remove any columns containing only NA
    dplyr::select(dplyr::where(~ sum(.x != "") > 0)) # remove any columns containing only emtpy strings
  if(nrow(data) == 0)
    stop("No data for output variable in TOXSWA file")

  data <- data %>%
    dplyr::select(c(dplyr::any_of(c(1, 2, 3)), dplyr::last_col())) %>%
    dplyr::rename(time=1, timestamp=2, name=3, value=4) %>%
    dplyr::mutate(value=units::set_units(as.numeric(value), unit_str, mode="standard"))

  # convert units of output if requested
  if(!is.null(output_unit)) {
    units(data$value) <- units::as_units(output_unit)
  }

  # check if variable is substance-dependent (which it should be), and
  # pivot by substance
  if(any(stringr::str_detect(data$name, "_")))
  {
    data <- data %>%
      dplyr::mutate(name=stringr::str_extract(name, "_(.+)$", group=1)) %>%
      tidyr::pivot_wider()
  }
  else if(!is.null(substance)) {
    stop("Output var is not substance dependent, check inputs")
  }

  # is chosen substance part of result?
  if(!is.null(substance)) {
    if(!all(substance %in% names(data)))
      stop("Selected substance code not found in file")

    data <- data %>% dplyr::select(c(1, 2, !!substance))
  }

  data <- data %>%
    dplyr::mutate(time=as.numeric(time),
                  time=time - time[[1]],
                  time=units::set_units(time, "days"),
                  time=units::set_units(time, time_unit, mode="standard"),
                  timestamp=lubridate::dmy_hm(timestamp))
  # set unit information as attribute for backwards compat
  myunits <- c("time"=units::deparse_unit(data$time),
               "conc"=units::deparse_unit(as.data.frame(data)[,3]))
  attr(data, "units") <- myunits

  fn <- tools::file_path_sans_ext(basename(filepath))
  result <- list()
  # split series by substance?
  if(split) {
    nms <- names(data)
    for(i in seq(3, length(data))) {
      key <- paste0(fn, "_", nms[[i]])
      result[[key]] <- dplyr::select(data, dplyr::all_of(c(1, 2, i)))
    }
  } else {
    result[[fn]] <- data
  }
  result
}
