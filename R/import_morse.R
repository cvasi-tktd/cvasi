#' Import `morse` model parameters
#'
#' Loads GUTS model parameters which were fitted by the morse package.
#'
#' @param fit Either a string with a file path to *.Rdata* or *.RDS* file
#'   containing a *morse* fit, or a *morse* fit object
#' @param find_sd a logical value. If `TRUE`, it will try to find fitted parameters of the GUTS-RED-SD model
#' @param find_it  a logical value. If `TRUE`, it will try to find fitted parameters of the GUTS-RED-IT model
#' @param reset_hb a logical value. If `TRUE`, the background hazard rate `hb` is set to zero
#' @param params `character`, if `"estim"` then only the best-fit parameter sets are
#'  imported, else all parameter sets in the MCM chains are returned
#' @param mcmc_size optional `integer`, sets the maximum number of imported parameter sets per MCMC.
#'   By default, all MSMS parameter samples are imported.
#' @param file *deprecated*`,` alias for parameter `fit`
#' @param find.SD *deprecated*`,` alias for parameter `find_sd`
#' @param find.IT *deprecated*`,` alias for parameter `find_it`
#' @param reset.hb *deprecated*`,` alias for parameter `rest_hb`
#' @param mcmc.size *deprecated*`,` alias for parameter `mcmc_size`
#'
#' @return list of [parameter_set] objects
#' @importFrom lifecycle is_present deprecated
#' @export
#'
#' @examples
#' # import all parameter fits
#' try(import_morse("path/to/morse_fit.RData"))
#'
#' # import parameters for a specific model
#' try(import_morse("path/to/morse_fit.RData", find.IT=TRUE, find.SD=FALSE))
#'
#' # modify model objects
#' try(models %>% set_param(import_morse("path/to/morse_fit.RData")))
import_morse <- function(fit, find_sd=TRUE, find_it=TRUE, reset_hb=TRUE,
                         params=c("estim", "all"), mcmc_size, find.SD=deprecated(),
                         find.IT=deprecated(), reset.hb=deprecated(), mcmc.size=deprecated(),
                         file=deprecated()) {
  if(is_present(file)) {
    lifecycle::deprecate_soft("1.3.0", "import_morse(file)", "import_morse(fit)")
    fit <- file
  }
  if(is_present(find.SD)) {
    lifecycle::deprecate_soft("1.3.0", "import_morse(find.SD)", "import_morse(find_sd)")
    find_sd <- find.SD
  }
  if(is_present(find.IT)) {
    lifecycle::deprecate_soft("1.3.0", "import_morse(find.IT)", "import_morse(find_it)")
    find_it <- find.IT
  }
  if(is_present(reset.hb)) {
    lifecycle::deprecate_soft("1.3.0", "import_morse(reset.hb)", "import_morse(reset_hb)")
    reset_hb <- reset.hb
  }
  if(is_present(mcmc.size)) {
    lifecycle::deprecate_soft("1.3.0", "import_morse(mcmc.size)", "import_morse(mcmc_size)")
    mcmc_size <- mcmc.size
  }

  if(missing(mcmc_size))
    mcmc_size <- -1 # load all by default
  params <- match.arg(params)

  fits <- list()
  # is fit a path to a data file?
  if(is.character(fit))
  {
    log_msg("\n++ Loading parameters from morse object file")
    log_msg("file: ", fit)
    if(!file.exists(fit))
      stop("morse object file does not exist")

    if(endsWith(tolower(fit), ".rdata")) {
      ex <- new.env()
      suppressWarnings(load(fit, envir=ex))
      fit <- ex
    } else if(endsWith(tolower(fit), ".rds")) {
      fit <- list(survfit=suppressWarnings(readRDS(fit)))
    }
  }
  else if(is(fit, "survFit")) {
    fit <- list(survfit=fit)
  } else {
    stop("unsupported type: ", is(fit))
  }

  # try to find fitted parameters automatically
  if(find_sd | find_it) {
    for(nm in names(fit)) {
      var <- fit[[nm]]
      if(is(var, "survFit")) {
        if(find_it & "alpha" %in% var$estim.par$parameters)
          fits[["GUTS-RED-IT"]] <- nm
        if(find_sd & "kk" %in% var$estim.par$parameters)
          fits[["GUTS-RED-SD"]] <- nm
      }
    }

    if(find_it & !("GUTS-RED-IT" %in% names(fits))) {
      warning("Could not find GUTS-RED-IT fitted parameters")
    }
    if(find_sd & !("GUTS-RED-SD" %in% names(fits))) {
      warning("Could not find GUTS-RED-SD fitted parameters")
    }
    if(length(fits) == 0)
      stop("Aborting import")
  }

  # extract parameter sets
  ps <- c()
  for(nm in names(fits)) {
    morse_fit <- fit[[fits[[nm]]]]

    # only load best fit parameters
    if(params == "estim") {
      p <- morse_fit$estim.par$median
      names(p) <- morse_fit$estim.par$parameters
      if(reset_hb) {
        p["hb"] <- 0 # reset background hazard rate to zero
      }

      log_msgf(nm, ": best-fit imported")
      log_msg("  ",paste(names(p), unlist(p), sep="=", collapse=", "))
      ps <- c(ps, parameter_set(model=nm, param=as.list(p)))
    }
    # load all MCMC shots
    else {
      # import all
      if(mcmc_size <= 0)
        do.call("rbind", morse_fit$mcmc) -> morse_all
      else { # select a certain number of rows from each MCM chain
        morse_all <- data.frame()
        for(i in seq(length(morse_fit$mcmc)))
          morse_all <- rbind(morse_all, morse_fit$mcmc[[i]][seq(mcmc_size), ])
      }

      # rename and convert parameter values
      morse_all[, c(paste0(morse_fit$estim.par$parameters, "_log10"))] %>%
        tibble::as_tibble() %>%
        dplyr::mutate_all(function(x) 10^x) %>%
        dplyr::rename_with(.fn=function(x) sub("_log10", "", x)) -> morse_all
      if(reset_hb) {
        morse_all$hb <- c(0)
      }

      log_msgf(paste0(nm, ": MCMC samples imported (n=", nrow(morse_all), ")"))
      for(i in seq(nrow(morse_all))) {
        ps <- c(ps, parameter_set(model=nm, param=as.list(morse_all[i, ])))
        #log_msg("  ",paste(names(morse.all),unlist(morse.all[i,]),sep="=",collapse=", "))
      }
    }
  }
  return(ps)
}

# for backwards compatibility
morse <- function(...) {
  lifecycle::deprecate_soft("1.3.0", "morse()", "import_morse()")
  import_morse(...)
}
