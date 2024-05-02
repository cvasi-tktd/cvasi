#' Import `morse` model parameters
#'
#' Loads GUTS model parameters which were fitted by the morse package.
#'
#' @param file Path to .RData file
#' @param find.SD a logical value. If `TRUE`, it will try to find fitted parameters of the GUTS_SD model
#' @param find.IT a logical value. If `TRUE`, it will try to find fitted parameters of the GUTS_IT model
#' @param reset.hb a logical value. If `TRUE`, the background hazard rate `hb` is set to zero
#' @param params `character`, if `"estim"` is selected then only the fitted parameter set for each model
#' is returned, else for `"auto"` all parameter sets in the MCM chains are returned
#' @param mcmc.size optional `integer`, sets the maximum number of imported parameter sets per MCMC,
#' all parameters are imported by default
#'
#' @return vector of `parameter_set` objects
#' @export
#'
#' @examples
#' # import all parameter fits
#' try(morse("path/to/morse_fit.RData"))
#'
#' # import parameters for a specific model
#' try(morse("path/to/morse_fit.RData", find.IT=TRUE, find.SD=FALSE))
#'
#' # modify model objects
#' try(models %>% set_param(morse("path/to/morse_fit.RData")))
morse <- function(file, find.SD=TRUE, find.IT=TRUE, reset.hb=TRUE, params=c("estim","all"), mcmc.size) {
  if(missing(mcmc.size))
    mcmc.size <- -1 # load all by default
  params <- match.arg(params)
  if(!file.exists(file))
    stop("morse file does not exist")
  log_msg("\n++ Loading parameters from morse")
  log_msg("file: ",file)

  fits <- list()
  ex<-new.env()
  suppressWarnings(load(file, envir=ex))
  # try to find fitted parameters automatically
  if(find.SD | find.IT) {
    for(nm in names(ex)) {
      var <- get(nm, envir=ex)
      if(is(var,"survFit")) {
        if(find.IT & "alpha" %in% var$estim.par$parameters)
          fits[["GUTS-RED-IT"]] <- nm
        if(find.SD & "kk" %in% var$estim.par$parameters)
          fits[["GUTS-RED-SD"]] <- nm
      }
    }
    if(length(fits)<sum(c(find.SD,find.IT)))
      stop("autofind can not identify all parameters")
  }

  # extract parameter sets
  ps <- c()
  for(nm in names(fits)) {
    morse.fit <- get(fits[[nm]], ex)

    # only load best fit parameters
    if(params == "estim") {
      p <- morse.fit$estim.par$median
      names(p) <- morse.fit$estim.par$parameters
      if(reset.hb)
        p["hb"] <- 0 # reset background hazard rate to zero

      log_msg(nm)
      log_msg("  ",paste(names(p),unlist(p),sep="=",collapse=", "))
      ps <- c(ps, parameter_set(model=nm,param=as.list(p)))
    }
    # load all MCMC shots
    else {
      # import all
      if(mcmc.size<=0)
        do.call("rbind", morse.fit$mcmc) -> morse.all
      else { # select a certain number of rows from each MCM chain
        morse.all <- data.frame()
        for(i in seq(length(morse.fit$mcmc)))
          morse.all <- rbind(morse.all, morse.fit$mcmc[[i]][seq(mcmc.size),])
      }

      # rename and convert parameter values
      morse.all[,c(paste0(morse.fit$estim.par$parameters,"_log10"))] %>%
        tibble::as_tibble() %>%
        dplyr::mutate_all(function(x) 10^x) %>%
        dplyr::rename_with(.fn=function(x) sub("_log10","", x)) -> morse.all
      if(reset.hb)
        morse.all$hb <- c(0)

      log_msg(paste0(nm, " (n=",nrow(morse.all),")"))
      for(i in seq(nrow(morse.all))) {
        ps <- c(ps, parameter_set(model=nm,param=as.list(morse.all[i,])))
        #log_msg("  ",paste(names(morse.all),unlist(morse.all[i,]),sep="=",collapse=", "))
      }
    }
  }
  return(ps)
}
