
# overwrite first MCMC estimate with median to ease prediction
setMedian <- function(fit) {
  for(i in seq(fit$mcmcInfo$n.chains)) {
    fit$mcmc[[i]][1,"kd_log10"] <- log10(fit$estim.par$median[1])
    fit$mcmc[[i]][1,"hb_log10"] <- log10(fit$estim.par$median[2])
    if(fit$model_type=="IT") {
      fit$mcmc[[i]][1,"alpha_log10"] <- log10(fit$estim.par$median[3])
      fit$mcmc[[i]][1,"beta_log10"] <- log10(fit$estim.par$median[4])
    } else {
      fit$mcmc[[i]][1,"z_log10"] <- log10(fit$estim.par$median[3])
      fit$mcmc[[i]][1,"kk_log10"] <- log10(fit$estim.par$median[4])
    }
  }
  fit
}

dat <- read.table(file='data-raw/Ring-test-dataset-C.txt',sep='\t',header=T)
sdat <- morse::survData(dat)

# fit GUTS-RED-IT model
#fit.it <- morse::survFit(sdat, model_type = 'IT')
#fit.it <- setMedian(fit.it)
#saveRDS(fit.it,"data-raw/minnow_fit.it.rds")
fit.it <- readRDS("data-raw/minnow_fit.it.rds")

# fit GUTS-RED-SD model
#fit.sd <- morse::survFit(sdat, model_type = 'SD')
#fit.sd <- setMedian(fit.sd)
#saveRDS(fit.sd,"data-raw/minnow_fit.sd.rds")
fit.sd <- readRDS("data-raw/minnow_fit.sd.rds")

# export effect scenarios
GUTS_RED_IT() %>%
  set_tag("fathead minnow") %>%
  set_param(c(kd=round(fit.it$estim.par$median[1], 4),
              #hb=round(fit.it$estim.par$median[2], 4),
              hb=0,
              alpha=round(fit.it$estim.par$median[3], 4),
              beta=round(fit.it$estim.par$median[4], 4))) %>%
  set_exposure(data.frame(t=seq(0,4),c=c(1))) -> minnow_it
usethis::use_data(minnow_it, overwrite=TRUE)


GUTS_RED_SD() %>%
  set_tag("fathead minnow") %>%
  set_param(c(kd=round(fit.sd$estim.par$median[1], 4),
              #hb=round(fit.sd$estim.par$median[2], 4),
              hb=0,
              z=round(fit.sd$estim.par$median[3], 4),
              kk=round(fit.sd$estim.par$median[4], 4))) %>%
  set_exposure(data.frame(t=seq(0,4),c=c(1))) -> minnow_sd
usethis::use_data(minnow_sd, overwrite=TRUE)


#predict_ode(fit.it, data.frame(time=0:4,conc=c(4.745483),replicate=c(1)),mcmc_size = 1)

rm(dat, fit.it, fit.sd, minnow_it, minnow_sd, sdat, setMedian)
