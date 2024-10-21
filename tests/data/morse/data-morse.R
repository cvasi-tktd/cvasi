library(morse)

# simple fit from morse tutorial
data("propiconazole")
survData = survData(propiconazole)
fit = survFit(survData, model_type="SD")

# clip MCMC samples to a reasonable length
fit$mcmc[[1]] <- fit$mcmc[[1]][1:50,]
fit$mcmc[[2]] <- fit$mcmc[[2]][1:50,]
fit$mcmc[[3]] <- fit$mcmc[[3]][1:50,]
saveRDS(fit, file=testthat::test_path("../data/morse/fit_sd.rds"))
save(fit, file=testthat::test_path("../data/morse/fit_sd.rdata"))

# fit IT model
fit = survFit(survData, model_type="IT")
fit$mcmc[[1]] <- fit$mcmc[[1]][1:50,]
fit$mcmc[[2]] <- fit$mcmc[[2]][1:50,]
fit$mcmc[[3]] <- fit$mcmc[[3]][1:50,]
saveRDS(fit, file=testthat::test_path("../data/morse/fit_it.rds"))

rm(propiconazole, survData, fit)
