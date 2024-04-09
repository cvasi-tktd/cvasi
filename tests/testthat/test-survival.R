#
# Fixed version of morse function, original doesn't work in morse package
# version <= 3.2.5
#
Surv.SD_Cext <- function(Cw, time, kk, kd, z, hb) {
  time.prec = dplyr::lag(time, 1) ; time.prec[1] = time[1] #   time[1] = tprec[1]
  # Using log transfomration: log(a+b) = log(a) + log(1+b/a) may prevent the numerical issues raised by exponential
  diff.int = (exp(time %*% t(kd)) + exp(time.prec %*% t(kd)) )*Cw/2 * (time-time.prec) #OK time[1]-tprec[1] = 0
  D = kd * exp(-kd %*% t(time)) * t(apply(diff.int, 2, cumsum))
  lambda = kk * pmax(D-z,0) + hb # the pmax function is important here for elementwise maximum with 0 and D[i,j]-z ATTENTION: pmax(0,D) != pmax(D,0)
  ## fix for new behaviour of dplyr::lag()
  lambda.prec = dplyr::lag(lambda[1,], 1 ) ; lambda.prec[1] = lambda[1]
  #lambda.prec = lag(lambda, 1 ) ; lambda.prec[1] = lambda[1]
  ## fix end
  int.lambda =  t(t((lambda + lambda.prec)/2) * (time-time.prec))
  S <- exp(-t(apply(int.lambda,1,cumsum)))
  return(S)
}


test_that("GUTS-RED constant exposure", {
  # GUTS model verification as conducted in
  # EFSA Scientific Opinion on TKTD models, pp. 36
  # doi:10.2903/j.efsa.2018.5377

  #
  # Begin EFSA code: 01.GUTS-implementation-check.R
  #
  exp <- data.frame(seq(0,7,0.01),
                    c(rep(5,401), rep(0,300)))
  colnames(exp) <- c("time","conc")
  MF <- 1 # Multiplication factor of the exposure profile
  kD <- 0.3 # unit: [time^-1]

  hb <- 0 # background mortality rate [time^-1]
  SOT_SD <- Surv.SD_Cext ## FIXED, morse function contains a bug preventing calculations atm
  #SOT_SD <- morse:::Surv.SD_Cext
  bw <- 0.5 # unit: 1/[D]
  zw <- 2.5 # unit: [D]
  SOT_SD_cst_simu <- SOT_SD(Cw=exp$conc, time=exp$time, kk=bw, kd=kD, z=zw, hb=hb)

  SOT_IT <- morse:::Surv.IT_Cext
  mw <- 2.5 # unit: [D]
  beta <- 2 # dimensionless
  SOT_IT_cst_simu <- SOT_IT(Cw=exp$conc, time=exp$time, kd=kD, hb=hb, alpha=mw, beta=beta)
  #
  # EFSA code end
  #

  # Compare with morse's standard prediction
  GUTS_RED_IT() %>%
    set_param(c(kd=kD,hb=hb,alpha=mw,beta=beta)) %>%
    set_exposure(exp) %>%
    survival(approx="constant") -> git
  GUTS_RED_SD() %>%
    set_param(c(kd=kD,hb=hb,kk=bw,z=zw)) %>%
    set_exposure(exp) %>%
    survival(approx="constant") -> gsd

  expect_lt(mae(SOT_IT_cst_simu[1,],git$survival), 1e-5, "IT:S approx")
  expect_lt(rmse(SOT_IT_cst_simu[1,],git$survival), 1e-5, "IT:S approx")
  expect_lt(mae(SOT_SD_cst_simu[1,],gsd$survival), 1e-5, "SD:S approx")
  expect_lt(rmse(SOT_SD_cst_simu[1,],gsd$survival), 1e-5, "SD:S approx")

  # Compare with morse's ODE prediction
  # Fit of our approach should be better as morse will also use the ODE solver
  GUTS_RED_IT() %>%
    set_param(c(kd=kD,hb=hb,alpha=mw,beta=beta)) %>%
    set_exposure(exp) %>%
    survival(approx="linear", hmax=NULL) -> git
  GUTS_RED_SD() %>%
    set_param(c(kd=kD,hb=hb,kk=bw,z=zw)) %>%
    set_exposure(exp) %>%
    survival(approx="linear", hmax=NULL) -> gsd

  morse:::SurvSD_ode(exp$conc,exp$time,replicate=1,kk=bw,kd=kD,z=zw,hb=hb,mcmc_size=1) -> ode.sd
  morse:::SurvIT_ode(exp$conc,exp$time,replicate=1,kd=kD,hb=hb,alpha=mw,beta=beta,mcmc_size=1) -> ode.it

  expect_lt(mae(ode.it[,1],git$survival), 1e-10, "IT:S ode")
  expect_lt(rmse(ode.it[,1],git$survival), 1e-10, "IT:S ode")
  expect_lt(mae(ode.sd[,1],gsd$survival), 1e-10, "SD:D ode")
  expect_lt(rmse(ode.sd[,1],gsd$survival), 1e-10, "SD:D ode")
})

test_that("GUTS-RED time-variable exposure", {
  # GUTS model verification as conducted in
  # EFSA Scientific Opinion on TKTD models, pp. 36
  # doi:10.2903/j.efsa.2018.5377

  #
  # Begin EFSA code: 01.GUTS-implementation-check.R
  #
  exp <- data.frame(seq(0,14,0.01),
                    c(rep(0,301), rep(5,200), rep(0,900)))
  colnames(exp) <- c("time","conc")
  kD <- 0.3 # unit: [time^-1]
  hb <- 0 # background mortality rate [time^-1]
  SOT_IT <- morse:::Surv.IT_Cext
  SOT_SD <- Surv.SD_Cext ## FIXED, morse function contains a bug preventing calculations atm
  #SOT_SD <- morse:::Surv.SD_Cext
  bw <- 0.5 # unit: 1/[D]
  zw <- 2.5 # unit: [D]
  mw <- 2.5 # unit: [D]
  beta <- 2 # dimensionless
  #
  # EFSA code end
  #

  # Simulate GUTS-RED-IT
  for (i in 2:50){
    MF <- i
    morse:::SurvIT_ode(exp$conc*MF,exp$time,replicate=1,kd=kD,hb=hb,alpha=mw,beta=beta,mcmc_size=1) -> ode.it

    GUTS_RED_IT() %>%
      set_param(c(kd=kD,hb=hb,alpha=mw,beta=beta)) %>%
      set_exposure(data.frame(t=exp$time,p=exp$conc*MF)) %>%
      survival(hmax=NULL) -> git

    expect_lt(nmae(ode.it[,1],git$survival), 1e-5, paste0("IT:S, MF=",MF))
    expect_lt(nrmse(ode.it[,1],git$survival), 1e-5, paste0("IT:S, MF=",MF))
  }

  # Simulate GUTS-RED-SD
  for (i in 2:50){
    MF <- i
    morse:::SurvSD_ode(exp$conc*MF,exp$time,replicate=1,kk=bw,kd=kD,z=zw,hb=hb,mcmc_size=1) -> ode.sd

    GUTS_RED_SD() %>%
      set_param(c(kd=kD,hb=hb,kk=bw,z=zw)) %>%
      set_exposure(data.frame(t=exp$time,p=exp$conc*MF)) %>%
      survival(hmax=NULL) -> gsd

    expect_lt(nmae(ode.sd[,1],gsd$survival), 1e-5, paste0("SD:S, MF=",MF))
    expect_lt(nrmse(ode.sd[,1],gsd$survival), 1e-5, paste0("SD:S, MF=",MF))
  }
})

test_that("GUTS-RED extreme cases", {
  # GUTS model verification as conducted in
  # EFSA Scientific Opinion on TKTD models, pp. 36
  # doi:10.2903/j.efsa.2018.5377

  #
  # Begin EFSA code: 01.GUTS-implementation-check.R
  #
  MF <- 1
  kD <- 0.3 # unit: [time^-1]
  hb <- 0 # background mortality rate
  bw <- 0.5 # unit: 1/[D]
  zw <- 2.5 # unit: [D]
  mw <- 2.5 # unit: [D]
  beta <- 2 # dimensionless
  SOT_SD <- Surv.SD_Cext ## FIXED, morse function contains a bug preventing calculations atm
  #SOT_SD <- morse:::Surv.SD_Cext
  SOT_IT <- morse:::Surv.IT_Cext

  # Zero exposure (conc=0)
  exp <- data.frame(seq(0,7,0.01), rep(0,701))
  colnames(exp) <- c("time","conc")
  SOT_SD_simu_0 <- SOT_SD(Cw=exp$conc, time=exp$time, kk=bw, kd=kD, z=zw, hb=hb)
  SOT_IT_simu_0 <- SOT_IT(Cw=exp$conc, time=exp$time, kd=kD,hb=hb, alpha=mw, beta=beta)
  #
  # EFSA code end
  #
  morse:::SurvSD_ode(exp$conc,exp$time,replicate=1,kk=bw,kd=kD,z=zw,hb=hb,mcmc_size=1) -> ode.sd
  morse:::SurvIT_ode(exp$conc,exp$time,replicate=1,kd=kD,hb=hb,alpha=mw,beta=beta,mcmc_size=1) -> ode.it

  GUTS_RED_IT() %>%
    set_param(c(kd=kD,hb=hb,alpha=mw,beta=beta)) %>%
    set_exposure(exp) %>%
    survival(approx="constant", hmax=NULL) -> git
  GUTS_RED_SD() %>%
    set_param(c(kd=kD,hb=hb,kk=bw,z=zw)) %>%
    set_exposure(exp) %>%
    survival(approx="constant", hmax=NULL) -> gsd

  expect_lt(nmae(SOT_IT_simu_0[1,],git$survival), 1e-10, "IT:S approx,zero exp")
  expect_lt(nrmse(SOT_IT_simu_0[1,],git$survival), 1e-10, "IT:S approx,zero exp")
  expect_lt(nmae(SOT_SD_simu_0[1,],gsd$survival), 1e-10, "SD:S approx,zero exp")
  expect_lt(nrmse(SOT_SD_simu_0[1,],gsd$survival), 1e-10, "SD:S approx,zero exp")

  expect_lt(nmae(ode.it[,1],git$survival), 1e-10, "IT:S ODE,zero exp")
  expect_lt(nrmse(ode.it[,1],git$survival), 1e-10, "IT:S ODE,zero exp")
  expect_lt(nmae(ode.sd[,1],gsd$survival), 1e-10, "SD:S ODE,zero exp")
  expect_lt(nrmse(ode.sd[,1],gsd$survival), 1e-10, "SD:S ODE,zero exp")
  rm(git,gsd,ode.it,ode.sd,exp,SOT_IT_simu_0,SOT_SD_simu_0)

  #
  # Begin EFSA code: 01.GUTS-implementation-check.R
  #
  # High exposure over 4 days (conc=100)
  exp <- data.frame(seq(0,7,0.01),
                    c(rep(100,401), rep(0,300)))
  colnames(exp) <- c("time","conc")
  SOT_SD_simu_100 <- SOT_SD(Cw=exp$conc, time=exp$time, kk=bw, kd=kD, z=zw, hb=hb)
  SOT_IT_simu_100 <- SOT_IT(Cw=exp$conc, time=exp$time, kd=kD,hb=hb, alpha=mw, beta=beta)
  #
  # EFSA code end
  #
  morse:::SurvSD_ode(exp$conc,exp$time,replicate=1,kk=bw,kd=kD,z=zw,hb=hb,mcmc_size=1) -> ode.sd
  morse:::SurvIT_ode(exp$conc,exp$time,replicate=1,kd=kD,hb=hb,alpha=mw,beta=beta,mcmc_size=1) -> ode.it

  GUTS_RED_IT() %>%
    set_param(c(kd=kD,hb=hb,alpha=mw,beta=beta)) %>%
    set_exposure(exp) %>%
    survival(approx="constant", hmax=NULL) -> git
  GUTS_RED_SD() %>%
    set_param(c(kd=kD,hb=hb,kk=bw,z=zw)) %>%
    set_exposure(exp) %>%
    survival(approx="constant", hmax=NULL) -> gsd

  ## morse's approximations have low precision
  expect_lt(nmae(SOT_IT_simu_100[1,],git$survival), 1e-5, "IT:S approx,zero exp")
  expect_lt(nrmse(SOT_IT_simu_100[1,],git$survival), 1e-5, "IT:S approx,zero exp")
  expect_lt(nmae(SOT_SD_simu_100[1,],gsd$survival), 1e-3, "SD:S approx,zero exp")
  expect_lt(nrmse(SOT_SD_simu_100[1,],gsd$survival), 1e-3, "SD:S approx,zero exp")
  ## morse's ODE models fare much better
  expect_lt(nmae(ode.it[,1],git$survival), 1e-6, "IT:S ODE,zero exp")
  expect_lt(nrmse(ode.it[,1],git$survival), 1e-6, "IT:S ODE,zero exp")
  expect_lt(nmae(ode.sd[,1],gsd$survival), 1e-10, "SD:S ODE,zero exp")
  expect_lt(nrmse(ode.sd[,1],gsd$survival), 1e-10, "SD:S ODE,zero exp")
  rm(git,gsd,ode.it,ode.sd,exp,SOT_IT_simu_100,SOT_SD_simu_100)

  #
  # Begin EFSA code: 01.GUTS-implementation-check.R
  #
  # Only background mortality (conc=0, hb=0.05)
  hb <- 0.05
  exp <- data.frame(seq(0,7,0.01), rep(0,701))
  colnames(exp) <- c("time","conc")
  SOT_SD_simu_BG <- SOT_SD(Cw=exp$conc, time=exp$time, kk=bw,kd=kD, z=zw, hb=hb)
  SOT_IT_simu_BG <- SOT_IT(Cw=exp$conc, time=exp$time, kd=kD,hb=hb, alpha=mw, beta=beta)
  #
  # EFSA code end
  #

  morse:::SurvSD_ode(exp$conc,exp$time,replicate=1,kk=bw,kd=kD,z=zw,hb=hb,mcmc_size=1) -> ode.sd
  morse:::SurvIT_ode(exp$conc,exp$time,replicate=1,kd=kD,hb=hb,alpha=mw,beta=beta,mcmc_size=1) -> ode.it

  GUTS_RED_IT() %>%
    set_param(c(kd=kD,hb=hb,alpha=mw,beta=beta)) %>%
    set_exposure(exp) %>%
    survival(approx="constant") -> git
  GUTS_RED_SD() %>%
    set_param(c(kd=kD,hb=hb,kk=bw,z=zw)) %>%
    set_exposure(exp) %>%
    survival(approx="constant") -> gsd

  expect_lt(nmae(SOT_IT_simu_BG[1,],git$survival), 1e-10, "IT:S approx,zero exp")
  expect_lt(nrmse(SOT_IT_simu_BG[1,],git$survival), 1e-10, "IT:S approx,zero exp")
  expect_lt(nmae(SOT_SD_simu_BG[1,],gsd$survival), 1e-10, "SD:S approx,zero exp")
  expect_lt(nrmse(SOT_SD_simu_BG[1,],gsd$survival), 1e-10, "SD:S approx,zero exp")

  expect_lt(nmae(ode.it[,1],git$survival), 1e-10, "IT:S ODE,zero exp")
  expect_lt(nrmse(ode.it[,1],git$survival), 1e-10, "IT:S ODE,zero exp")
  expect_lt(nmae(ode.sd[,1],gsd$survival), 1e-10, "SD:S ODE,zero exp")
  expect_lt(nrmse(ode.sd[,1],gsd$survival), 1e-10, "SD:S ODE,zero exp")
  rm(git,gsd,ode.it,ode.sd,exp,SOT_IT_simu_BG,SOT_SD_simu_BG)
})

test_that("GUTS-RED sensitivity analysis", {
  # GUTS model verification as conducted in
  # EFSA Scientific Opinion on TKTD models, pp. 36
  # doi:10.2903/j.efsa.2018.5377

  #
  # Begin EFSA code: 01.GUTS-implementation-check.R
  #
  kD <- 0.3 # unit: [time^-1]
  hb <- 0 # background mortality rate
  bw <- 0.5 # unit: 1/[D]
  zw <- 2.5 # unit: [D]
  mw <- 2.5 # unit: [D]
  beta <- 2 # dimensionless

  exp <- data.frame(seq(0,5,0.01),
                    c(rep(0,301), rep(15,200)))
  colnames(exp) <- c("time","conc")
  percent <- -75:100
  #
  # EFSA code end
  #
  GUTS_RED_IT() %>%
    set_param(c(kd=kD,hb=hb,alpha=mw,beta=beta)) %>%
    set_exposure(exp) -> git
  GUTS_RED_SD() %>%
    set_param(c(kd=kD,hb=hb,kk=bw,z=zw)) %>%
    set_exposure(exp) -> gsd

  # Variation of kD
  kD <- 0.3*(1+percent/100)
  for (i in 1:length(percent)){
    morse:::SurvIT_ode(exp$conc,exp$time,replicate=1,kd=kD[i],hb=hb,alpha=mw,beta=beta,mcmc_size=1) %>% tail(1) -> ode.it
    git %>% set_param(c(kd=kD[i])) %>% survival(hmax=NULL) %>% tail(1) -> e.it
    expect_lt(nmae(ode.it[,1],e.it$survival), 1e-5, "IT:S ODE,kd varying")

    morse:::SurvSD_ode(exp$conc,exp$time,replicate=1,kk=bw,kd=kD[i],z=zw,hb=hb,mcmc_size=1) %>% tail(1) -> ode.sd
    gsd %>% set_param(c(kd=kD[i])) %>% survival(hmax=NULL) %>% tail(1) -> e.sd
    expect_lt(nmae(ode.sd[,1],e.sd$survival), 1e-5, "SD:S ODE,kd varying")
  }
  rm(i,ode.sd,ode.it,e.it,e.sd)

  # Variation of bw (model SD)
  kD <- 0.3
  bw <- 0.5*(1+percent/100)
  for (i in 1:length(percent)){
    morse:::SurvSD_ode(exp$conc,exp$time,replicate=1,kk=bw[i],kd=kD,z=zw,hb=hb,mcmc_size=1) %>% tail(1) -> ode.sd
    gsd %>% set_param(c(kk=bw[i])) %>% survival(hmax=NULL) %>% tail(1) -> e.sd
    expect_lt(nmae(ode.sd[,1],e.sd$survival), 1e-5, "SD:S ODE,kk varying")
  }
  rm(i,ode.sd,e.sd)

  # Variation of zw (model SD)
  bw <- 0.5
  zw <- 2.5*(1+percent/100)
  for (i in 1:length(percent)){
    morse:::SurvSD_ode(exp$conc,exp$time,replicate=1,kk=bw,kd=kD,z=zw[i],hb=hb,mcmc_size=1) %>% tail(1) -> ode.sd
    gsd %>% set_param(c(z=zw[i])) %>% survival(hmax=NULL) %>% tail(1) -> e.sd
    ## !! reduced precision !!
    expect_lt(nmae(ode.sd[,1],e.sd$survival), 5e-5, "SD:S ODE,z varying")
  }
  rm(i,ode.sd,e.sd)

  # Variation of mw (model IT)
  zw <- 2.5
  mw <- 2.5*(1+percent/100)
  for (i in 1:length(percent)){
    morse:::SurvIT_ode(exp$conc,exp$time,replicate=1,kd=kD,hb=hb,alpha=mw[i],beta=beta,mcmc_size=1) %>% tail(1) -> ode.it
    git %>% set_param(c(alpha=mw[i])) %>% survival(hmax=NULL) %>% tail(1) -> e.it
    expect_lt(nmae(ode.it[,1],e.it$survival), 1e-5, "IT:S ODE,alpha varying")
  }
  rm(i,ode.it,e.it)

  # Variation of beta (model IT)
  mw <- 2.5
  beta <- 2*(1+percent/100)
  for (i in 1:length(percent)){
    morse:::SurvIT_ode(exp$conc,exp$time,replicate=1,kd=kD,hb=hb,alpha=mw,beta=beta[i],mcmc_size=1) %>% tail(1) -> ode.it
    git %>% set_param(c(beta=beta[i])) %>% survival(hmax=NULL) %>% tail(1) -> e.it
    expect_lt(nmae(ode.it[,1],e.it$survival), 1e-5, "IT:S ODE,beta varying")
  }
  rm(i,ode.it,e.it)

})
