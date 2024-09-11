################
##
## Test of model implementation
##
################

# GUTS model verification as conducted in
# EFSA Scientific Opinion on TKTD models, pp. 36, doi:10.2903/j.efsa.2018.5377
test_that("EFSA classical exposure scenario", {
  source(test_path("morse_guts-red.R"), local=TRUE)

  # Code and parameters from EFSA (2018), Appendix E
  # > 01.GUTS-implementation-check.R
  exp <- data.frame(time=seq(0,7,0.01),
                    conc=c(rep(5,401), rep(0,300)))
  kD <- 0.3 # unit: [time^-1]
  hb <- 0 # background mortality rate [time^-1]
  bw <- 0.5 # unit: 1/[D]
  zw <- 2.5 # unit: [D]
  mw <- 2.5 # unit: [D]
  beta <- 2 # dimensionless

  # test GUST-RED-SD scenario
  GUTS_RED_SD() %>%
    set_param(c(kd=kD, hb=hb, kk=bw, z=zw)) %>%
    set_exposure(exp) -> sd
  res_cvasi <- simulate(sd)
  res_morse <- morse_sd(sd)
  # test that results are equal using a sensible numerical precision
  expect_equal(res_cvasi, res_morse, tolerance = 1e-3)

  # test GUST-RED-IT scenario
  GUTS_RED_IT() %>%
    set_param(c(kd=kD, hb=hb, alpha=mw, beta=beta)) %>%
    set_exposure(exp) -> it
  res_cvasi <- simulate(it)
  res_morse <- morse_it(it)
  # test that results are equal using a sensible numerical precision
  expect_equal(res_cvasi, res_morse, tolerance = 1e-3)
})

# GUTS model verification as conducted in
# EFSA Scientific Opinion on TKTD models, pp. 36, doi:10.2903/j.efsa.2018.5377
test_that("EFSA pulsed scenarios", {
  source(test_path("morse_guts-red.R"), local=TRUE)

  # repeat tests for (selected) exposure multiplication factors 1..50
  for(MF in c(1,2,5,10,25,50))
  {
    # Code and parameters from EFSA (2018), Appendix E
    # > 01.GUTS-implementation-check.R
    exp <- data.frame(time=seq(0,14,0.01),
                      conc=c(rep(0,301), rep(5 * MF,200), rep(0,900)))

    kD <- 0.3 # unit: [time^-1]
    hb <- 0 # background mortality rate [time^-1]
    bw <- 0.5 # unit: 1/[D]
    zw <- 2.5 # unit: [D]
    mw <- 2.5 # unit: [D]
    beta <- 2 # dimensionless

    # test GUST-RED-SD scenario
    GUTS_RED_SD() %>%
      set_param(c(kd=kD, hb=hb, kk=bw, z=zw)) %>%
      set_exposure(exp) -> sd
    res_cvasi <- simulate(sd)
    res_morse <- morse_sd(sd)
    # test that results are equal using a sensible numerical precision
    expect_equal(res_cvasi, res_morse, tolerance = 1e-3)

    # test GUST-RED-IT scenario
    GUTS_RED_IT() %>%
      set_param(c(kd=kD, hb=hb, alpha=mw, beta=beta)) %>%
      set_exposure(exp) -> it
    res_cvasi <- simulate(it)
    res_morse <- morse_it(it)
    # test that results are equal using a sensible numerical precision
    expect_equal(res_cvasi, res_morse, tolerance = 1e-3)
  }
})

# GUTS model verification as conducted in
# EFSA Scientific Opinion on TKTD models, pp. 36
# doi:10.2903/j.efsa.2018.5377
test_that("EFSA extreme scenarios: zero exposure, no background mortality", {
  source(test_path("morse_guts-red.R"), local=TRUE)

  # Code and parameters from EFSA (2018), Appendix E
  # > 01.GUTS-implementation-check.R
  exp <- data.frame(time=seq(0,7,0.01), conc=rep(0,701))
  kD <- 0.3 # unit: [time^-1]
  hb <- 0 # background mortality rate [time^-1]
  bw <- 0.5 # unit: 1/[D]
  zw <- 2.5 # unit: [D]
  mw <- 2.5 # unit: [D]
  beta <- 2 # dimensionless

  # test GUST-RED-SD scenario
  GUTS_RED_SD() %>%
    set_param(c(kd=kD, hb=hb, kk=bw, z=zw)) %>%
    set_exposure(exp) -> sd
  sd_cvasi <- simulate(sd)
  sd_morse <- morse_sd(sd)
  # test that results are equal using a sensible numerical precision
  expect_equal(sd_cvasi, sd_morse, tolerance = 1e-3)

  # test GUST-RED-IT scenario
  GUTS_RED_IT() %>%
    set_param(c(kd=kD, hb=hb, alpha=mw, beta=beta)) %>%
    set_exposure(exp) -> it
  it_cvasi <- simulate(it)
  it_morse <- morse_it(it)
  # test that results are equal using a sensible numerical precision
  expect_equal(it_cvasi, it_morse, tolerance = 1e-3)

  # internal concentrations must be identical in both models
  expect_equal(sd_cvasi$D, it_cvasi$D, tolerance = 0.001)
})

# GUTS model verification as conducted in
# EFSA Scientific Opinion on TKTD models, pp. 36
# doi:10.2903/j.efsa.2018.5377
test_that("EFSA extreme scenarios: high exposure over days 1-4", {
  source(test_path("morse_guts-red.R"), local=TRUE)

  # Code and parameters from EFSA (2018), Appendix E
  # > 01.GUTS-implementation-check.R
  exp <- data.frame(time=seq(0,7,0.01),
                    conc=c(rep(100,401), rep(0,300)))
  kD <- 0.3 # unit: [time^-1]
  hb <- 0 # background mortality rate [time^-1]
  bw <- 0.5 # unit: 1/[D]
  zw <- 2.5 # unit: [D]
  mw <- 2.5 # unit: [D]
  beta <- 2 # dimensionless

  # test GUST-RED-SD scenario
  GUTS_RED_SD() %>%
    set_param(c(kd=kD, hb=hb, kk=bw, z=zw)) %>%
    set_exposure(exp) -> sd
  sd_cvasi <- simulate(sd)
  sd_morse <- morse_sd(sd)
  # test that results are equal using a sensible numerical precision
  expect_equal(sd_cvasi, sd_morse, tolerance = 1e-3)

  # test GUST-RED-IT scenario
  GUTS_RED_IT() %>%
    set_param(c(kd=kD, hb=hb, alpha=mw, beta=beta)) %>%
    set_exposure(exp) -> it
  it_cvasi <- simulate(it)
  it_morse <- morse_it(it)
  # test that results are equal using a sensible numerical precision
  expect_equal(it_cvasi, it_morse, tolerance = 1e-3)

  # internal concentrations must be identical in both models
  expect_equal(sd_cvasi$D, it_cvasi$D, tolerance = 0.001)
})

# GUTS model verification as conducted in
# EFSA Scientific Opinion on TKTD models, pp. 36
# doi:10.2903/j.efsa.2018.5377
test_that("EFSA extreme scenarios: zero exposure with slight background mortality", {
  source(test_path("morse_guts-red.R"), local=TRUE)

  # Code and parameters from EFSA (2018), Appendix E
  # > 01.GUTS-implementation-check.R
  exp <- data.frame(time=seq(0,7,0.01), conc=rep(0,701))
  kD <- 0.3 # unit: [time^-1]
  hb <- 0.05 # background mortality rate [time^-1]
  bw <- 0.5 # unit: 1/[D]
  zw <- 2.5 # unit: [D]
  mw <- 2.5 # unit: [D]
  beta <- 2 # dimensionless

  # test GUST-RED-SD scenario
  GUTS_RED_SD() %>%
    set_param(c(kd=kD, hb=hb, kk=bw, z=zw)) %>%
    set_exposure(exp) -> sd
  sd_cvasi <- simulate(sd)
  sd_morse <- morse_sd(sd)
  # test that results are equal using a sensible numerical precision
  expect_equal(sd_cvasi, sd_morse, tolerance = 1e-3)

  # test GUST-RED-IT scenario
  GUTS_RED_IT() %>%
    set_param(c(kd=kD, hb=hb, alpha=mw, beta=beta)) %>%
    set_exposure(exp) -> it
  it_cvasi <- simulate(it)
  it_morse <- morse_it(it)

  # remove column H (hazard) from results because it is not explicitly calculated
  # by the morse model implementation, therefore we cannot check it
  it_cvasi <- dplyr::select(it_cvasi, !H)
  it_morse <- dplyr::select(it_morse, !H)

  # test that results are equal using a sensible numerical precision
  expect_equal(it_cvasi, it_morse, tolerance = 1e-3)

  # internal concentrations must be identical in both models
  expect_equal(sd_cvasi$D, it_cvasi$D, tolerance = 0.001)
})

################
##
## Snapshots which provide a constant baseline for model outputs.
## The results have been compared manually with the graphs in EFSA (2018).
##
################

test_that("snapshot: zero exposure, no bg mortality", {
  exp <- data.frame(time=seq(0,7,0.01), conc=rep(0,701))
  kD <- 0.3 # unit: [time^-1]
  hb <- 0 # background mortality rate [time^-1]
  bw <- 0.5 # unit: 1/[D]
  zw <- 2.5 # unit: [D]
  mw <- 2.5 # unit: [D]
  beta <- 2 # dimensionless

  # test GUST-RED-SD scenario
  GUTS_RED_SD() %>%
    set_param(c(kd=kD, hb=hb, kk=bw, z=zw)) %>%
    set_exposure(exp) %>%
    simulate() -> res_sd
  #ggplot2::ggplot(res_sd) + ggplot2::geom_line(aes(time, D))
  #ggplot2::ggplot(res_sd) + ggplot2::geom_line(aes(time, S*100))
  expect_snapshot_value(res_sd, style="json2", tolerance = 1e-3)

  # test GUST-RED-IT scenario
  GUTS_RED_IT() %>%
    set_param(c(kd=kD, hb=hb, alpha=mw, beta=beta)) %>%
    set_exposure(exp) %>%
    simulate() -> res_it
  #ggplot2::ggplot(res_it) + ggplot2::geom_line(aes(time, D))
  #ggplot2::ggplot(res_it) + ggplot2::geom_line(aes(time, S*100))
  expect_snapshot_value(res_it, style="json2", tolerance = 1e-3)
})

test_that("snapshot: high exposure, no bg mortality", {
  exp <- data.frame(time=seq(0,7,0.01),
                    conc=c(rep(100,401), rep(0,300)))
  kD <- 0.3 # unit: [time^-1]
  hb <- 0 # background mortality rate [time^-1]
  bw <- 0.5 # unit: 1/[D]
  zw <- 2.5 # unit: [D]
  mw <- 2.5 # unit: [D]
  beta <- 2 # dimensionless

  # test GUST-RED-SD scenario
  GUTS_RED_SD() %>%
    set_param(c(kd=kD, hb=hb, kk=bw, z=zw)) %>%
    set_exposure(exp) %>%
    simulate() -> res_sd
  #ggplot2::ggplot(res_sd) + ggplot2::geom_line(aes(time, D))
  #ggplot2::ggplot(res_sd) + ggplot2::geom_line(aes(time, S*100))
  expect_snapshot_value(res_sd, style="json2", tolerance = 1e-3)

  # test GUST-RED-IT scenario
  GUTS_RED_IT() %>%
    set_param(c(kd=kD, hb=hb, alpha=mw, beta=beta)) %>%
    set_exposure(exp) %>%
    simulate() -> res_it
  #ggplot2::ggplot(res_it) + ggplot2::geom_line(aes(time, D))
  #ggplot2::ggplot(res_it) + ggplot2::geom_line(aes(time, S*100))
  expect_snapshot_value(res_it, style="json2", tolerance = 1e-3)
})

test_that("snapshot: no exposure, slight bg mortality", {
  exp <- data.frame(time=seq(0,7,0.01), conc=rep(0,701))
  kD <- 0.3 # unit: [time^-1]
  hb <- 0.05 # background mortality rate [time^-1]
  bw <- 0.5 # unit: 1/[D]
  zw <- 2.5 # unit: [D]
  mw <- 2.5 # unit: [D]
  beta <- 2 # dimensionless

  # test GUST-RED-SD scenario
  GUTS_RED_SD() %>%
    set_param(c(kd=kD, hb=hb, kk=bw, z=zw)) %>%
    set_exposure(exp) %>%
    simulate() -> res_sd
  #ggplot2::ggplot(res_sd) + ggplot2::geom_line(aes(time, D))
  #ggplot2::ggplot(res_sd) + ggplot2::geom_line(aes(time, S*100))
  expect_snapshot_value(res_sd, style="json2", tolerance = 1e-3)

  # test GUST-RED-IT scenario
  GUTS_RED_IT() %>%
    set_param(c(kd=kD, hb=hb, alpha=mw, beta=beta)) %>%
    set_exposure(exp) %>%
    simulate() -> res_it
  #ggplot2::ggplot(res_it) + ggplot2::geom_line(aes(time, D))
  #ggplot2::ggplot(res_it) + ggplot2::geom_line(aes(time, S*100))
  expect_snapshot_value(res_it, style="json2", tolerance = 1e-3)
})

################
##
## Test of model sensitivity
##
################
# Instead of calculating local parameter sensitivities, each parameter is
# varied by a factor and simulation outputs are numerically compared. If
# outputs match, then it follows that the sensitivities are also identical.
#
# Exposure series is identical to the one used in file `01.GUTS-implementation-check.R`
# by EFSA (2018).
test_that("EFSA sensitivity: GUTS-RED-IT", {
  source(test_path("morse_guts-red.R"), local=TRUE)

  # parameters to modify
  for(p in c("kd", "alpha", "beta")) {
    # factors to apply to parameter values
    for(f in c(0.75, 0.8, 0.9, 1, 1.1, 1.3, 1.5, 1.75, 2)) {
      # set up basic scenario
      scenario <- GUTS_RED_IT() %>%
        set_param(c(kd=0.3, hb=0, alpha=2.5, beta=2)) %>%
        set_exposure(data.frame(time= seq(0,5,0.01), conc=c(rep(0,301), rep(15,200))))
      # modify parameter value
      scenario <- scenario %>% set_param(setNames(scenario@param[[p]] * f, p))
      # simulate and compare
      expect_equal(simulate(scenario), morse_it(scenario), tolerance=0.001)
    }
  }
})

test_that("EFSA sensitivity: GUTS-RED-SD", {
  source(test_path("morse_guts-red.R"), local=TRUE)

  # parameters to modify
  for(p in c("kd", "z", "kk")) {
    # factors to apply to parameter values
    for(f in c(0.75, 0.8, 0.9, 1, 1.1, 1.3, 1.5, 1.75, 2)) {
      # set up basic scenario
      scenario <- GUTS_RED_SD() %>%
        set_param(c(kd=0.3, hb=0, kk=0.5, z=2.5)) %>%
        set_exposure(data.frame(time= seq(0,5,0.01), conc=c(rep(0,301), rep(15,200))))
      # modify parameter value
      scenario <- scenario %>% set_param(setNames(scenario@param[[p]] * f, p))
      # simulate and compare
      expect_equal(simulate(scenario), morse_sd(scenario), tolerance=0.001)
    }
  }
})


################
##
## Test of EPx calculation
##
################
test_that("EFSA LPx propiconazole", {
  filename <- test_path("../data/EFSA_propiconazole/focus.rds")
  skip_if_not(file.exists(filename))

  # load exposure series
  exp <- readRDS(filename)

  # Helper functions to return LP50 for propiconazole, model parameters
  # listed in Table 3 of EFSA (2018), p 56. Technically, the parameters' units
  # do not fit to the unit of the exposure series (umol/L vs ug/L)
  mf_sd <- function(exp) {
    GUTS_RED_SD() %>%
      set_param(c(kd=2.179, hb=0.02683, kk=0.1237, z=16.89)) %>%
      set_exposure(exp) %>%
      epx(level=50) %>%
      dplyr::pull(L.EP50) %>%
      round(digits=0)
  }

  mf_it <- function(exp) {
    GUTS_RED_IT() %>%
      set_param(c(kd=0.7194, hb=0.01579, alpha=17.69, beta=6.7)) %>%
      set_exposure(exp) %>%
      epx(level=50) %>%
      dplyr::pull(L.EP50) %>%
      round(digits=0)
  }

  ## Some LPx differ slightly from the reported values in EFSA (2018).
  ## However, it is reasonable to assume that our results are more precise than
  ## the ad-hoc model implementations provided to EFSA.

  # LPx values reported in Table 4, page 66, of EFSA (2018)
  # Apples, R1 pond
  expect_equal(mf_sd(exp$R1pond), 17)
  expect_equal(mf_it(exp$R1pond), 17)
  # Apples, R2 stream
  expect_equal(mf_sd(exp$R2stream), 44)
  expect_equal(mf_it(exp$R2stream), 49)
  # Cereals, D1 ditch
  expect_equal(mf_sd(exp$D1ditch), 3)
  expect_equal(mf_it(exp$D1ditch), 3)
  # Cereals, D1 stream
  expect_equal(mf_sd(exp$D1stream), 20, tolerance=0.06)
  expect_equal(mf_it(exp$D1stream), 24)
  # Cereals, D3 ditch
  expect_equal(mf_sd(exp$D3ditch), 12)
  expect_equal(mf_it(exp$D3ditch), 16)
  # Cereals, D4 pond
  expect_equal(mf_sd(exp$D4pond), 12)
  expect_equal(mf_it(exp$D4pond), 12)
  # Cereals, D4 stream
  expect_equal(mf_sd(exp$D4stream), 205, tolerance=0.02)
  expect_equal(mf_it(exp$D4stream), 250, tolerance=0.02)
  # Cereals, D5 pond
  expect_equal(mf_sd(exp$D5pond), 12)
  expect_equal(mf_it(exp$D5pond), 12)
  # Cereals, D5 stream
  expect_equal(mf_sd(exp$D5stream), 195, tolerance=0.02)
  expect_equal(mf_it(exp$D5stream), 237, tolerance=0.01)
  # Cereals, R4 stream
  expect_equal(mf_sd(exp$R4stream), 39)
  expect_equal(mf_it(exp$R4stream), 39)
})
