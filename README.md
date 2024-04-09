
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cvasi: Calibration, Validation, and Simulation of TKTD models in R

<!-- badges: start -->
<!--[![CRAN status](https://www.r-pkg.org/badges/version/cvasi)](https://cran.r-project.org/package=cvasi)-->

[![R-CMD-check](https://github.com/bayer-int/cvasi/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bayer-int/cvasi/actions/workflows/R-CMD-check.yaml)
<!--[![Codecov test coverage](https://codecov.io/gh/xy/cvasi/branch/main/graph/badge.svg)](https://app.codecov.io/gh/xy/cvasi?branch=main)-->
<!-- badges: end -->

The `cvasi` package aims to ease the use of ecotox effect models by
providing an intuitive workflow. Model inputs and parameters are
encapsulated in `EffectScenario` objects which can be piped to other
functions. Operations can be chained using the `tidyr` syntax. The most
time-consuming processes can be run in parallel if requested.

The package provides facilities to

- simulate effect models GUTS-RED, DEB, and Lemna
- calculate survival and other effect endpoints
- create dose-response curves
- derive effect profiles (EPx values)
- import exposure time-series from FOCUS TOXSWA
- import fitted parameters from morse
- …

Please have a look at the [CHANGELOG](CHANGELOG.md) for an overview of
user-facing updates and changes.

## Documentation

The package contains the following vignettes

- [User
  Manual](https://github.com/bayer-int/cvasi/blob/master/doc/manual.md)
- [Modeling
  Howto](https://github.com/bayer-int/cvasi/blob/master/doc/howto.md)

They can also be accessed locally by executing an *R* statement such as:

``` r
vignette("manual", package="cvasi")
```

## Installation

1.  Create a GitHub Access Token if you don’t have one already:
    - Open [GitHub](https://github.com/)
    - Go to *User Preferences* \> *Settings* \> *Developer Settings*
    - Go to *Personal Access Tokens*
    - Click *Generate new token*  
      Assign a token name, set an expiration date, select `repo` scope,
      and create the token.  
      **Please save it for later use**, it will only be displayed once.
2.  Install the packages in the following order

``` r
install.packages("remotes", dependencies=TRUE)
Sys.setenv(GITHUB_PAT=rstudioapi::askForPassword("Enter your GitHub token"))
remotes::install_github("bayer-int/cvasi", dependencies=TRUE, upgrade="never")
```

## Example

Basic usage:

``` r
library(cvasi)

# create and parameterize a GUTS-RED-IT scenario
GUTS_RED_IT() %>%
  set_param(c(kd=0.0005,hb=0,alpha=0.4,beta=1.5)) %>%
  set_exposure(data.frame(t=c(0,100,101,200,201,400),pec=c(0,0,0.1,0.1,0,0))) -> scenario

# simulate scenario
scenario %>%
  simulate(times=1:400) %>%
  tail()
#>     time           D H
#> 395  395 0.004429420 0
#> 396  396 0.004427206 0
#> 397  397 0.004424993 0
#> 398  398 0.004422781 0
#> 399  399 0.004420570 0
#> 400  400 0.004418360 0
```

Calculation of effects:

``` r
# calculate effect level
scenario %>% effect()
#> # A tibble: 1 × 4
#>   scenario         L L.dat.start L.dat.end
#>   <list>       <dbl>       <dbl>     <dbl>
#> 1 <GutsRdIt> 0.00135           0       400

# create a dose response curve
scenario %>% dose_response() -> drc
head(drc)
#>   endpoint        mf      effect
#> 1        L  3.812500 0.009920108
#> 2        L  4.799653 0.013948570
#> 3        L  6.042405 0.019601514
#> 4        L  7.606938 0.027459506
#> 5        L  9.576567 0.038355140
#> 6        L 12.056184 0.053336114

library(ggplot2)
ggplot(drc) + geom_point(aes(mf,effect)) + scale_x_log10()
```

<img src="doc/figures/README-drc-1.png" width="50%" />

``` r
# derive EPx values
scenario %>% epx()
#> # A tibble: 1 × 3
#>   scenario   L.EP10 L.EP50
#>   <list>      <dbl>  <dbl>
#> 1 <GutsRdIt>   19.0   82.1
```

Multiple scenarios can be processed in parallel without modifications to
the workflow:

``` r
future::plan(future::multisession)

c(GUTS_RED_IT(),GUTS_RED_SD()) %>%
  set_param(morse("path/to/fitted.RData")) %>%
  epx()
```
