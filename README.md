
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cvasi: Calibration, Validation, and Simulation of TKTD models in R

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/cvasi)](https://cran.r-project.org/package=cvasi)
[![R-CMD-check](https://github.com/cvasi-tktd/cvasi/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cvasi-tktd/cvasi/actions/workflows/R-CMD-check.yaml)
<!--[![Codecov test coverage](https://codecov.io/gh/xy/cvasi/branch/main/graph/badge.svg)](https://app.codecov.io/gh/xy/cvasi?branch=main)-->
<!-- badges: end -->

The `cvasi` package aims to ease the use of ecotox effect models by
providing an intuitive workflow. Model inputs and parameters are
encapsulated in scenario objects which can be piped to other functions.
Operations can be chained using the `tidyr` syntax. The most
time-consuming processes can be run in parallel if requested.

The package provides facilities to

- simulate effect models such as *GUTS-RED*, *DEB*, *Lemna*,
  *Myriophyllum*, and *Algae*
- calculate effect endpoints
- derive effect profiles (*EPx* values)
- import exposure time-series from *FOCUS TOXSWA*
- import fitted parameters from *morse*
- and more

A graphical user interface implemented in
[Shiny](https://posit.co/products/open-source/rstudio/) is also
available, see the *[cvasi.ui](https://github.com/cvasi-tktd/cvasi.ui/)*
package. Please have a look at the [Changelog](NEWS.md) for an overview
of user-facing updates and changes.

## Installation

Install the package from CRAN:

``` r
install.packages("cvasi", dependencies=TRUE)
```

Or install the newest development version from *GitHub*:

``` r
install.packages("remotes", dependencies=TRUE)
remotes::install_github("cvasi-tktd/cvasi", dependencies=TRUE)
```

For installing `cvasi` from *GitHub* on Windows computers, please make
sure that you also have
[*Rtools*](https://cran.r-project.org/bin/windows/Rtools/) installed on
your machine. *Rtools* are required to compile the package’s C code.

## Documentation

The package contains the following vignettes

- [User
  Manual](https://github.com/cvasi-tktd/cvasi/blob/main/doc/cvasi-1-manual.md)
- [Modeling
  Howto](https://github.com/cvasi-tktd/cvasi/blob/main/doc/cvasi-2-howto.md)

They can also be accessed locally by executing an *R* statement such as:

``` r
vignette("cvasi-1-manual", package="cvasi")
```

## Usage

Basic usage:

``` r
library(cvasi)

# create and parameterize a GUTS-RED-IT scenario
GUTS_RED_IT() %>%
  set_param(c(kd=0.0005, hb=0, alpha=0.4, beta=1.5)) %>%
  set_exposure(data.frame(time=c(0, 100, 101, 200, 201, 400),
                          conc=c(0, 0, 0.1, 0.1, 0, 0))) %>%
  set_times(1:400) -> scenario

# simulate scenario
results <- scenario %>% simulate()
tail(results)
#>     time           D H        S
#> 395  395 0.004429420 0 0.998655
#> 396  396 0.004427206 0 0.998655
#> 397  397 0.004424993 0 0.998655
#> 398  398 0.004422781 0 0.998655
#> 399  399 0.004420570 0 0.998655
#> 400  400 0.004418360 0 0.998655

# ... and plot simulation results
plot(results)
```

<img src="doc/figures/readme-unnamed-chunk-5-1.png" width="70%" style="display: block; margin: auto;" />

Calculation of effects:

``` r
# calculate effect level
scenario %>% effect()
#> # A tibble: 1 x 4
#>   scenario         L L.dat.start L.dat.end
#>   <list>       <dbl>       <dbl>     <dbl>
#> 1 <GutsRdIt> 0.00135           1       400

# create a dose-response curve
scenario %>% dose_response() -> drc
head(drc)
#>   endpoint        mf      effect
#> 1        L  3.812500 0.009915394
#> 2        L  4.799653 0.013954569
#> 3        L  6.042405 0.019597765
#> 4        L  7.606938 0.027459877
#> 5        L  9.576567 0.038357524
#> 6        L 12.056184 0.053336214

# plot the dose-response curve
plot(drc)
```

<img src="doc/figures/readme-unnamed-chunk-6-1.png" width="60%" style="display: block; margin: auto;" />

``` r

# derive EPx values
scenario %>% epx()
#> # A tibble: 1 x 3
#>   scenario   L.EP10 L.EP50
#>   <list>      <dbl>  <dbl>
#> 1 <GutsRdIt>   19.0   82.1
```

Multiple scenarios can be processed in parallel without modifications to
the workflow:

``` r
# enable parallel processing
future::plan(future::multisession)

# derive EPx for a list of 100 scenarios in parallel
rep(c(scenario), 100) %>% epx()

# disable parallel processing
future::plan(future::sequential)
```

## License

The package and its source code is free and open-source software
available under the [GPL-3.0
license](https://github.com/cvasi-tktd/cvasi/blob/main/LICENSE.md).

## Issues

If you find any issues or bugs within the package, please create a [new
issue](https://github.com/cvasi-tktd/cvasi/issues) on GitHub.

## Contributing

Contributions to the project are welcome! Please have a look at the
[Contribution
Guidelines](https://github.com/cvasi-tktd/cvasi/blob/main/CONTRIBUTING.md)
before submitting a Pull Request.

## Acknowledgements

Financial support for creation and release of this software project was
provided by Bayer Crop Science. This R package started as an internal
project at Bayer Crop Science and the project owners would like to thank
the people who have contributed (in no particular order):

Nils Kehrein, Johannes Witt, André Gergs, Thomas Preuss, Julian
Heinrich, Zhenglei Gao, Tjalling Jager, Dirk Nickisch, Torben Wittwer,
and Peter Vermeiren.
