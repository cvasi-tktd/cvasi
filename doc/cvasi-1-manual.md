User Manual
================
N. Kehrein
24 Octobre, 2024

- [Required skills](#required-skills)
- [How to install](#how-to-install)
- [Modeling and assessment workflow](#modeling-and-assessment-workflow)
  - [Overview](#overview)
  - [Setting up a scenario](#setting-up-a-scenario)
  - [Running a simulation](#running-a-simulation)
  - [Deriving assessment endpoints](#deriving-assessment-endpoints)
- [Data format of model inputs](#data-format-of-model-inputs)
- [How to derive model inputs](#how-to-derive-model-inputs)
- [Description of model outputs](#description-of-model-outputs)
  - [Simulation results](#simulation-results)
  - [Effect levels](#effect-levels)
  - [Effect profiles](#effect-profiles)
- [References](#references)

The *cvasi* package is a software library that extends the features of
the programming language [*R*](https://www.r-project.org/) by routines
for ecotox effect modeling. The routines are intended to be used for
scientific purposes as well as in the context of regulatory risk
assessments. *cvasi* implements model equations and default parameters
to simulate a set of toxicokinetic-toxicodynamic (TKTD) models. The
models are intended to assess the effects of substance exposure on
effect endpoints, such as growth and reproduction, for a range of
standard test species.

The package offers a standardized framework which can be extended by
(almost) any TKTD model that can be implemented in *R*. Its modeling
workflows are designed to be self-explanatory and as intuitive as
possible to make modeling approaches more accessible. In addition, the
framework’s routines are optimized to reduce the computing time required
for extensive risk assessments.

# Required skills

The *cvasi* package and its routines can be used by anyone with basic to
intermediate knowledge of the *R* programming language. The routines can
be used in a procedural fashion, commonly referred to as *base R*, in
which function statements are executed, assigned to a variable, and then
passed on to the next function:

``` r
library(cvasi)

## Sample base R workflow ##
# Create a scenario object of model GUTS-RED-IT
my_it <- GUTS_RED_IT()
# Set model parameters
my_it <- set_param(my_it, c(kd=1.2, hb=0, alpha=9.2, beta=4.3))
# Print scenario details
my_it
#> 'GUTS-RED-IT' scenario
#> param: kd=1.2, hb=0, alpha=9.2, beta=4.3
#> init : D=0, H=0
#> endpt: L
#> times: none
#> forcs: none
#> expsr: none
#> >> exposure series is empty
```

A more advanced approach is to use the *tidyr* syntax (cf. Wickham &
Grolemund 2017) which uses verb-like statements to modify data. *tidyr*
syntax can be used to chain functions calls which pass the result of one
function on to the next:

``` r
## Example 'tidyr' workflow ##
# the pipeline (%>%) symbol passes results to the next statement
GUTS_RED_IT() %>%
  set_param(c(kd=1.2, hb=0, alpha=9.2, beta=4.3))
#> 'GUTS-RED-IT' scenario
#> param: kd=1.2, hb=0, alpha=9.2, beta=4.3
#> init : D=0, H=0
#> endpt: L
#> times: none
#> forcs: none
#> expsr: none
#> >> exposure series is empty
```

Both workflow styles achieve the same result, but *tidyr* is terser,
especially if several operations are performed on the same object.
*tidyr* style is the recommended way to interact with the *cvasi*
package.

# How to install

<!--The simplest way to install the *cvasi* package is by downloading it from *CRAN*:
&#10;
-->

The latest development version can be installed from GitHub:

``` r
install.packages("remotes", dependencies=TRUE)
remotes::install_github("cvasi-tktd/cvasi", dependencies=TRUE)
```

The package and its source code is also available on
[GitHub](https://github.com/cvasi-tktd/cvasi/).

# Modeling and assessment workflow

## Overview

The central concept within the *cvasi* package is the *scenario*. It
encapsulates all key properties to run a simulation such as initial
state and model parameters. The scenario can be thought of as the
collection of data and parameters necessary to reproduce a certain study
condition or situation. The set of available scenario properties depends
on the model type but may include:

- Initial state,
- Model parameters,
- Assessment parameters,
- Exposure time-series, and
- Environmental factor time-series

Scenario properties can be modified using intuitively named *tidyr*
verbs: in general, each verb accepts a scenario as an argument and
returns a modified scenario as the result. For instance, the
`set_param()` function will update a scenario’s parameters to new
values:

``` r
library(cvasi)

# Create a new GUTS-RED-IT scenario and set its model parameters
GUTS_RED_IT() %>%
  set_param(c(kd=1.2, hb=0, alpha=9.2, beta=4.3))
#> 'GUTS-RED-IT' scenario
#> param: kd=1.2, hb=0, alpha=9.2, beta=4.3
#> init : D=0, H=0
#> endpt: L
#> times: none
#> forcs: none
#> expsr: none
#> >> exposure series is empty
```

If a scenario is properly defined, it can be passed to dedicated
functions that, for instance, run a simulation or derive assessment
endpoints. These higher-level functions do not return a scenario object
but a table of e.g. simulation results for that particular scenario. As
an example, simulating the sample scenario `minnow_it` will run a
*GUTS-RED-IT* model using substance-specific parameters:

``` r
# Example GUTS-RED-IT scenario derived from an acute fish toxicity study
# of the fathead minnow and Chlorpyrifos (Geiger et al. 1988)
minnow_it %>%
  simulate()
#>   time         D H         S
#> 1    0 0.0000000 0 1.0000000
#> 2    1 0.7075891 0 0.9999854
#> 3    2 0.9144958 0 0.9999558
#> 4    3 0.9749972 0 0.9999417
#> 5    4 0.9926889 0 0.9999370
```

The table returned by `simulate()` contains the value of each state
variable at the requested output time points. In case of the
*GUTS-RED-IT* model, this comprises the state variables `D` (scaled
damage) and `H` (cumulative hazard). Effect levels, effect profiles, and
dose-response graphs can be derived in a similar manner. Please refer to
EFSA PPR Panel (2018) or the package help for more information on
*GUTS-RED* type models:

``` r
# Access the package help on GUTS-RED type models
?"GUTS-RED-models"
```

## Setting up a scenario

A scenario is created by first calling a constructor function that is
dedicated to a particular model. The *cvasi* package supports multiple
TKTD models, for instance:

- Reduced General Unified Threshold models of Survival (*GUTS-RED*)
  - `GUTS_RED_SD()`
  - `GUTS_RED_IT()`
- Macrophyte models
  - `Lemna_Schmitt()`, published by Schmitt *et al.* (2013)
  - `Lemna_SETAC()`, published by Klein *et al.* (2021)
  - `Myrio()`, an adaption of Klein *et al.* (2021) to *Myriophyllum* at
    Tier 2C
- Algae models
  - `Algae_Weber()`, published by Weber *et al.* (2012)
  - `Algae_TKTD()`, based on Weber *et al.* (2012), but with scaled
    damage
  - `Algae_Simple()`, simplified growth model without external forcing
    variables
- Dynamic Energy Budget (*DEB*) models
  - `DEB_abj()`, the *abj* model with type M acceleration
  - `DEBtox()`, as implemented in the *DEBtox2019* module of
    [*BYOM*](https://www.debtox.info/byom.html)

The listed functions create objects that will carry all data and
settings to fully describe a scenario. In case of a *GUTS-RED-IT*
scenario, one needs to provide at least the four model parameters and an
exposure time-series to fully define a scenario:

``` r
# Define an exposure time-series
myexposure <- data.frame(time=c(0, 1, 1.01, 5),
                         conc=c(10, 10, 0, 0))

# Create and parameterize a scenario
GUTS_RED_IT() %>%
  set_param(c(kd=1.2, hb=0, alpha=9.2, beta=4.3)) %>%
  set_exposure(myexposure) -> myscenario
```

One can examine the main scenario properties by passing the scenario to
the console like in the next example. The console will print the model
name, model parameters (*param*), initial state (*init*), enabled effect
endpoints (*endpt*), output times (*times*), environmental forcings
(*forcs*), and exposure series (*expsr*). Depending on model type, the
scenario may carry additional properties that are not necessarily
displayed in the console output.

``` r
# Print information about the scenario
myscenario
#> 'GUTS-RED-IT' scenario
#> param: kd=1.2, hb=0, alpha=9.2, beta=4.3
#> init : D=0, H=0
#> endpt: L
#> times: [0,5] n=4, regular
#> forcs: none
#> expsr: none
#>   time conc
#> 1 0.00   10
#> 2 1.00   10
#> 3 1.01    0
#> 4 5.00    0
```

Several `set_*()` verbs are available to modify scenario properties,
such as:

- `set_tag()`, to assign a custom tag to identify the scenario
- `set_init()`, to set the initial state
- `set_param()`, to modify model parameters
- `set_times()`, to define output time points
- `set_forcings()`, to set environmental factors (i.e. *forcings*)
- `set_exposure()`, to set an exposure time-series
- `set_endpoints()`, to enable/disable assessment endpoints
- `set_transfer()`, to define transfer intervals of *Lemna* fronds
- `set_moa()`, to select a model’s mode of action
- `set_window()`, to control moving exposure windows

Please note that `set_exposure()` will, by default, use the exposure
series’ time points also as output time points. If this behavior is
undesired, it can be disabled by setting the argument
`reset_times=FALSE`:

``` r
# Update the exposure time-series but keep former output time points
myscenario %>%
  set_exposure(no_exposure(), reset_times=FALSE)
#> 'GUTS-RED-IT' scenario
#> param: kd=1.2, hb=0, alpha=9.2, beta=4.3
#> init : D=0, H=0
#> endpt: L
#> times: [0,5] n=4, regular
#> forcs: none
#> expsr: no exposure
#>   time conc
#> 1    0    0
```

Note that `times` still has the original four elements although the
assigned exposure series has only a single entry. Output time points can
be explicitly set with `set_times()`. For deriving effect endpoints, the
simulated time frame can be split up into moving exposure windows of a
certain length using `set_window()`. A more complex scenario setup may
look like the following workflow:

``` r
# Selected model parameters
myparam <- c(p_M=3211, v=0.023, k_J=0.63)
# Initial body length L
myinit <- c(L=0.02)
# Constant non-zero exposure
myexposure <- data.frame(time=0, conc=1.72)

DEB_abj() %>%
  set_param(myparam) %>%
  set_init(myinit) %>%
  set_exposure(myexposure) %>%
  set_times(0:10) %>%             # Output times 0,1,2,...,10
  set_mode_of_action(4) %>%       # Method of Action #4 to be activated
  set_window(length=3)            # Using moving exposure windows of length 3 days
#> 'DEB_abj' scenario
#> param: p_M=3211, v=0.023, k_J=0.63, MoA=4
#> init : L=0.02, E=0, H=0, R=0, cV=0, Lmax=0
#> endpt: L, R
#> times: [0,10] n=11, regular
#> forcs: none
#> expsr: none
#>   time conc
#> 1    0 1.72
```

## Running a simulation

After a scenario is fully set up, it can be passed to `simulate()` to
return model results:

``` r
# Example scenario of the Lemna TKTD model
metsulfuron %>%
  set_times(0:7) %>%
  simulate()
#>   time       BM E    M_int     C_int  FrondNo
#> 1    0 50.00000 1   0.0000 0.0000000 500000.0
#> 2    1 52.15858 1 223.0745 0.2560989 521585.8
#> 3    2 52.43495 1 369.6344 0.4221192 524349.5
#> 4    3 52.33917 1 463.0261 0.5297392 523391.7
#> 5    4 52.17923 1 521.8927 0.5989176 521792.3
#> 6    5 52.00016 1 558.6268 0.6432808 520001.6
#> 7    6 51.81403 1 581.2187 0.6717006 518140.3
#> 8    7 51.63740 1 474.0716 0.5497473 516374.0
```

`simulate()` returns the model state for each output time point. In this
example, it returns results for the period from zero to seven with an
equi-distant time step length of one. The temporal resolution of results
can be increased by specifying additional output times:

``` r
# Same simulation period, but with a smaller step length of 0.1
metsulfuron %>%
  set_times(seq(0, 7, 0.1)) %>%
  simulate() %>%
  tail()
#>    time       BM E    M_int     C_int  FrondNo
#> 66  6.5 51.72111 1 556.4478 0.6442288 517211.1
#> 67  6.6 51.70319 1 544.1623 0.6302236 517031.9
#> 68  6.7 51.68570 1 529.6855 0.6136648 516857.0
#> 69  6.8 51.66877 1 513.1159 0.5946630 516687.7
#> 70  6.9 51.65259 1 494.5480 0.5733237 516525.9
#> 71  7.0 51.63740 1 474.0716 0.5497473 516374.0
```

The resulting table now contains ten times as many rows because we
decreased the step length by a factor of ten but simulated the same
period. The temporal resolution of simulations can have an influence on
results, i.e. the numerical values of the returned variables. These
differences usually originate from small numerical errors introduced by
the solver of the model’s Ordinary Differential Equations. Generally,
parameters such as the step-length in time can have influence on the
precision of simulation results.

There are several ways to increase the precision of simulation results
without modifying a scenario’s output times. One option is to decrease
the solver’s maximum step length in time by setting the optional
argument `hmax`. The smaller `hmax`, the more precise the results:

``` r
# Original simulation period, but with a maximum solver step length of hmax=0.01
metsulfuron %>%
  set_times(0:7) %>%
  simulate(hmax=0.01)
#>   time       BM E    M_int     C_int  FrondNo
#> 1    0 50.00000 1   0.0000 0.0000000 500000.0
#> 2    1 52.15858 1 223.0745 0.2560989 521585.8
#> 3    2 52.43495 1 369.6344 0.4221192 524349.5
#> 4    3 52.33917 1 463.0261 0.5297392 523391.7
#> 5    4 52.17923 1 521.8927 0.5989176 521792.3
#> 6    5 52.00016 1 558.6268 0.6432808 520001.6
#> 7    6 51.81403 1 581.2187 0.6717006 518140.3
#> 8    7 51.63740 1 474.0716 0.5497473 516374.0
```

Although very effective, reducing `hmax` to small values may lead to
inefficiently long runtimes of simulations. If numerical precision is an
issue, it is advisable to also test other solver settings such as
absolute and relative error tolerances (`atol` and `rtol`) or to switch
to another solver method. Numerical precision and stability is a complex
and advanced topic, please refer to the manual of `simulate()` for
additional information:

``` r
?cvasi::simulate
```

## Deriving assessment endpoints

Simulation results are the basis for deriving assessment endpoints such
as effect levels and effect profiles. The *cvasi* package implements
routines to derive these endpoints efficiently independent of the actual
model that is being assessed. Hence, if a model is available within the
package framework, it can be used to derive assessment endpoints. As an
example, the following statement derives effect levels for a sample
*GUTS-RED-IT* scenario:

``` r
# GUTS-RED-IT scenario of the fathead minnow and chlorpyrifos
minnow_it %>% effect()
#> # A tibble: 1 × 4
#>   scenario           L L.dat.start L.dat.end
#>   <list>         <dbl>       <dbl>     <dbl>
#> 1 <GutsRdIt> 0.0000630           0         4
```

The lethality endpoint `L` has a value of `6.3e-5` which means that
lethality has only marginally increased by 0.0063% compared to a control
scenario without exposure to the contaminant. The columns `L.dat.start`
and `L.dat.end` denote the period for which the effect level was
observed. In case of *GUTS-RED* models, this usually refers to the whole
simulation period. For certain assessments it may be necessary to
evaluate all exposure periods of a certain length within the original
series separately. This approach is referred to as a *moving exposure
window*:

``` r
# Setting up a custom scenario with a total simulation period of 14 days and
# an exposure window length of 7 to assess a trapezoidal exposure pattern
americamysis %>%
  set_window(7) %>%
  set_exposure(data.frame(t=c(0,3,4,7,8), c=c(0,0,3,3,0))) %>%
  set_times(0:14) -> mydeb

# Derive maximum effect level of all exposure windows
mydeb %>% effect()
#> # A tibble: 1 × 7
#>   scenario      L L.dat.start L.dat.end     R R.dat.start R.dat.end
#>   <list>    <dbl>       <dbl>     <dbl> <dbl>       <dbl>     <dbl>
#> 1 <DebAbj> 0.0521           2         9     0           0         7
```

The `americamysis` scenario provides two effect endpoints: structural
length (`L`) and reproduction (`R`). Exposure to the simulated toxicant
results at maximum in a 5.21% decrease in body length compared to
control scenarios. The maximum effect was observed in the exposure
window starting at time `2.0` and ending at `9.0`, cf. columns
`L.dat.start` and `L.dat.end`. The toxicant had no effect on
reproduction due to the simulation being too short for reproduction to
occur.

Scenarios can also be assessed regarding to so-called effect profiles,
or *EPx* values for short. An *EPx* value quantifies the multiplication
factor that is necessary to produce an effect level of x% for that
particular scenario. As an example, an *EP50* of `17.0` would mean that
the scenario’s exposure series needs to be multiplied by a factor of 17
to observe a 50% effect. The function `epx()` can derive effect profiles
efficiently with arbitrary precision:

``` r
# Restrict assessed endpoints to structural length (L)
mydeb %>% 
  set_endpoints("L") %>%
  epx()
#> # A tibble: 1 × 3
#>   scenario L.EP10 L.EP50
#>   <list>    <dbl>  <dbl>
#> 1 <DebAbj>   1.16   1.71
```

In this example, a factor of `1.16` resulted in 10% effect on structural
length and a factor of `1.71` in 50% effect. `epx()` provides means to
control the precision of derived *EPx* values, the range of tested
multiplication factors, factor cutoff thresholds, and debugging
capabilities. To examine how a particular *EPx* value was derived,
simply set the argument `verbose=TRUE`:

``` r
# Examine how the EP20 value is derived
minnow_it %>% epx(level=20, verbose=TRUE)
#> epx: screening multiplication factors
#>   start: 10
#>   L.EP20: 1 <<<
#>   L.EP20: 5.5
#>   L.EP20: 7.75
#>   L.EP20: 6.625
#>   L.EP20: 7.1875
#>   L.EP20: 6.90625
#>   L.EP20: 6.765625
#>   L.EP20: 6.8359375
#>   L.EP20: 6.80078125
#> # A tibble: 1 × 2
#>   scenario   L.EP20
#>   <list>      <dbl>
#> 1 <GutsRdIt>   6.80
```

In this example, the algorithm of `epx()` first tests a multiplication
factor of `10` and then continues testing additional factors using a
binary search algorithm until a factor is found that causes 20% effect.

**Please note**: Depending on model and chosen parameters, it may be
impossible to calculate *EPx* values (for selected effect levels),
because the model or organism cannot reach the required state. As an
example: If the simulated period of a *DEB_abj* scenario is too short,
reproduction cannot occur. Therefore, effects on reproduction will
always be zero independent of the exposure level. Consequently, *EPx*
values for the reproduction endpoint will not be available.

# Data format of model inputs

Input data for the *cvasi* package relies on common *R* data structures
and imposes only minimal requirements to set up scenarios:

1.  **Data representing numerical values should be represented by
    numerical types**. Practically, this means that numerical scenario
    properties such as output time points must not be passed as string
    literals even if the string may be representing a number. Due to *R*
    being a dynamically typed language, parameter values and their types
    may have to be checked on import by the user.
2.  **Time-series data must be represented by a `data.frame` with
    exactly two numerical columns**: The first column for time, the
    second for the series’ value. The column names are irrelevant but
    sensible names may help documenting the data. Constant values can be
    represented by time-series with only a single row or entry.

Therefore, it should be possible to apply the package’s routines on
existing datasets with minimal effort. Please refer to Wickham and
Grolemund (2017) on how to implement plain and consistent processes to
import and transform data using *tidyr* syntax.

*cvasi* does support loading data from common sources, such as the
exposure model *TOXSWA*, to ease data processing. Notable file formats
and data structures that are supported are exposure time-series from the
model FOCUS *TOXSWA*, as well as fitted *GUTS-RED* model parameter sets
from [*morse*](https://cran.r-project.org/package=morse).

# How to derive model inputs

In general, all models require parameter values specific to the
simulated species and substance. Some scenario types contain default
values for selected model parameters as recommended by model authors.
Scenarios providing default values include:

- `Lemna_Schmitt()` and derived models are parameterized according to
  Schmitt *et al.* (2013)
- `Lemna_SETAC()`, `Myrio()`, and derived models are parameterized as
  recommended by Klein *et al.* (2021)

In addition, some models require values for environmental factors such
as temperature and nutrient concentrations. These values or time-series
are specific to a certain scenario and must be chosen accordingly.

Parameters of TKTD sub-models are substance-specific and are usually
obtained by fitting model parameters to observed data. The *cvasi*
package supports this process with the function `calibrate()` which uses
a frequentist approach for parameter fitting. However, parameters can
also be derived by other means. Generally, the user is responsible for
choosing model parameters and the package does not impose any
restrictions in this regard.

The package function `calibrate()` supports the following operation
modes:

- Fitting of a single parameter or multiple parameters simultaneously
- Fit to one or more independent datasets
- Weighting of datasets
- Choosing the error function which is minimized
- Selecting one of multiple optimization algorithms

Parameter fitting builds upon the *R* function `stats::optim()` and it
exposes full control over the fitting process. In this context, a
dataset is represented by a *calibration set* object which consists of a
scenario, observed data, and an optional weighting term. Please refer to
the [Modeling Howto](cvasi-2-howto.html) for a description of the
fitting process using `calibrate()`.

As an alternative for *GUTS-RED* type models, model parameters can also
be obtained using the [*morse*
package](https://cran.r-project.org/package=morse). It uses a Bayesian
approach for parameter fitting.

# Description of model outputs

The *cvasi* package does not use or define any custom file formats for
model outputs but strictly relies on common *R* data structures.
Simulation results and assessment endpoints are returned in tabular
formats which can easily be serialized to common file formats such as
`.csv` or *Microsoft Excel* files for further processing and data
storage.

The following subsections will describe in detail the return values of
common package routines for simulating and assessing scenarios.

## Simulation results

The return value of `simulate()` is a `data.frame` containing the value
of each state variable at each requested output time. The first column
represents *time*, followed by columns for each state variable:

``` r
## Display selected scenario properties
metsulfuron@times  # Output times
#>  [1]  0  1  2  3  4  5  6  7  8  9 10 11 12 13 14
metsulfuron@init   # Initial state
#>    BM     E M_int 
#>    50     1     0

## Simulate the sample scenario
metsulfuron %>% simulate()
#>    time       BM E     M_int      C_int  FrondNo
#> 1     0 50.00000 1   0.00000 0.00000000 500000.0
#> 2     1 52.15858 1 223.07447 0.25609887 521585.8
#> 3     2 52.43495 1 369.63440 0.42211917 524349.5
#> 4     3 52.33917 1 463.02605 0.52973924 523391.7
#> 5     4 52.17923 1 521.89269 0.59891761 521792.3
#> 6     5 52.00016 1 558.62681 0.64328081 520001.6
#> 7     6 51.81403 1 581.21867 0.67170059 518140.3
#> 8     7 51.63740 1 474.07158 0.54974731 516374.0
#> 9     8 51.61806 1 301.84519 0.35015964 516180.6
#> 10    9 52.32264 1 192.18727 0.21994721 523226.4
#> 11   10 54.34850 1 122.36718 0.13482205 543485.0
#> 12   11 57.08426 1  77.91217 0.08172830 570842.6
#> 13   12 59.99459 1  49.60731 0.04951275 599945.9
#> 14   13 62.96113 1  31.58537 0.03003980 629611.3
#> 15   14 65.95879 1  20.11066 0.01825733 659587.9
```

The model state in the first row is, by definition, equal to the initial
state of the scenario. The simulation result may contain additional
columns that are derived variables or intermediate model variables. In
case of the `metsulfuron` scenario, the internal toxicant concentration
(`C_int`) was derived from `BM`, `M_int`, and model parameters. The
number of fronds (`FrondNo`) was derived from `BM` and model parameters.
Additional outputs such as `C_int` and `FrondNo` are included in results
for user convenience and to support model understanding. Additional
outputs can be disabled by setting the optional argument `nout=0`:

``` r
metsulfuron %>%
  simulate(nout=0) %>%
  head(5)
#>   time       BM E    M_int
#> 1    0 50.00000 1   0.0000
#> 2    1 52.15858 1 223.0745
#> 3    2 52.43495 1 369.6344
#> 4    3 52.33917 1 463.0261
#> 5    4 52.17923 1 521.8927
```

Intermediate model variables can be enabled in the same manner:

``` r
metsulfuron %>%
  simulate(nout=8) %>%
  head(5)
#>   time       BM E    M_int     C_int  FrondNo   C_int_u BM_fresh k_phot_eff k_resp_eff     f_Eff P_up_eff
#> 1    0 50.00000 1   0.0000 0.0000000 500000.0 0.0000000 835.0000 0.07548561 0.02030631 1.0000000   0.0054
#> 2    1 52.15858 1 223.0745 0.2560989 521585.8 0.3414652 871.0482 0.03746128 0.02030631 0.5049205   0.0054
#> 3    2 52.43495 1 369.6344 0.4221192 524349.5 0.5628256 875.6636 0.01993781 0.02030631 0.2693322   0.0054
#> 4    3 52.33917 1 463.0261 0.5297392 523391.7 0.7063190 874.0641 0.01760496 0.02030631 0.2376344   0.0054
#> 5    4 52.17923 1 521.8927 0.5989176 521792.3 0.7985568 871.3931 0.01699676 0.02030631 0.2291285   0.0054
```

## Effect levels

The return value of `effect()` is a `data.frame` containing the effect
level of each selected endpoint. The first column, `scenario`, contains
the original scenario which was the basis of the assessment. Followed by
the effect level and the exposure window in which the maximum effect was
observed:

``` r
metsulfuron %>%
  set_times(0:7) %>%  # restrict scenario to the period [0,7]
  effect()
#> # A tibble: 1 × 7
#>   scenario      BM BM.dat.start BM.dat.end     r r.dat.start r.dat.end
#>   <list>     <dbl>        <dbl>      <dbl> <dbl>       <dbl>     <dbl>
#> 1 <LmnSchmS> 0.268            0          7 0.906           0         7
```

Effect levels are reported as a fraction relative to a control scenario.
Effects can be converted to percentages by multiplying the value with
`100` (%). For scenarios with moving exposure windows disabled, the
reported window will cover the entire simulation period. If multiple
windows result in identical maximum effect levels, the first matching
window will be reported. The table’s content can be reduced to endpoints
only by setting the argument `ep_only=TRUE`.

`effect()` can also be instructed to return the effect levels of all
assessed exposure windows by setting the argument `max_only=FALSE`:

``` r
metsulfuron %>%
  set_window(length=7, interval=1) %>%   # enable moving exposure windows
  effect(max_only=FALSE)                 # return effects of all windows
#> # A tibble: 8 × 5
#>   scenario          BM         r dat.start dat.end
#>   <list>         <dbl>     <dbl>     <dbl>   <dbl>
#> 1 <LmnSchmS>  2.68e- 1  9.06e- 1         0       7
#> 2 <LmnSchmS>  2.65e- 1  8.95e- 1         1       8
#> 3 <LmnSchmS>  2.48e- 1  8.29e- 1         2       9
#> 4 <LmnSchmS>  2.05e- 1  6.69e- 1         3      10
#> 5 <LmnSchmS>  1.47e- 1  4.63e- 1         4      11
#> 6 <LmnSchmS>  7.39e- 2  2.23e- 1         5      12
#> 7 <LmnSchmS>  3.15e- 3  9.18e- 3         6      13
#> 8 <LmnSchmS> -6.66e-16 -1.78e-15         7      14
```

In few cases, `effect()` may report spurious non-zero effect levels that
are very close to zero. These can either be avoided by increasing the
numerical precision of simulations or by setting a marginal effect
threshold which instructs `effect()` to zero any effect below that
threshold. A warning will be shown if a marginal effect threshold
greater than 1% is chosen. Applying a threshold of 1% to the previous
example results in zeroing the effect in the last and second to last
exposure window but leaving the remaining results unchanged:

``` r
metsulfuron %>%
  set_window(length=7, interval=1) %>%         # enable moving exposure windows
  effect(max_only=FALSE, marginal_effect=0.01) # return effects of all windows
#> # A tibble: 8 × 5
#>   scenario       BM     r dat.start dat.end
#>   <list>      <dbl> <dbl>     <dbl>   <dbl>
#> 1 <LmnSchmS> 0.268  0.906         0       7
#> 2 <LmnSchmS> 0.265  0.895         1       8
#> 3 <LmnSchmS> 0.248  0.829         2       9
#> 4 <LmnSchmS> 0.205  0.669         3      10
#> 5 <LmnSchmS> 0.147  0.463         4      11
#> 6 <LmnSchmS> 0.0739 0.223         5      12
#> 7 <LmnSchmS> 0      0             6      13
#> 8 <LmnSchmS> 0      0             7      14
```

## Effect profiles

The return value of `epx()` is a `data.frame` containing the effect
profile (*EPx*) of each selected endpoint and effect level. The first
column, `scenario`, contains the original scenario which was the basis
of the assessment. Followed by the derived effect profiles, i.e. the
multiplication factors to achieve x% effect:

``` r
metsulfuron %>% epx()
#> Warning in epx(.): Some scenarios have failed (n=1), result will include NAs
#>   **  multiplication factor out of range: BM.EP50 > 1e+30
#> # A tibble: 1 × 6
#>   scenario   BM.EP10 r.EP10 BM.EP50 r.EP50 error                                                 
#>   <list>       <dbl>  <dbl>   <dbl>  <dbl> <chr>                                                 
#> 1 <LmnSchmS>   0.395  0.325      NA  0.880 " multiplication factor out of range: BM.EP50 > 1e+30"
```

For the sample `metsulfuron` scenario, a factor of `0.395` would need to
be applied to the exposure series to achieve an effect level of 10% on
biomass (*BM*). An effect level of 50% on biomass is unattainable with
the tested factors ranging from `1e-30` to `1e30` which is demonstrated
by the warning message and the `NA` value for `BM.EP50`.

The table’s content can be reduced to endpoints only by setting the
argument `ep_only=TRUE`. The range of tested factors can be controlled
by setting the arguments `min_factor` and `max_factor`. Multiplication
factors exceeding this range will raise a warning and `NA` will be
returned for that *EPx*. Finding multiplication factors can be cut short
if a certain margin of safety is reached by using the argument
`factor_cutoff`: if the algorithm identifies that an *EPx* greater or
equal to `factor_cutoff` is required, then `factor_cutoff` is reported
as the result. This approach may be used to save computational resources
for long running models if the exact *EPx* value above the cutoff is not
of interest.

The precision of *EPx* values is controlled by the argument
`effect_tolerance` and is given as the upper absolute error threshold of
effects that is deemed acceptable. The default value of `0.001` ensures
that a derived *EPx* will result in an effect of x% ± 0.1. Decreasing
the `effect_tolerance` will result in additional model iterations and
longer runtime.

The algorithm’s inner workings can be examined by setting the argument
`verbose=TRUE`:

``` r
metsulfuron %>%
  set_endpoints("BM") %>%
  epx(verbose=TRUE)
#> epx: screening multiplication factors
#>   start: 10
#>   BM.EP10: 1 <<<
#>   BM.EP10: 0.1 <<<
#>   BM.EP10: 0.55
#>   BM.EP10: 0.325
#>   BM.EP10: 0.4375
#>   BM.EP10: 0.38125
#>   BM.EP10: 0.409375
#>   BM.EP10: 0.3953125
#>   BM.EP50: 100 >>>
#>   BM.EP50: 1000 >>>
#>   BM.EP50: 10000 >>>
#>   BM.EP50: 1e+05 >>>
#>   BM.EP50: 1e+06 >>>
#>   BM.EP50: 1e+07 >>>
#>   BM.EP50: 1e+08 >>>
#>   BM.EP50: 1e+09 >>>
#>   BM.EP50: 1e+10 >>>
#>   BM.EP50: 1e+11 >>>
#>   BM.EP50: 1e+12 >>>
#>   BM.EP50: 1e+13 >>>
#>   BM.EP50: 1e+14 >>>
#>   BM.EP50: 1e+15 >>>
#>   BM.EP50: 1e+16 >>>
#>   BM.EP50: 1e+17 >>>
#>   BM.EP50: 1e+18 >>>
#>   BM.EP50: 1e+19 >>>
#>   BM.EP50: 1e+20 >>>
#>   BM.EP50: 1e+21 >>>
#>   BM.EP50: 1e+22 >>>
#>   BM.EP50: 1e+23 >>>
#>   BM.EP50: 1e+24 >>>
#>   BM.EP50: 1e+25 >>>
#>   BM.EP50: 1e+26 >>>
#>   BM.EP50: 1e+27 >>>
#>   BM.EP50: 1e+28 >>>
#>   BM.EP50: 1e+29 >>>
#>   BM.EP50: 1e+30 >>>
#>   BM.EP50: 1e+31 >>>
#> Warning in epx(., verbose = TRUE): Some scenarios have failed (n=1), result will include NAs
#>   **  multiplication factor out of range: BM.EP50 > 1e+30
#> # A tibble: 1 × 4
#>   scenario   BM.EP10 BM.EP50 error                                                 
#>   <list>       <dbl>   <dbl> <chr>                                                 
#> 1 <LmnSchmS>   0.395      NA " multiplication factor out of range: BM.EP50 > 1e+30"
```

In this example, the binary search finds a `BM.EP10` with acceptable
precision in less than ten iterations. For the `BM.EP50`, the algorithm
re-uses the knowledge gained from previously performed calculations but
is ultimately unable to find a suitable multiplication factor within the
allowed range of multiplication factors.

# References

- EFSA PPR Panel (EFSA Panel on Plant Protection Products and their
  Residues), Ockleford C., Adriaanse P., Berny P., et al., 2018:
  *Scientific Opinion on the state of the art of
  Toxicokinetic/Toxicodynamic (TKTD) effect models for regulatory risk
  assessment of pesticides for aquatic organisms*. EFSA Journal 2018;
  16(8):5377, 188 pp. DOI:
  [10.2903/j.efsa.2018.5377](https://doi.org/10.2903/j.efsa.2018.5377)
- Geiger D.L., Call D.J., and Brooke L.T., 1988: *Acute toxicities of
  organic chemicals to fathead minnows (Pimephales promelas): Volume
  IV*, pp. 195-197. University of Wisconsin-Superior, Center for Lake
  Superior Environmental Studies. ISBN 9780961496838.
- Klein J., Cedergreen N., Heine S., Reichenberger S., Rendal C.,
  Schmitt W., Hommen U., 2021: *Refined description of the Lemna TKTD
  growth model based on Schmitt et al. (2013) - equation system and
  default parameters*. Report of the working group *Lemna* of the SETAC
  Europe Interest Group Effect Modeling. Version 1, uploaded on 22.
  Sept. 2021. <https://www.setac.org/group/effect-modeling.html>
- Schmitt W., Bruns E., Dollinger M., and Sowig P., 2013: *Mechanistic
  TK/TD-model simulating the effect of growth inhibitors on Lemna
  populations*. Ecol Model 255, pp. 1-10. DOI:
  [10.1016/j.ecolmodel.2013.01.017](https://doi.org/10.1016/j.ecolmodel.2013.01.017)
- Wickham H. and Grolemund G., 2017: *R for Data Science: Import, Tidy,
  Transform, Visualize, and Model Data* (1st ed.). O’Reilly Media.
  <https://r4ds.had.co.nz>
