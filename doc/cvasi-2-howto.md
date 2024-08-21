Modeling Howto
================
Nils Kehrein
03 May, 2024

- [How to access scenario
  properties](#how-to-access-scenario-properties)
- [Using *tidyr* syntax](#using-tidyr-syntax)
- [Predictions](#predictions)
- [Moving exposure windows](#moving-exposure-windows)
- [Simulating biomass transfers](#simulating-biomass-transfers)
- [Fitting model parameters](#fitting-model-parameters)
- [Changes in parameter values over
  time](#changes-in-parameter-values-over-time)
- [Decrease assessment runtime](#decrease-assessment-runtime)
- [Implementing custom models](#implementing-custom-models)
- [Complete working example](#complete-working-example)
  - [Setting up a scenario](#setting-up-a-scenario)
  - [Simulating a scenario and
    plotting](#simulating-a-scenario-and-plotting)

This Howto provides instructions on how to address certain modeling
challenges and offers additional details and context for certain
features of the package. A final section provides a complete worked out
example. For a more general overview, please refer to the
[manual](cvasi-1-manual.html).

## How to access scenario properties

The package provides a number of `set*()` functions to modify scenario
properties such as `set_param()` and `set_times()`. Analogous `set*`
functions are not provided because, generally, it should not be
necessary to retrieve data from scenarios. However, it is possible (but
not recommended) to access all scenario settings via objects *slots*. A
*slot* is the name for an object attribute in *R*. The slots of an
object can be accessed by using the `@` operator. It behaves similar to
the `$` operator on named lists:

``` r
# Create a new scenario object
myscenario <- Lemna_Schmitt()

# Get model name
get_model(myscenario)
#> [1] "Lemna_Schmitt"

# Set a custom tag to identify the scenario
myscenario %>%
  set_tag("Lab experiment #1") -> myscenario

# Get custom tag
get_tag(myscenario)
#> [1] "Lab experiment #1"

# The tag is also displayed when printing scenario properties
myscenario
#> 'Lemna_Schmitt' scenario
#> tag  : Lab experiment #1
#> param: Emax=1, AperBM=1000, Kbm=1, P_Temp=0, MolWeight=390.4, k_phot_fix=1, k_phot_max=0.47, k_resp=0.05, k_loss=0, Tmin=8, Tmax=40.5, Topt=26.7, t_ref=25, Q10=2, k_0=3, a_k=5e-05, C_P=0.3, CP50=0.0043, a_P=1, KiP=101, C_N=0.6, CN50=0.034, a_N=1, KiN=604, BM50=176, mass_per_frond=1e-04, BMw2BMd=16.7
#> init : BM=0, E=1, M_int=0
#> endpt: BM, r
#> times: none
#> forcs: none
#> expsr: none
#> >> exposure series is empty

# Accessing scenario slots and their default values
myscenario@forcings.req # forcings required for effect calculations
#> [1] "temp" "rad"
myscenario@endpoints    # available effect endpoints
#> [1] "BM" "r"
myscenario@control.req  # are control runs required for effect calculation?
#> [1] TRUE
```

The previous example displays some of the default values of a
*Lemna_Schmitt* scenario. The set of available slots depends on the
model type and is documented in the package help. For instance, scenario
properties shared by all models are documented in the effect scenario
class:

``` r
# Call the help page of effect scenarios class
?scenarios
```

A scenario class inherits all slots from its ancestors. A notable class
which modifies simulation behavior and provides additional scenario
properties is `Transferable`: it provides capabilities to consider
biomass transfers at defined time points during simulation. Details
about its class slots and functionality are described in the help pages.

``` r
# Call the help page of the biomass transfer class
?Transferable
```

## Using *tidyr* syntax

The *tidyr* syntax, popularized by the *tidyverse* packages in R,
provides a coherent and efficient approach to data manipulation and
analysis. The *tidyverse*, which includes the widely used *dplyr* and
*ggplot2* packages, follows a standardized grammar that makes code more
readable and intuitive. *tidyr* syntax emphasizes the use of functions
with clear and descriptive names. This makes it easier for users to
understand and reproduce analyses. The `%>%` (pipe) operator is a key
component of *tidyr* syntax and enables fluent and expressive
concatenation of operations. Overall, the introduction of *tidyr* syntax
improves code readability, reproducibility, and collaboration, resulting
in maintainable data analysis pipelines.

In brief, the advantages of *tidyr* syntax are:

- a series of statements can be combined to an intuitive workflow using
  the pipeline (`%>%`) operator
- a short cut for the pipe (`%>%`) operator is Ctrl+Shift+M
- pipelines reduce the need for intermediary variables (but thoughtful
  intermediates are recommended)
- *tidy* verbs generally take `list` data types as input and return
  these also as output
- some verbs enrich outputs with additional information, effectively
  extending the output data to a table

``` r
# The example scenario `metsulfuron` based on the Lemna model by Schmitt et al. (2013)
# is modified by setting a new exposure series and initial state. Then, it is
# simulated.
  metsulfuron %>%
    set_noexposure() %>%  # set no exposure (i.e., a control run)
    set_init(c(BM = 50)) %>%  # set initial biomass
    simulate()
#>    time       BM E M_int C_int  FrondNo
#> 1     0 50.00000 1     0     0 500000.0
#> 2     1 52.79250 1     0     0 527925.0
#> 3     2 55.64673 1     0     0 556467.3
#> 4     3 58.55411 1     0     0 585541.1
#> 5     4 61.50533 1     0     0 615053.3
#> 6     5 64.49048 1     0     0 644904.8
#> 7     6 67.49918 1     0     0 674991.8
#> 8     7 70.52071 1     0     0 705207.1
#> 9     8 73.54416 1     0     0 735441.6
#> 10    9 76.55861 1     0     0 765586.1
#> 11   10 79.55324 1     0     0 795532.4
#> 12   11 82.51755 1     0     0 825175.5
#> 13   12 85.44144 1     0     0 854414.4
#> 14   13 88.31537 1     0     0 883153.7
#> 15   14 91.13051 1     0     0 911305.1
```

## Predictions

TKTD models have both species and substance specificity. If risks are
identified at Tier 1 (standard test species approach) and exposure
duration is expected to be shorter than in standard tests, the simplest
solution is to develop TKTD models for standard test species. However,
if Tier 2A (geometric mean/evidence weighting approach) or Tier 2B
(species sensitivity distribution approach) information is available, it
may be more appropriate to develop TKTD models for a wider range of
species to increase the accuracy of the risk assessment. Validated TKTD
models for these different species can be used as an alternative to
assess specific risks using available field exposure profiles. This
includes the calculation of exposure profile specific LPx/EPx values
(multiplication factor for a specific overall exposure profile causing x
% lethality or effect) based on an appropriate aquatic exposure
assessment.

``` r
# Initialize the random numbers generator
set.seed(123)
# Generating a random exposure series spanning 14 days
random_conc <- runif(15, min=0, max=0.1)
exposure_profile <- data.frame(time=0:14, conc=random_conc)
# Run EPx calculations
minnow_it %>%
  set_exposure(exposure_profile) %>%  # set a specific exposure scenario
  epx()  # run EPx calculations
#> # A tibble: 1 × 3
#>   scenario   L.EP10 L.EP50
#>   <list>      <dbl>  <dbl>
#> 1 <GutsRdIt>   67.0   111.
```

## Moving exposure windows

A moving time window is a computing technique used in data analysis.
Data is systematically analysed within a window of fixed length that
moves or slides through the data set, in our case an exposure
time-series. The window captures a subset of successive data points, and
as it moves through the data; simulations and effect calculations are
performed for each window.

`effect()` can report effect levels for all evaluated exposure windows
on demand:

``` r
# Derive effect levels of all exposure windows
metsulfuron %>% 
  set_window(length=7, interval=1) %>%
  effect(max_only=FALSE)
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

The resulting table describes how effect levels change when the exposure
window moves along the exposure series. It is also possible to specify
the marginal effect threshold of reported results (this prevents
overinterpretation of spurious effect levels originating from marginal
numerical errors introduced during simulation), as in the following
example:

``` r
# Only report effect levels larger than 1e-5 = 0.001%
metsulfuron %>% 
  set_window(length=7, interval=1) %>%
  effect(max_only=FALSE, marginal_effect=1e-5)
#> # A tibble: 8 × 5
#>   scenario        BM       r dat.start dat.end
#>   <list>       <dbl>   <dbl>     <dbl>   <dbl>
#> 1 <LmnSchmS> 0.268   0.906           0       7
#> 2 <LmnSchmS> 0.265   0.895           1       8
#> 3 <LmnSchmS> 0.248   0.829           2       9
#> 4 <LmnSchmS> 0.205   0.669           3      10
#> 5 <LmnSchmS> 0.147   0.463           4      11
#> 6 <LmnSchmS> 0.0739  0.223           5      12
#> 7 <LmnSchmS> 0.00315 0.00918         6      13
#> 8 <LmnSchmS> 0       0               7      14
```

The effect over all moving windows can be visualized using `ggplot`:

``` r
set.seed(123)
# Generate a random exposure series spanning 14 days
ts <- data.frame(time = 0:14,
                 conc = runif(15, min=0, max=0.1))

# Run EPx calculations for a window length of 7 days and a step size of 1 day
metsulfuron %>%
  set_exposure(ts) %>%
  set_window(length=7, interval=1) %>%
  effect(max_only=FALSE) -> results
results
#> # A tibble: 8 × 5
#>   scenario          BM        r dat.start dat.end
#>   <list>         <dbl>    <dbl>     <dbl>   <dbl>
#> 1 <LmnSchmS> 0.000113  0.000327         0       7
#> 2 <LmnSchmS> 0.000115  0.000333         1       8
#> 3 <LmnSchmS> 0.000109  0.000317         2       9
#> 4 <LmnSchmS> 0.0000971 0.000283         3      10
#> 5 <LmnSchmS> 0.000104  0.000302         4      11
#> 6 <LmnSchmS> 0.000119  0.000346         5      12
#> 7 <LmnSchmS> 0.000136  0.000394         6      13
#> 8 <LmnSchmS> 0.000116  0.000337         7      14

# Create a plot of effects over time
library(ggplot2)
ggplot(results) +
  geom_point(aes(dat.start, BM*100)) +
  labs(x="Start of window (day)", y="Effect on biomass (%)")
```

![](../doc/figures/howto-unnamed-chunk-10-1.png)<!-- --> The effect plot
shows the effect for the time point where each window starts. Effects
are not available, and therefore not plotted, for time points where the
window exceeds the simulated timeframe.

## Simulating biomass transfers

A transfer refers to an event where a certain amount of biomass (BM) is
moved to a new medium after a period of time. This feature replicates a
procedure occurring e.g. in *Lemna* effect studies and may be necessary
to recreate study results. At each transfer, a defined amount of biomass
is transferred to a new medium. This is modeled by interrupting the
simulation at a transfer time point, modifying the biomass level BM, and
scaling affected compartments according to new biomass levels. Scaling
of compartments depending on biomass, such as internal toxicant mass, is
necessary to correctly reflect mass balances and concentrations over
time.

Option 1: Regular intervals

``` r
metsulfuron %>%
  set_init(c(BM=1)) %>%
  set_noexposure() %>%
  set_transfer(interval=3, biomass=1) %>%
  simulate() -> result
result
#>    time       BM E M_int C_int  FrondNo
#> 1     0 1.000000 1     0     0 10000.00
#> 2     1 1.088182 1     0     0 10881.82
#> 3     2 1.184076 1     0     0 11840.76
#> 4     3 1.288342 1     0     0 12883.42
#> 5     4 1.088182 1     0     0 10881.82
#> 6     5 1.184076 1     0     0 11840.76
#> 7     6 1.288342 1     0     0 12883.42
#> 8     7 1.088182 1     0     0 10881.82
#> 9     8 1.184076 1     0     0 11840.76
#> 10    9 1.288342 1     0     0 12883.42
#> 11   10 1.088182 1     0     0 10881.82
#> 12   11 1.184076 1     0     0 11840.76
#> 13   12 1.288342 1     0     0 12883.42
#> 14   13 1.088182 1     0     0 10881.82
#> 15   14 1.184076 1     0     0 11840.76

library(ggplot2)
ggplot(result) +
  geom_line(aes(time, BM)) +
  labs(x="Time (days)", y="Biomass (g_dw/m2)", title="Biomass transfer every three days")
```

![](../doc/figures/howto-unnamed-chunk-11-1.png)<!-- -->

Option 2: Custom time points and custom biomass

``` r
metsulfuron %>%
  set_init(c(BM=1)) %>%
  set_noexposure() %>%
  set_transfer(times=c(3, 6), biomass=c(1, 0.5)) %>%
  simulate() -> result2

library(ggplot2)
ggplot(result2) +
  geom_line(aes(time, BM)) +
  labs(x="Time (days)", y="Biomass (g_dw/m2)", title="Biomass transfer at custom time points")
```

![](../doc/figures/howto-unnamed-chunk-12-1.png)<!-- -->

``` r
# Call the help page of set_transfer
?set_transfer
```

## Fitting model parameters

Parameters of a model can be fitted (calibrated) against observed effect
data. To fit a scenario to observed effect data:

1.  Combine the necessary inputs (model, exposure time-series, effect
    data).
2.  Fit the scenario for the given parameters and observed effect data.

For the first step, two options are available:

- When only one exposure level (e.g., one experimental treatment) and
  corresponding effect dataset (effect data only contain observations
  corresponding to one exposure scenario) are of interest, then a
  scenario consisting of model parameters and exposure time-series can
  directly be fitted to effect data using the `calibrate()` function.

- As a more complex and versatile option, scenarios and effect data are
  combined into one or more *calibration sets*. Each *calibration set*
  has exactly one scenario that describes the parameters of the
  experiment. If more than one *calibration set* is defined, scenario
  parameters can differ between set, but don’t have to, A numerical
  weight can be assigned to each *calibration set* to control its impact
  on the calculated error term and derived fitting procedure. All
  *calibration sets* are then passed to the `calibrate()` function.

The following describes an example which fits selected parameters of the
*Lemna* model by Schmitt *et al.* (2013) to observed frond numbers from
experiments with the herbicide *metsulfuron*:

Option 1: direct calibration with one scenario and one effect dataset

``` r
library(dplyr)
# No exposure in control scenario
exposure <- data.frame(time=0:14, conc=0)

# set k_phot_fix, k_resp and Emax to the defaults recommended in Klein et al. (2022)
# for Tier 2C studies. Set initial biomass to 12.0 (original data is in fronds
# rather than biomass, but for sake of simplicity, no conversion was performed).
control <- metsulfuron %>% 
  set_param(c(k_phot_fix=TRUE, k_resp=0, Emax=1)) %>% 
  set_init(c(BM=12)) %>%
  set_exposure(exposure)
# `metsulfuron` is an example scenario based on Schmitt et al. (2013) and therefore
# uses the historical, superseded nomenclature by Schmitt et al. We recommend using
# the newer SETAC Lemna model by Klein et al. (2022) for current applications,
# see `Lemna_SETAC()` for details.

# Get control data from Schmitt et al. (2013)
obs_control <- Schmitt2013 %>%
  filter(ID == "T0") %>%
  select(t, BM=obs)

# Fit parameter `k_phot_max` to observed data (k_phot_max is the typical physiological
# parameter to optimize against the control data
fit1 <- calibrate(
  x = control,
  par = c(k_phot_max = 1),
  data = obs_control,
  output = "BM",
  method="Brent", # Brent is recommended for one-dimensional optimization
  lower=0,        # lower parameter boundary
  upper=0.5       # upper parameter boundary
)
fit1$par
#> k_phot_max 
#>  0.3860241

# For illustrative purposes: Fit parameter `k_phot_max` and `k_resp` (need at 
# least 2 params to do likelihood profiling)
fit2 <- calibrate(
  x = control,
  par = c(k_phot_max=1,  k_resp=0),
  data = obs_control,
  endpoint = "BM")
fit2$par
#> k_phot_max     k_resp 
#>  0.6968750  0.3070312 

# decide on parameter boundaries for likelihood profiling and
# space explorer (if defaults seem not sufficient)
control@param.up[c("k_phot_max", "k_resp")]
pars_bound <- list(k_resp = c(0,10), k_phot_max = c(0,30))

# Update the scenario with fitted parameter and simulate it
fitted_growth <- control %>% 
  set_param(fit2$par) %>% 
  set_param_bounds(pars_bound)
sim_mean <- fitted_growth %>%
  simulate() %>%
  mutate(trial="control")

# Exposure and observations in long format for plotting
treatments <- exposure %>%
  mutate(trial="control")
obs_mean <- obs_control %>%
  mutate(trial="control") %>%
  select(time=t, data=BM, trial)

# Plot results
plot_sd(
  model_base = fitted_growth,
  treatments = treatments,
  rs_mean = sim_mean,
  obs_mean = obs_mean
)

# Likelihood profiling - physiological params
res <- lik_profile(x = fitted_growth,
                   data = obs_control,
                   endpoint = "BM",
                   par = fit2$par,
                   refit = TRUE,
                   type = "fine",
                   break_prof = FALSE)
# check outputs
plot_lik_profile(res)

# parameter space explorer
explore_space(x = list(CalibrationSet(control, obs_control)),
              res = res,
              endpoint = "BM",
              sample_size = 1000,
              max_runs = 30,
              nr_accept = 500)


```

![](../doc/figures/howto-unnamed-chunk-14-1.png)<!-- -->

Option 2: Create a list of *calibration sets* and then fit TK/TD model
parameters on all datasets and exposure levels at the same time:

``` r
# get all the trials and exposure series from Schmitt et al. (2013) and create
# a list of calibration sets
Schmitt2013 %>%
  group_by(ID) %>%
  group_map(function(data, key) {
    exp <- data %>% select(t, conc)
    obs <- data %>% select(t, BM=obs)
    sc <- fitted_growth %>% set_exposure(exp) 
    caliset(sc, obs)
  }) -> cs

# Fit parameters on results of all trials at once
fit2 <- calibrate(
  cs,
  par=c(EC50=0.3, b=4.16, P_up=0.005),
  output="BM",
  method="L-BFGS-B",
  lower=c(0, 0.1, 0),
  upper=c(1000, 10, 0.1)
)
#> Warning in rk(y, times, func, parms, method = "ode45", ...): Number of time steps 1 exceeded maxsteps at t = 0

#> Warning in rk(y, times, func, parms, method = "ode45", ...): Number of time steps 1 exceeded maxsteps at t = 0

#> Warning in rk(y, times, func, parms, method = "ode45", ...): Number of time steps 1 exceeded maxsteps at t = 0

#> Warning in rk(y, times, func, parms, method = "ode45", ...): Number of time steps 1 exceeded maxsteps at t = 0

#> Warning in rk(y, times, func, parms, method = "ode45", ...): Number of time steps 1 exceeded maxsteps at t = 0

#> Warning in rk(y, times, func, parms, method = "ode45", ...): Number of time steps 1 exceeded maxsteps at t = 0
fit2$par
#>       EC50          b       P_up 
#> 0.52965567 5.24863437 0.02039446

# Update the scenario with fitted parameter and simulate all trials
fitted_tktd <- fitted_growth %>%
  set_param(fit2$par)

treatments <- Schmitt2013 %>% select(time=t, conc, trial=ID)
rs_mean <- simulate_batch(
  model_base = fitted_tktd,
  treatments = treatments
)
# Observations in long format for plotting
obs_mean <- Schmitt2013 %>%
  select(time=t, data=obs, trial=ID)

# Plot results
plot_sd(
  model_base = fitted_tktd,
  treatments = treatments,
  rs_mean = rs_mean,
  obs_mean = obs_mean
)
```

![](../doc/figures/howto-unnamed-chunk-15-1.png)<!-- -->

The resulting scenario with fitted parameters shows a very good fit with
the observed effects from experiments.

## Changes in parameter values over time

Sequences can be used to represent changing conditions over time, such
as a change in model parameters which would otherwise be constant. This
can be used to represent events such as a pump failure or change in
temperature.

The function `sequence()` creates an object which represents a sequence
of several scenarios. The sequence is treated as a single scenario and
each scenario is simulated one after the other.

``` r
# base scenario only valid until day 7
sc1 <- metsulfuron %>%
  set_times(0:7)

# a parameter change occurs at day 7: k_loss increases from 0 to 0.1 d-1
sc2 <- metsulfuron %>%
  set_times(7:14) %>%
  set_param(c(k_loss=0.1))
 
seq <- sequence(list(sc1, sc2))
simulate(seq)
#>    time       BM E      M_int      C_int  FrondNo
#> 1     0 50.00000 1   0.000000 0.00000000 500000.0
#> 2     1 52.15858 1 223.074470 0.25609887 521585.8
#> 3     2 52.43495 1 369.634402 0.42211917 524349.5
#> 4     3 52.33917 1 463.026054 0.52973924 523391.7
#> 5     4 52.17923 1 521.892686 0.59891761 521792.3
#> 6     5 52.00016 1 558.626807 0.64328081 520001.6
#> 7     6 51.81403 1 581.218666 0.67170059 518140.3
#> 8     7 51.63740 1 474.071576 0.54974731 516374.0
#> 9     8 46.72570 1 273.120825 0.35001169 467257.0
#> 10    9 42.94600 1 157.349626 0.21939489 429460.0
#> 11   10 40.59758 1  90.651838 0.13370880 405975.8
#> 12   11 38.94561 1  52.226090 0.08029945 389456.1
#> 13   12 37.48893 1  30.088353 0.04805946 374889.3
#> 14   13 36.12872 1  17.334420 0.02873031 361287.2
#> 15   14 34.84676 1   9.986659 0.01716095 348467.6
```

For more information:

``` r
# Call the help page of `sequence`
?sequence
```

## Decrease assessment runtime

There are multiple ways to decrease the runtime of a simulation. One
option is to increase the maximum step length of the solver, `hmax`. It
is an optional argument which can be passed to either `simulate()`,
`effect()`, or `epx()`. The larger `hmax`, the faster the simulation
generally completes. However, be careful: The larger `hmax`, the greater
the risk that simulation results will be inaccurate and simulations may
even fail due to numerical issues.

``` r
# Simulations with a maximum solver step length of hmax=0.01
metsulfuron %>%
  set_times(0:7) %>%
  simulate(hmax=0.1)
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

The calculation of *EPx* values may take a lot of time if one or more of
the following conditions apply:

1)  a large number of scenarios is assessed
2)  the simulated exposure time-series are very long
3)  moving exposure windows are considered which are very small in
    comparison to the length of the exposure series

The `epx()` function is implemented in a way that it can parallelize
calculations by using more than one CPU. To enable parallel processing,
a command like the following needs to preceed the call to `epx()`:

``` r
future::plan(future::multisession)
```

If the computer running the calculation has `n` physical CPU cores, then
the time necessary to calculate *EPx* values will (in the best case)
decrease by the same factor. For more information on how to make use of
parallelization in R, please refer to the `future` package:

``` r
vignette("future-1-overview", package="future")
```

## Implementing custom models

The set of supported models can be extended by users as needed.
Experimental or one-time use models can be implemented in a user’s
script and inserted into the package’s workflow.

The starting point to add a new model is an implementation of the
model’s rules and dynamics in *R* code. In most cases, this will be a
code snippet which describes the model’s Ordinary Differential Equations
(ODE):

``` r
## An exemplary implementation of the GUTS-RED-SD TKTD model ##
# Model ODEs following the deSolve specification
sd_ode <- function(t, state, params) {
  with(as.list(c(state, params)), {
    dDw <- kd * (Cw(t) - Dw)            # dDw/dt, scaled damage
    dH <- kk * max(0, Dw - z)           # dH/dt, cumulative hazard
    
    list(c(dDw, dH))                    # return derivatives
  })
}
```

The previous example implements the model equations in a way that can be
processed by the `deSolve` package for numerical integration. For a
detailed description on how to use `deSolve`, please refer to its
vignette:

``` r
vignette("deSolve", package="deSolve")
```

With some additional scenario data such as initial state, output time
points, and model parameters, we are able to simulate our custom TKTD
model:

``` r
## Properties of a sample scenario ##
init <- c(Dw=0, H=0)                         # initial state
times <- 0:5                                 # output time points [0,5]
param <- c(kd=22, hb=0.01, z=0.5, kk=0.08)   # model parameters
exp <- data.frame(time=0:5,                  # exposure time-series
                  conc=c(0,1,1,0.5,0.6,0.2))

# Create a linear interpolation of the exposure time-series
expf <- approxfun(x=exp$time, y=exp$conc, method="linear", rule=2)
# Extend parameter set by interpolated exposure series
paramx <- as.list(c(param, Cw=expf))

# Numerically solve the ODE
deSolve::ode(y=init, times=times, parms=paramx, func=sd_ode)
#>   time        Dw           H
#> 1    0 0.0000000 0.000000000
#> 2    1 0.9545447 0.008266888
#> 3    2 0.9999995 0.048101596
#> 4    3 0.5227279 0.069837063
#> 5    4 0.5954544 0.073573877
#> 6    5 0.2181819 0.074888021
```

When we have made sure that our custom model can be simulated and works
as expected, we can continue with integrating it into the package’s
workflow. First, we will define a new scenario class that identifies our
custom model by a unique name. Next, we have to tell the package how the
models of this class can be simulated. In a final step, we may have to
describe how effects are calculated for these models.

We start by defining a new scenario class that derives from a suitable
ancestor. In general, we can inherit from the base class
`EffectScenario` which provides the general scenario capabilities.
However, if the custom model is just a variant of an existing model
category, then it will be easier to derive from specialized scenario
class that already provides certain features such as effect calculation.
The following class tree gives an overview of the scenario classes in
use:

    EffectScenario
    |    
    |   Transferable
    |___|__ Lemna
    |   |   |__ LemnaSchmittScenario
    |   |   |__ LemnaSetacScenario
    |   |
    |   |__ Myriophyllum
    |   |   |__ MyrioExpScenario
    |   |   |__ MyrioLogScenario
    |   |
    |   |__ Algae
    |       |__ AlgaeWeberScenario
    |       |__ AlgaeTKTDScenario
    |       |__ AlgaeSimpleScenario    
    |
    |__ GutsRedSd
    |__ GutsRedIt
    |
    |__ DebScenario
        |__ DebAbj
        |__ DebDaphnia

To give an example, to implement a variant of a *Lemna* model, it would
be advisable to derive from the class `Lemna` to benefit from already
implemented features such as effect endpoint calculation and simulation
of biomass transfers. For the custom GUTS-RED-SD model, the ideal choice
would be to derive from `GutsRedSd` to minimize the implementation
overhead. However, we will derive from the general `EffectScenario`
class for the sake of our example and create a scenario object:

``` r
## Integrate a new model class into the package workflow ##
# Create a unique class that derives from 'EffectScenario'
setClass("MyGuts", contains="EffectScenario")

# Create an object of the new class and assign scenario properties
new("MyGuts", name="My custom model") %>%
  set_init(init) %>%
  set_times(times) %>%
  set_param(param) %>%
  set_exposure(exp, reset_times=FALSE) %>%
  set_endpoints("L") -> myscenario

myscenario
#> 'My custom model' scenario
#> param: kd=22, hb=0.01, z=0.5, kk=0.08
#> init : Dw=0, H=0
#> endpt: L
#> times: [0,5] n=6, regular
#> forcs: none
#> expsr: none
#>   time conc
#> 1    0  0.0
#> 2    1  1.0
#> 3    2  1.0
#> 4    3  0.5
#> 5    4  0.6
#> 6    5  0.2
```

The object `myscenario` now carries all the settings and data which we
also used for our test simulation. In the next step, the `solver()`
function will be overloaded to handle objects of the newly defined
scenario class `MyGuts`. The adapted `solver()` function will collect
the properties of a given scenario object, call the ODE solver, and
return simulation results:

``` r
# the actual function calling deSolve can have a different signature
solver_myguts <- function(scenario, times, ...) {
  # overriding output times by function argument must be possible
  if(missing(times))
    times <- scenario@times
  
  # get relevant data from scenario
  init <- scenario@init
  param <- scenario@param
  exp <- scenario@exposure@series
  if(nrow(exp) == 1) { # extend exposure series to have at least two rows
    row2 <- exp[1,]
    row2[[1]] <- row2[[1]]+1
    exp <- rbind(exp, row2)
  }
  
  # Create a linear interpolation of the exposure time-series
  expf <- approxfun(x=exp[,1], y=exp[,2], method="linear", rule=2)
  # Extend parameter set by interpolated exposure series
  paramx <- as.list(c(param, Cw=expf))
  
  as.data.frame(deSolve::ode(y=init, times=times, parms=paramx, func=sd_ode, ...))
}

## Overload the solver() function ##
# The functions signature, i.e. the number and names of its arguments, must stay as is
setMethod("solver", "MyGuts", function(scenario, times, ...) solver_myguts(scenario, times, ...))
```

Overloading an *S4* function is done using `setMethod()`. The first
argument to `setMethod()` identifies which function we want to overload,
in this case it is `solver`. The second argument, `MyGuts`, defines
which object type our overloaded function will accept. In this case, we
want to provide an implementation to simulate `MyGuts` scenarios. *R*
will decide during runtime which of the function candidates to execute
when `solver()` is called based on the type of the first argument to
`solver()`. The third argument,
`function(object, ...) solver_myguts(scenario=object, ...)` forwards
calls to an appropriate function which can handle `MyGuts` objects. The
function signature `function(object, ...)` must stay exactly as it is
and must not be modified.

The code within the body of `solver_myguts()` is almost identical to the
original code used to simulate the prototyped model. We have to deal
with one corner case, though: some exposure time-series will contain
only a single row, representing constant exposure over time, but this
would raise an error in `approxfun()`. In order to avoid this error, we
will duplicate the first row and append it to the series. Technically,
this issue should be handled in the prototyped code as well but was left
out for reasons of brevity.

The parameterized scenario object `myscenario` of class `MyGuts` can be
passed to the overloaded `simulate()` function:

``` r
myscenario %>% simulate()
#>   time        Dw           H
#> 1    0 0.0000000 0.000000000
#> 2    1 0.9545447 0.008266888
#> 3    2 0.9999995 0.048101596
#> 4    3 0.5227279 0.069837063
#> 5    4 0.5954544 0.073573877
#> 6    5 0.2181819 0.074888021
```

The custom model can now be simulated using the framework but it is
still missing effect endpoints. If, on the other hand, we had decided to
inherit our scenario class from a suitable ancestor, such as
`GutsRedSd`, we could have stopped at this point as the ancestor would
provide routines to calculate effects.

By default, any state variable is available as an effect endpoint and
the calculated effect would reflect the change in the state variables’
value at the end of the simulation. However, we need to calculate the
*survival* endpoint in a different manner for GUTS-RED type models. To
implement this specialized endpoint, we need to overload the function
`fx()` which calculates effect endpoints:

``` r
## Overload effect endpoint calculation ##
# fx() is called by effect()
setMethod("fx", "MyGuts", function(scenario, ...) fx_myguts(scenario, ...))

# @param scenario Scenario object to assess
# @param window Start & end time of the moving exposure window
# @param ... any additional parameters
fx_myguts <- function(scenario, window, ...) {
  # simulate the scenario (it is already clipped to the moving exposure window)
  out <- simulate(scenario, ...)
  # only use model state at the end of the simulation
  out <- tail(out, 1)
  # calculate survival according to EFSA Scientific Opinion on TKTD models
  # p. 33, doi:10.2903/j.efsa.2018.5377
  t <- unname(window[2] - window[1])
  survival <- exp(-out$H) * exp(-scenario@param$hb * t)
  return(c("L"=survival))
}

# Derive effect levels for our sample scenario
myscenario %>% effect()
#> # A tibble: 1 × 4
#>   scenario      L L.dat.start L.dat.end
#>   <list>    <dbl>       <dbl>     <dbl>
#> 1 <MyGuts> 0.0722           0         5
```

`fx()` is used by `effect()` to derive effect endpoints for each model.
By convention, any overload of the `fx()` function must return a named
numerical vector containing the effect endpoints for the provided
scenario and moving exposure window. The scenario will already be
parameterized to only simulate the current exposure window. The return
value of `fx()` will then be used to calculate effect levels. In case of
models requiring a control scenario, the effect level will be calculated
as `1 - effect/control`. For models not requiring a control, the
overloaded `fx()` must return the final effect level, i.e. a value from
the interval `[0,1]` (0% to 100% effect).

## Complete working example

In this section, a complete example is given to make predictions and
evaluate toxic effects using a calibrated model, as might be conducted
in a risk assessment context. Specifically, the *Lemna* model will be
set up to match the study of Schmitt et al. (2003, doi:
10.1016/j.ecolmodel.2013.01.017) who exposed *Lemna* to
metsulfuron-methyl.

The model will be parameterized with the values as described in the
study. Then, the model will be inspected and used to make predictions
for exposure scenarios, plot results, and get EPx calculations.

### Setting up a scenario

Setting up the model (i.e. creating a scenario) involves defining the
parameters, the environmental variables (external forcings and chemical
exposure), and the initial conditions. Further, a tag can be assigned to
easily identify the scenario by name. Also, for primary producer models,
a transfer of biomass can be defined to match the experimental design of
the study.

``` r
## Get all model parameters
# Lemna model parameters taken from file 'mm2.r' included
# in supporting material of Schmitt et al. (2013)
param_study <- list(
  #     - Effect -
  Emax     = 0.784,   # [same as conc. data]  maximum effect concentration
  EC50     = 0.3,     # [same as conc. data]  Midpoint of effect curve
  b        = 4.16,    # [-]  Slope of effect curve
  #     - Toxicokinetics -
  P_up     = 0.0054,  # [cm/d]  Permeability for uptake
  AperBM   = 1000,    # [cm^2/g_dw]  Frond area/dry weight
  Kbm      = 0.75,    # []  Biomass(fw)-water partition coefficient
  P_Temp   = F,       # Switch for temperature dependence of cuticle permeability
  MolWeight = 381,    # [g/mol] Molmass of molecule (determines Q10_permeability)
  #     - Fate of biomass -
  k_phot_fix  = F,    # If True k_G_max is not changed by environmental factors
  k_phot_max  = 0.47, # [1/d]  Maximum photosynthesis rate
  k_resp   = 0.05,    # [1/d]  Respiration rate at ref. temperature
  k_loss   = 0.0,     # [1/d]  Some rate of loss (e.g. Flow rate)
  #     - Temperature dependence -
  Tmin     = 8.0  ,   # [°C]  Minimum growth temperature 
  Tmax     = 40.5 ,   # [°C]  Maximum growth temperature
  Topt     = 26.7 ,   # [°C]  Optimum growth temperature 
  t_ref    = 25,      # [°C]  Reference temperature for respiration rate
  Q10      = 2,       # [-]   Temperature dependent factor for respiration rate
  #     - Light dependence (Linear) -
  k_0      = 3 ,      # [1/d]  Intercept of linear part
  a_k      = 5E-5 ,   # [(1/d)/(kJ/m^2/d)]   Slope of linear part
  #     - Phosphorus dependence (Hill like dependence) -
  C_P      = 0.3,     # [mg/L]  Phosporus concentration in water
  CP50     = 0.0043,  # [mg/L]  P-conc. where growth rate is half
  a_P      = 1,       # []  Hill coefficient
  KiP      = 101,     # [mg/L]  P-inhibition constant for very high P-conc.
  #     - Nitrogen dependence (Hill like dependence) -
  C_N      = 0.6,     # [mg/L]  Nitrogen concentration in water
  CN50     = 0.034,   # [mg/L]  N-conc. where growth rate is half
  a_N      = 1,       # []  Hill coefficient
  KiN      = 604,     # [mg/L]  n-inhibition constant for very high P-conc.
  #     - Density dependence -
  BM50     = 176,     # [g_dw/m^2] Cut off BM
  #     - Others -
  mass_per_frond = 0.0001,  # [g_dw/frond]  Dry weight per frond
  BMw2BMd  = 16.7     # [g_fresh/g_dry]  Fresh- / dryweight 
)


## Define the forcing variables
forc_temp <- data.frame(t = 0, tmp = 12) # [°C]  Current temperature (may also be a table)
forc_rad  <- data.frame(t = 0, rad = 15000) # [kJ/m²/d]  Radiation  (may also be given as table)


## Define a simple exposure pattern
# t 0..6  concentration 1 ug/L
# t 7..14 concentration 0 ug/L
exposure <- data.frame(time = 0:14, 
                       conc = c(rep(1, 7), rep(0, 8))
                       )


## Set initial values 
# given in file 'mmc2.r' of Schmitt et al. (2013)
init <- c(
  BM       = 50,     # [g_dw/m^2]  Dry Biomass dry weight per m2
  E        = 1,      # (0-1)  (Toxic) Effect = Factor on growth rate  (Range: 0 - 1, 1=no effect)
  M_int    = 0       # [ug]   Amount of toxicant in biomass
)

## create a scenario object, containing the model (with parameters) and the exposure time-series
Lemna_Schmitt() %>%               # the Lemna model by Schmitt et al. (2013)
  set_tag("metsulfuron") %>%      # set a tage for the specific implementation of the model
  set_init(init) %>%              # set the starting values (as prepared above)
  set_param(param_study) %>%      # set the parameters (as prepared above)
  set_exposure(exposure) %>%      # set the exposure scenario (exposure time series)
  set_forcings(temp=forc_temp, rad=forc_rad) -> metsulfuron # set the external forcing 
# variables, and save everything as an object
```

### Simulating a scenario and plotting

``` r
## simulate with model, under a range of different exposure scenarios
# create several exposure scenarios
exp_scen <- data.frame(time = Schmitt2013$t,
                       conc = Schmitt2013$conc,
                       trial = Schmitt2013$ID)
# simulate for all these scenarios
results <- simulate_batch(
  model_base = metsulfuron,
  treatments = exp_scen,
  param_sample = NULL
)
# plot results
plot_sd(
  model_base = metsulfuron,
  treatments = exp_scen,
  rs_mean = results,
)
```

<img src="../doc/figures/howto-unnamed-chunk-29-1.png" width="100%" />

``` r


## simulate with model, under a range of different exposure scenarios, and including 
## a biomass transfer
# simulate for all scenarios
results <- metsulfuron %>%
  set_transfer(interval = c(5), biomass = 10) %>% # implement a biomass transfer every 5 days
  simulate_batch(treatments = exp_scen)
# plot results
plot_sd(
  model_base = metsulfuron,
  treatments = exp_scen,
  rs_mean = results,
)
```

<img src="../doc/figures/howto-unnamed-chunk-29-2.png" width="100%" />
