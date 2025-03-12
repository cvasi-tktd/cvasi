# cvasi Development version

* MODIFIED

   * The effects calculated by `effect()` are no longer limited to a maximum
     value of `1.0`. Instead, effects are calculated similarly to *relative errors*,
     quantifying the relative difference of endpoints in control and treatment
     scenarios. Values greater than one as well as less than zero can occur.
   * Unused parameter `const_growth` removed from `Algae_Simple()` constructor.
   * *Algae* models now check if all required parameters are present before
     starting a simulation.
   * `Lemna_SETAC` model: The default value of parameter `beta` has been updated
      from formerly `0.025` to now `0.25`. This reflects changes to the
      SETAC working group's report in version 1.2. The old value originated from
      typos in former report versions.

# cvasi 1.4.0

* NEW
   * New `batch()` function to ease the creation of batch simulations based on a
     single scenario and multiple exposure series.
   * New overloads of `plot()` which can graphically depict the return values of
     *cvasi* functions such as `simulate()` and `dose_response()`.
   * New parameter for `sequence()`: argument `breaks` can split the sequence
     elements at the given time points to ease the creation of sequences.
     
* MODIFIED
   * Requirements relaxed for scenarios with simulated biomass transfers:
      * Transfer time points no longer need to be part of the output times.
      * Negative output times are now supported.
   * Example scenarios `minnow_it` and `minnow_sw` now feature a non-zero
     background mortality rate.
   * Default solver of `Algae_Simple` and `Lemna_Schmitt` set to `lsoda`
     to harmonize the numerical scheme used within a class of models
     (before, `ode45` was used).
    * Parameter list of `solver()` functions simplified; the following parameters
      no longer need to be accepted:
      * `times`, output times are solely defined by the scenario itself
      * `f`, `approx`, and `rule`: if default settings of *deSolve*'s interpolation of
        forcings series needs to be adapted, the `fcontrol` argument has to be
        provided by users

# cvasi 1.3.1

* Improved `import_toxswa()`: now supports scaling of imported time series,
  importing selected *TOXSWA* output variables, as well as importing selected
  substance concentrations.
* Function `import_exposure_text()` moved to the *cvasi.ui* package.
* Small bugfix in `plot_ppc()`.
* `morse()` renamed to `import_morse()`, the function now supports loading
  parameter sets from *morse* objects directly, as well as from *.RData* and
  *.RDS* files.
* Parameters to `import_morse()` renamed to a more canonical standard, i.e.
  `find.IT` to `find_it`, `find.SD` to `find_sd` and so forth. Using the old
  parameter names still works but will raise a warning.
* New overloads for the generic `plot()` to depict return values of functions
  such as `simulate()` and `dose_response()`.
* Model equations of the `Lemna_SETAC()` model by Klein et al. was integrated
  into the package. This removes package `lemna` as a dependency.
* Scenario defaults of `Lemna_SETAC()` were modified to ease scenario creation
  and to reflect common study conditions:
    * Initial biomass set to `0.0012` 
    * Parameter default `k_photo_fixed=TRUE`
    * Forcings default to constant non-zero conditions; temperature (°C) `temp=12`, 
      irradiance (kJ m-2 d-1) `irr=15000`, Phosphorus concentratio (mg P L-1) `P=0.3`,
      Nitrogen concentration (mg N L-1) `N=0.6`. These defaults only take effect
      if parameter `k_photo_fixed=FALSE`.

# cvasi 1.2.0

* New likelihood profiling with `lik_profile()` based on the routines described
  by Tjalling Jager (doi: 10.1002/ieam.4333) and implemented in *BYOM*
* The function `survival()` has been deprecated. Survival probabilities of
  affected models are now part of the return value of `simulate()`.
* The function `DEB_Daphnia()` has been superseded by the more appropriatly
  named `DEBtox()`. The former function is still available, but will show a
  warning when used.
* The `DEBtox()` model equations have been updated to conform with *BYOM*'s
  *DEBtox 2019* module version 4.7.
* New slot `param.bounds` was added to effect scenario classes to store valid
  parameter ranges of commonly fitted model parameters. The information is 
  currently only used in likelihood profiling.
* New function `set_bounds()` to set or modify parameter bounds for one or more
  scenarios or calibration sets.
* Custom error functions supplied to `calibrate()` must accept four arguments;
  the error function will be supplied with all observed and predicted values in
  one call. The two additional parameters represent optional weights and
  study IDs.
* Verbose output is enabled by default for `calibrate()`
* New slot `tag` was added to *calibration set* classes to store additional
  metadata such as study IDs. The information will be passed on to the error
  function by `calibrate()`.
* The function to create *calibration sets* has been renamed to `caliset()`.
  The former `CalibrationSet()` is still available, but will show a warning
  when used.

# cvasi 1.1.3

* Links to MOSAIC web page (mosaic.univ-lyon1.fr) had to be removed because
  server was inaccessible

# cvasi 1.1.2

* `morse` removed (temporarily) as suggested package, because it was archived by CRAN
* Model man pages amended

# cvasi 1.1.1

* Various improvements to plotting routines
* New `plot_scenario()`

# cvasi 1.1.0

* `calibrate()`
  * Function arguments simplified and partly renamed
  * Fitting is now more error-tolerant and will not abort if a simulation
    fails or if it returns invalid values

# cvasi 1.0.1

* `Algae_TKTD()` harmonized with `Algae_Simple()` model equations
* Fix for numerical issues in unit tests and vignette on *macOS ARM* platform
* Minor changes to conform with *CRAN* requirements

# cvasi 0.10.5

Initial public release
