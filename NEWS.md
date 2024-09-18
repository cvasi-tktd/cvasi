# cvasi Development version

* New likelihood profiling with `lik_profile()` based on the routines described
  by Tjalling Jager (doi: 10.1002/ieam.4333) and implemented in *BYOM*
* The function `survival()` has been deprecated. Survival probabilities of
  affected models are now part of the return value of `simulate()`.
* The function `DEB_Daphnia()` has been superseded by the more appropriatly
  names `DEBtox()`. The former function is still available, but will show a
  warning when used.
* The `DEBtox()` model equations have been updated to conform with *BYOM*'s
  *DEBtox 2019* module version 4.7.
* New slot `param.bounds` was added to effect scenario classes to store valid
  parameter ranges of commonly fitted model parameters. The information is 
  currently only used in likelihood profiling.
* New function `set_bounds()` to set or modify parameter bounds for one or more
  scenarios or calibration sets.
* Custom error functions supplied to `calibrate()` must not accept four arguments;
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
