# Roadmap

Planned releases and changes

## Unspecified version

FEATURES

 * Log-likelihood profiling based on count data
 * Mixtox calculations
 
USABILITY

 * More robust handling of numerics with a self-correcting or *auto* option in
   case of numerical issues
 * Simpler workflows
   * Import of study data from flat files, automatic conversion to calibration sets
   * Export of calibration sets as flat files

DEPRECATED

 * `simulate_batch()` replaced by the more flexible `batch()`
 * `CalibrationSet()` replaced by `caliset()`
 

## Release v2.0.0

BREAKING CHANGES

 * `set_exposure()`: default value of argument `reset_times` set to `reset_times=FALSE`
