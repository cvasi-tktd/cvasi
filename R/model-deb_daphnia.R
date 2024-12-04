#
# Originally, a DEBtox model as implemented in BYOM, DEBtox2019 module, version 4.5,
# last modified 27 Nov 2021. Source: https://www.debtox.info/byom.html
# However, the model equations are almost identical to DEBtox 2019 version 4.7
# which is available here in the package. Therefore, the older version is no longer
# implemented explicitly.
#
# See `DEBtox()` for the newer model version.
#

########################
## Class definition
########################

#' @include model-debtox.R solver.R fx.R
#' @export
setClass("DebDaphnia", contains="DebTox")

########################
## Constructor
########################

#' @describeIn DEBtox Deprecated model variant of `DEBtox()`
#' @export
DEB_Daphnia <- function() {
  lifecycle::deprecate_soft("1.2.0", "DEB_Daphnia()", "DEBtox()",
                            details="The model constructor has been renamed to `DEBtox()`")

    new("DebDaphnia",
        name="DEBtox_Daphnia",
        param.req=c("L0", "Lp", "Lm", "rB", "Rm", "f", "hb", "Lf", "Lj", "Tlag",
                    "kd", "zb", "bb", "zs", "bs", "FBV", "KRV", "kap", "yP",
                    "Lm_ref", "len", "Tbp", "MoA", "FB"),
        endpoints=c("L","R","S"),
        control.req=TRUE,
        init=c("D"=0,"L"=0,"R"=0,"S"=1),
        exposure=no_exposure()
    )
}

########################
## Simulation
########################

# For backwards-compatibility
solver_debtox_daphnia <- function(scenario, ...) {
  # only effective difference between model versions is the introduction of
  # parameter `a` and the limit of hazard rate `h` to 99% per hour in the newer
  # DEBtox version.

  # Setting the default value of a=1 disables the Weibull background hazard
  scenario <- set_param(scenario, c("a"=1))
  # Continue as a DEBtox scenario
  solver_debtox(scenario, ...)
}

#' @describeIn solver (deprecated) Numerically integrates *DEBtox_Daphnia* scenarios
setMethod("solver", "DebDaphnia", solver_debtox_daphnia)

########################
## Effects
########################

# uses the default fx implementation, nothing more required
