#
# DEBtox model as implemented in BYOM, DEBtox2019 module, version 4.7, last
# modified 13 Dec 2022. Source: https://www.debtox.info/byom.html
#
# See Jager (2020, DOI: 10.1016/j.ecolmodel.2019.108904 ) for details.
#


########################
## Class definition
########################

#' @include man-deb.R
#' @export
setClass("DebTox", contains="Deb")


########################
## Constructor
########################

#' DEBtox model
#'
#' Creates a  *DEBtox* scenario as described by Jager (2020). It
#' represents a simplified *DEBtox* model based on *DEBkiss*. In the *BYOM*
#' application \[[link](https://www.debtox.info/byom.html)\], this model is
#' referred to as *DEBtox 2019*, version 4.7.
#' It supports an optional feature of the *ERA special* model variant, which
#' can consider a reference *Lm* parameter to compare results of multiple
#' datasets.
#'
#' ## State variables
#'
#' The following list describes the names and units of the
#' model's state variables:
#'  - `D`, scaled damage (\[C\])
#'  - `L`, body length (mm)
#'  - `R`, cumulative reproduction (-)
#'  - `S`, survival probability (-)
#'
#' State variables `D`, `L`, and `R` are initialized with zero. Variable `S`
#' is initialized with one (`1.0`). See [set_init()] on how to set
#' the initial state manually.
#'
#' ## Parameters
#'
#' The following parameters are required:
#'  - General
#'    - `L0`, body length at start (mm)
#'    - `Lp`, body length at puberty (mm)
#'    - `Lm`, maximum body length (mm)
#'    - `rB`, von Bertalanffy growth rate constant (1/d)
#'    - `Rm`, maximum reproduction rate (#/d)
#'    - `f`, scaled functional response (-)
#'    - `hb`, background hazard rate (d-1)
#'    - `a`, Weibull background hazard coefficient (-). Set to `1` to disable.
#'  - Extra parameters
#'    - `Lf`, body length at half-saturation feeding (mm)
#'    - `Lj`, body length at which acceleration stops (mm)
#'    - `Tlag`, lag time for start development (d)
#'  - TK/TD parameters
#'    - `kd`, dominant rate constant (d-1)
#'    - `zb`, effect threshold energy budget (\[C\])
#'    - `bb`, effect strength energy-budget effects (1/\[C\])
#'    - `zs`, effect threshold survival (\[C\])
#'    - `bs`, effect strength survival (1/(\[C\] d))
#'  - Other parameters (formerly globals in *BYOM*)
#'    - `FBV`, dry weight egg as fraction of structural body weight (-)
#'    - `KRV`, part. coeff. repro buffer and structure (kg/kg) (for losses with reproduction)
#'    - `kap`, approximation for kappa (for starvation response)
#'    - `yP`, product of yVA and yAV (for starvation response)
#'    - `Lm_ref`, optional reference max length for scaling rate constants (mm).
#'       Set to zero to disable the reference length. Disabled by default.
#'    - `len`, a switch to control body length dynamics: `1` organism can shrink,
#'         `2` organism cannot shrink. Default value is `1`.
#'    - `Tbp`, optional brood-pouch delay (d). Set to `NA` or zero to disable.
#'          Default value is `0`.
#'    - `MoA`, mode of action switches (-). Default value is `0`.
#'    - `FB`, feedback on damage dynamics switches (-). Default value is `0`.
#'
#' A reference `Lm_ref` is needed to properly compare different data sets,
#' or when calibrating on more than one data set. If `Lm` differs, one would not
#' want to have different rate constants at the same length.
#'
#' ### Mode of Action
#'
#' Any combination of the following mode of actions (*MoA*) can be considered by
#' the model:
#'  - `MoA = 1`: assimilation/feeding
#'  - `MoA = 2`: costs for maintenance
#'  - `MoA = 4`: costs for growth and reproduction
#'  - `MoA = 8`: costs for reproduction
#'  - `MoA = 16`: hazard for reproduction
#'
#' To activate more than one mode of action, simply add up the corresponding
#' codes and set parameter `MoA` to the desired value. To disable all mode of
#' actions, set parameter `MoA` to zero. See also [set_moa()].
#'
#' As an example, to consider effects on feeding and maintenance, set the
#' mode of action to three (`3`):
#'
#' `DEBtox() %>% set_param(c(MoA=3))`
#'
#' ### Feedbacks
#'
#' Any combination of the following damage feedbacks can be considered by
#' the model:
#'  - `1`: surf:vol scaling uptake rate
#'  - `2`: surf:vol scaling elimination rate
#'  - `4`: growth dilution
#'  - `8`: losses with reproduction
#'
#' To activate more than one feedback, simply add up the corresponding
#' codes. To disable all feedbacks, set the parameter to zero.
#'
#' ## Effects
#'
#' The state variables *L* (body length), *R* (cumulative reproduction), and
#' *S* (survival probability) are set as effect endpoints by default. All state
#' variables are available as potential endpoints. The list of considered
#' endpoints can be modified by using [set_endpoints()].
#'
#' To calculate effects, each *DEBtox* scenario is simulated twice: One simulation
#' which considers exposure to a toxicant and one simulation without exposure, i.e.
#' a control. See also [effect()].
#'
#' ## Simulation output
#'
#' The following intermediary model variables can be added to the model
#' output on demand. Simply set the optional parameter `nout` to the
#' required output level and pass it to [simulate()].
#'
#'  - `nout` >= 1: `f`, actual scaled response
#'  - `nout` >= 2: `fR`, actual f considering starvation
#'  - `nout` >= 3: `kd`, actual kd
#'  - `nout` >= 4: `s`, stress level
#'  - `nout` >= 5: `h`, hazard rate
#'  - `nout` >= 6: `sA`, stress factor on assimilation/feeding
#'  - `nout` >= 7: `sM`, stress factor on maintenance
#'  - `nout` >= 8: `sG`, stress factor on growth costs
#'  - `nout` >= 9: `sR`, stress factor on reproduction costs
#'  - `nout` >= 10: `sH`, stress factor on hazard to reproduction
#'  - `nout` >= 11: `xu`, damage feedback factor for surf:vol scaling uptake rate
#'  - `nout` >= 12: `xe`, damage feedback factor for surf:vol scaling elimination rate
#'  - `nout` >= 13: `xG`, damage feedback factor for growth dilution
#'  - `nout` >= 14: `xR`, damage feedback factor for losses with repro
#'
#' ## Model history and changes
#'
#' - cvasi v1.0.0
#'    - The `DEB_Daphnia()` model implemented BYOM's *DEBtox 2019*
#'      model version 4.5
#' - cvasi v1.2.0
#'    - The model equations were updated to conform with BYOM's *DEBtox 2019*
#'      version 4.7. This introduced a new model parameter `a`, the Weibull
#'      background hazard coefficient, and limited the maximum hazard rate to
#'      99% per hour.
#'    - The scenario constructor was renamed to `DEBtox()`.
#'    - Additional intermediary model variables available as optional simulation
#'      output
#'
#' @return an S4 object of type [DebTox-class]
#' @export
#' @family DEB models
#' @aliases DEBtox DEBtox2019 DebTox-class DEB_Daphnia DebDaphnia-class
#' @references
#' Jager T, 2020: Revisiting simplified DEBtox models for analysing
#' ecotoxicity data. Ecol Model 416. \doi{10.1016/j.ecolmodel.2019.108904}
#'
#' Romoli et al., 2024: Environmental risk assessment with energy budget
#' models: a comparison between two models of different complexity.
#' Environ Toxicol Chem 43(2):440-449. \doi{10.1002/etc.5795}
DEBtox <- function() {
  new("DebTox",
      name="DEBtox",
      param=list("Lm_ref"=0, "len"=1, "Tbp"=0, "MoA"=0, "FB"=0),
      param.req=c("L0", "Lp", "Lm", "rB", "Rm", "f", "hb", "a", "Lf", "Lj", "Tlag",
                  "kd", "zb", "bb", "zs", "bs", "FBV", "KRV", "kap", "yP", "Lm_ref",
                  "len", "Tbp", "MoA", "FB"),
      endpoints=c("L","R","S"),
      control.req=TRUE,
      init=c("D"=0, "L"=0, "R"=0, "S"=1),
      exposure=no_exposure()
  )
}


########################
## Simulation
########################

#' @importFrom deSolve ode
solver_debtox <- function(scenario, times, approx=c("linear", "constant"),
                          f=1, rule=2, method="ode45", ...) {
  # use time points from scenario if nothing else is provided
  if(missing(times))
    times <- scenario@times
  # check if at least two time points are present
  if(length(times) < 2)
    stop("times vector must have at least two elements")

  params <- scenario@param
  if(is.list(params))
    params <- unlist(params)
  approx <- match.arg(approx)

  if(scenario@init[["L"]] < params[["L0"]])
    warning("initial L is smaller than parameter L0")

  # make sure that parameters are present and in required order
  params_req <- c("L0", "Lp", "Lm", "rB", "Rm", "f", "hb", "a", "Lf", "Lj", "Tlag",
                  "kd", "zb", "bb", "zs", "bs", "FBV", "KRV", "kap", "yP",
                  "Lm_ref", "MoA", "FB")
  # names of additional output columns
  outnames <- c("f", "fR", "kd", "s", "h", "sA", "sM", "sG", "sR", "sH",
                "xu", "xe", "xG", "xR")

  # check for missing parameters
  params.missing <- is.na(params[params_req])
  if(any(params.missing))
    stop(paste("missing parameters:", paste(params_req[params.missing], sep=",", collapse=",")))

  # This is a quick check to make sure that acceleration cannot continue
  # after puberty. So, Lj should be smaller than Lp. This is done to ensure
  # consistency with standard DEB, where acceleration stops at a maturity
  # level, and maturity does not increase anymore after puberty. However,
  # there is no strong theoretical reason why animals should stop
  # accelerating at puberty, so this part could be commented out.
  if(params[["Lj"]] > params[["Lp"]])
    stop("Lj > Lp, acceleration cannot continue after puberty")

  # only one forcings time-series: exposure
  forcings <- list(scenario@exposure@series)

  # This is a means to include a delay caused by the brood pouch in species
  # like Daphnia. The repro data are for the appearance of neonates, but egg
  # production occurs earlier. This globally shifts the model output in this
  # function below. This way, the time vector in the data does not need to be
  # manipulated, and the model plots show the neonate production as expected.
  has_bp <- min(params[["Tbp"]], na.rm=TRUE) > 0 # consider brood-pouch delay?
  if(has_bp) {
    # Extra times needed to calculate brood-pouch delay
    tbp <- times[times > params[["Tbp"]]] - params[["Tbp"]]
    ordering <- order(c(times, tbp))
    # Boolean vector to identify newly added time points
    loct <- c(rep(F, length(times)), rep(T, length(tbp)))[ordering]
    # Full vector with original and new time points, sorted
    times <- sort(c(times, tbp))
  }

  # run solver
  out <- ode(y=scenario@init, times=times, parms=params[params_req], dllname="cvasi",
             initfunc="debtox_init", func="debtox_func", initforc="debtox_forc",
             forcings=forcings, fcontrol=list(method=approx, rule=rule, f=f, ties="ordered"),
             outnames=outnames, method=method, ...)
  out <- as.data.frame(out)

  # When animal cannot shrink in length (but does on weight!)
  if(min(params[["len"]], na.rm=TRUE) == 2) {
    # Find cumulative maximum of body length and replace values in output
    out[, "L"] <- cummax(out[, "L"])
  }

  # Make sure survival does not get negative
  # In some cases, it may become just a bit negative, which means zero.
  out[, "S"] <- pmax(0, out[, "S"])

  # If we need a brood-pouch delay ...
  if(has_bp) {
    # Find where the extra brood-pouch time points are in the output
    Xbp <- out[loct, "R"]
    # Sub-select the output to return only data for the original time points
    out <- out[!loct, ]
    # Put in the brood-pouch reproduction values we asked for, initial lag phase
    # set to zero.
    out[, "R"] <- c(rep(0, nrow(out) - length(Xbp)), Xbp)
  }

  out
}

#' @include solver.R
#' @describeIn solver Numerically integrates *DEBtox* scenarios
setMethod("solver", "DebTox", solver_debtox)


########################
## Effects
########################

# uses the default fx implementation, nothing more required
