#' Myriophyllum models
#'
#' Supported models:
#' * [Myrio()], with exponential growth
#' * [Myrio_log()], with logistic growth
#'
#' @name Myriophyllum-models
#' @seealso [Lemna-models], [Transferable]
#' @family Myriophyllum models
#' @family scenarios
#' @aliases Myriophyllum-class
NULL

#' @include class-Transferable.R
#' @export
setClass("Myriophyllum", contains=c("Transferable","EffectScenario"))

# Myriophyllum model class
#' @export
setClass("MyrioExpScenario", contains="Myriophyllum")

# Myriophyllum model class
#' @export
setClass("MyrioLogScenario", contains="Myriophyllum")

#' Myriophyllum model with exponential growth
#'
#' The *Myriophyllum* model is derived from the *Lemna* TKTD model by
#' Klein *et al.* (2021). The Myriophyllum model is mathematically equivalent
#' to the Tier 2C version of the *Lemna* model by Klein *et al.* (2021),
#' cf. [Lemna_SETAC()]. Recommended settings for Tier 2C are `k_photo_fixed=TRUE`
#' and `k_resp=0` (Klein *et al.* 2021).
#' In particular, the growth model is a simple exponential growth model,
#' which is considered to be the typical situation for a laboratory macrophyte
#' study. Instead of frond numbers as for *Lemna*, the biomass is also returned as
#' total shoot length (`TSL`) in simulation results.
#' Consequently, the model has the additional parameter `r_DW_TSL`
#' (dry weight per total shoot length ratio) instead of `r_DW_FN` (dry weight
#' per frond number ratio).
#'
#' @section State variables:
#' The model has two state variables:
#' - `BM`, Biomass (g dw m-2)
#' - `M_int`, Mass of toxicant in plant population (mass per m2, e.g. ug m-2)
#'
#' @section Model parameters:
#' - Growth model
#'   - `k_photo_max`, Maximum photosynthesis rate (d-1), default: `0.47`
#'
#' - Concentration response (Toxicodynamics)
#'   - `EC50_int`, Internal concentration resulting in 50% effect (ug L-1)
#'   - `E_max`, Maximum inhibition (-), default: `1`
#'   - `b`, Slope parameter (-)
#'
#' - Internal concentration (Toxicokinetics)
#'   - `P`, Permeability (cm d-1)
#'   - `r_A_DW`, Area per dry-weight ratio (cm2 g-1), default: `1000`
#'   - `r_FW_DW`, Fresh weight per dry weight ratio (-), default: `16.7`
#'   - `r_FW_V`, Fresh weight density (g cm-3), default: `1`
#'   - `r_DW_TSL`, Dry weight per total shoot length ratio  (?)
#'   - `K_pw`, Partitioning coefficient plant:water (-), default: `1`
#'   - `k_met`, Metabolisation rate (d-1), default: `0`
#'
#' @section Environmental factors:
#'
#' None.
#'
#' @section Simulation output:
#'
#' Simulation results will contain two additional columns besides state variables:
#' * `C_int`, internal concentration of toxicant (mass per volume)
#' * `TSL`, total shoot length (?)
#'
#' The available output levels are as follows:
#' - `nout >= 1`
#'   - `C_int`, internal concentration (mass per volume)
#' - `nout >= 2`
#'   - `TSL`, total shoot length (?)
#' - `nout >= 3`
#'   - `f_photo`, photosynthesis dependency function (-)
#' - `nout >= 5`, growth and TK/TD
#'   - `C_int_unb`, unbound internal concentration (mass per volume)
#'   - `C_ext`, external concentration (mass per volume)
#' - `nout >= 7`, environmental factors
#'   - `dBM`, biomass derivative (g dw m-2 d-1)
#'   - `dM_int`, mass of toxicant in plants derivative (mass per m2 d-1)
#'
#' @inheritSection Lemna_SETAC Effects
#' @inheritSection Transferable Biomass transfer
#' @references
#' Klein J., Cedergreen N., Heine S., Reichenberger S., Rendal C.,
#' Schmitt W., Hommen U., 2021: *Refined description of the Lemna TKTD growth model
#' based on Schmitt et al. (2013) - equation system and default parameters*.
#' Report of the working group *Lemna* of the SETAC Europe Interest Group Effect
#' Modeling. Version 1, uploaded on 22. Sept. 2021.
#' https://www.setac.org/group/effect-modeling.html
#'
#' @return an S4 object of type [MyrioExpScenario-class]
#' @seealso [Macrophyte-models], [Transferable], [Scenarios]
#' @family Myriophyllum models
#' @family macrophyte models
#' @aliases MyrioExpScenario-class
#' @export
Myrio <- function() {
  new("MyrioExpScenario",
      name="Myriophyllum",
      param.req=c("k_photo_max", "E_max", "EC50_int", "b", "P", "r_A_DW",
                  "r_FW_DW", "r_FW_V", "r_DW_TSL", "K_pw", "k_met"),
      # default values as defined by Klein et al. (2021)
      param=list("k_photo_max"=0.47, "E_max"=1, "r_A_DW"=1000,
                 "r_FW_DW"=16.7, "r_FW_V"=1, "K_pw"=1, "k_met"=0),
      endpoints=c("BM", "r"),
      control.req=TRUE,
      init=c(BM=0, M_int=0),

      transfer.interval=-1,
      transfer.biomass=NA_real_,
      transfer.comp.biomass="BM",
      transfer.comp.scaled="M_int"
  )
}

#' Myriophyllum model with logistic growth
#'
#' The *Myriophyllum* model is derived from the *Lemna* TKTD model
#' by Klein *et al.* (2021).
#' `Myrio_log()` modifies the [Myrio()] model to feature logistic growth, i.e.
#' control growth is described by the differential equation
#' `d BM/dt = k_photo_max*BM*(1 - BM/BM_L)` where `BM_L` is the carrying capacity.
#'
#' @inheritSection Myrio State variables
#' @section Model parameters:
#' - Growth model
#'   - `k_photo_max`, Maximum photosynthesis rate (d-1), default: `0.47`
#'   - `BM_L`, Carrying capacity (g dw m-2)
#'
#' - Concentration response (Toxicodynamics)
#'   - `EC50_int`, Internal concentration resulting in 50% effect (ug L-1)
#'   - `E_max`, Maximum inhibition (-), default: `1`
#'   - `b`, Slope parameter (-)
#'
#' - Internal concentration (Toxicokinetics)
#'   - `P`, Permeability (cm d-1)
#'   - `r_A_DW`, Area per dry-weight ratio (cm2 g-1), default: `1000`
#'   - `r_FW_DW`, Fresh weight per dry weight ratio (-), default: `16.7`
#'   - `r_FW_V`, Fresh weight density (g cm-3), default: `1`
#'   - `r_DW_TSL`, Dry weight per total shoot length ratio  (?)
#'   - `K_pw`, Partitioning coefficient plant:water (-), default: `1`
#'   - `k_met`, Metabolisation rate (d-1), default: `0`
#'
#' @inheritSection Myrio Environmental factors
#' @inheritSection Myrio Simulation output
#' @inheritSection Myrio Effects
#' @inheritSection Transferable Biomass transfer
#' @inherit Myrio references
#'
#' @return an S4 object of type [MyrioLogScenario-class]
#' @seealso [Transferable], [Scenarios]
#' @family Myriophyllum models
#' @family macrophyte models
#' @aliases MyrioLogScenario-class
#' @export
Myrio_log <- function() {
  new("MyrioLogScenario",
      name="Myriophyllum",
      param.req=c("k_photo_max", "BM_L", "E_max", "EC50_int", "b", "P", "r_A_DW",
                  "r_FW_DW", "r_FW_V", "r_DW_TSL", "K_pw", "k_met"),
      # default values as defined by Klein et al. (2021)
      param=list("k_photo_max"=0.47, "E_max"=1, "r_A_DW"=1000,
                 "r_FW_DW"=16.7, "r_FW_V"=1, "K_pw"=1, "k_met"=0),
      endpoints=c("BM", "r"),
      control.req=TRUE,
      init=c(BM=0, M_int=0),
      transfer.interval=-1,
      transfer.biomass=NA_real_,
      transfer.comp.biomass="BM",
      transfer.comp.scaled="M_int"
  )
}

