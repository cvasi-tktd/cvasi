#' Dynamic Energy Budget (DEB) models
#'
#' Supported models:
#' * [DEB_abj]
#' * [DEBtox]
#'
#' @name DEB-models
#' @family DEB models
#' @family scenarios
#' @aliases DebScenario-class
NULL

# Parent class for all DEB related models/scenarios
#' @export
setClass("DebScenario", contains="EffectScenario")

#' @export
setClass("DebAbj", contains="DebScenario")


#' DEB_abj
#'
#' Creates a *DEB abj* scenario. The *abj* model with type M acceleration is
#' like model *std*, but acceleration occurs between birth and metamorphosis (V1-morph).
#' Isomorphy is assumed before and after acceleration. Metamorphosis is before
#' puberty and occurs at maturity `E_Hj`, which might or might not correspond with
#' changes in morphology. The *abj* model is a one-parameter extension of model *std*
#' [(DEB Wiki)](https://debportal.debtheory.org/docs/Typified_models.html).
#'
#' ## State variables
#'
#' The following list describes the default names and standard units of the model's
#' state variables:
#' * `L`, structural length (cm)
#' * `E`, energy reserve (J)
#' * `H`, energy invested in maturity (J)
#' * `R`, reproduction buffer (J)
#' * `cV`, internal concentration ([C])
#' * `Lmax`, maximum structural length (cm)
#'
#' All state variables are initialized with zero. See [set_init()] on how to set
#' the initial state.
#'
#' ## Parameters
#'
#' The following model parameters are required:
#' * `p_M`, vol-spec somatic maintenance (J/d.cm^3)
#' * `v`, energy conductance (cm/d)
#' * `k_J`, maturity maint rate coefficient (1/d)
#' * `p_Am`, surface-area specific maximum assimilation rate (J/d.cm^2)
#' * `kap`, allocation fraction to soma (-)
#' * `E_G`, spec cost for structure (J/cm^3)
#' * `f`, scaled functional response (-)
#' * `E_Hj`, maturity at metamorphosis (J)
#' * `E_Hp`, maturity at puberty (J)
#' * `kap_R`, reproduction efficiency (-)
#' * `L_b`, structural length at birth (cm)
#' * `L_j`, structural length at metamorphosis (cm)
#' * `ke`, elimination rate constant (d-1)
#' * `c0`, no-effect concentration sub-lethal ([C])
#' * `cT`, tolerance concentration ([C])
#' * `MoA`, mode of action switch (-)
#'
#' ## Mode of Actions
#'
#' Any combination of the following mode of actions (MoA) can be considered by
#' the model:
#' * `MoA = 1`: effect on feeding
#' * `MoA = 2`: effect on maintenance costs
#' * `MoA = 4`: effect on overhead costs for making an egg
#' * `MoA = 8`: hazard during oogenesis
#' * `MoA = 16`: energy conductance
#'
#' To activate more than one MoA, simply add up the corresponding
#' codes. To disable all MoAs, set the parameter to zero.
#' See also [set_mode_of_action()].
#'
#' ## Effects
#'
#' The state variables *L* (structural length) and *R* (reproduction buffer) are
#' set as effect endpoints by default. All state variables are available as
#' potential endpoints. The list of considered endpoints can be modified
#' by using [set_endpoints()].
#'
#' To calculate effects, each *DEB* scenario is simulated twice: One simulation
#' which considers exposure to a toxicant and one simulation without exposure, i.e.
#' a control. See also [effect()].
#'
#' @return an S4 object of type [DebAbj-class]
#' @export
#' @family DEB models
#' @aliases DebAbj-class
#' @examples
#' # Create an abj scenario from scratch and simulate it
#' DEB_abj() %>%
#'   set_init(c(L=0.02,E=0.1,H=0.01)) %>%
#'   set_param(c(p_M=3000,v=0.02,k_J=0.6,p_Am=300,kap=0.9,E_G=4000,f=1,
#'               E_Hj=0.05,E_Hp=0.3,kap_R=0.9,ke=1,c0=0,cT=1,L_b=0.02,
#'               L_j=0.04,MoA=0)) %>%
#'   set_exposure(no_exposure()) %>%
#'   set_times(0:10) %>%
#'   simulate()
#'
#' # Print information about sample scenario 'americamysis'
#' americamysis
#'
#' # Simulate 'americamysis' scenario
#' americamysis %>% simulate()
DEB_abj <- function() {
  new("DebAbj",
      name="DEB_abj",
      param.req=c("p_M","v","k_J","p_Am","kap","E_G","f","E_Hj","E_Hp","kap_R",
                  "ke","c0","cT","L_b","L_j","MoA"),
      endpoints=c("L","R"),
      control.req=TRUE,
      init=c("L"=0,"E"=0,"H"=0,"R"=0,"cV"=0,"Lmax"=0),
      exposure=no_exposure()
  )
}
