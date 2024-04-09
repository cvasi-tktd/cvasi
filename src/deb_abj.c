#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <math.h>
/*******************************************************************
 *
 * DEB abj model adapted from Matlab code
 *
 * The model provides additional output on intermediary variables on
 * request; please refer to deSolve's manual on the 'nout' parameter.
 *
 *******************************************************************/

/*
 * Allocate memory for global parameter array
 */
static double parms[16];
/*
 * Allocate memory for forcing function data
 * Array's values get updated by ODE solver in every time step.
 */
static double forc[1];
/*
 * Mode of Action switch for model
 */
static unsigned short MoA;
/*
 * Mode of Action identifiers
 */
static unsigned short moaFeeding     = 0x01;
static unsigned short moaMaintCost   = 0x02;
static unsigned short moaOverheadEgg = 0x04;
static unsigned short moaOogenesis   = 0x08;
static unsigned short moaEnergyCond  = 0x10;

/*
 * Aliases for state variable
 */
#define L  y[0] // cm, structural length at previous time point
#define E  y[1] // J, energy reserve
#define H  y[2] // J, energy invested in maturity
#define R  y[3] // J, reproduction buffer
#define cV y[4] // ug/l, internal concentration
#define Lmax y[5] // cm, maximum structural length
/*
 * Aliases for derivatives
 */
#define dL  ydot[0] // cm/d, change in structural length
#define dE  ydot[1] // J/d, change in energy reserve
#define dH  ydot[2] // J/d, change in energy invested in maturity
#define dR  ydot[3] // J/d, change in reproduction buffer
#define dcV ydot[4] // ug/l.d, change in internal concentration
#define dLmax ydot[5] // cm/d, change in maximum structural length
/*
 * Aliases for parameters
 */
#define p_M parms[0]     // J/d.cm^3, vol-spec somatic maint
#define param_v parms[1]       // cm/d, energy conductance
#define param_k_J parms[2]     // 1/d, maturity maint rate coefficient
#define param_p_Am parms[3]    // J / d.cm^2, surface-area specific maximum assimilation rate
#define kap parms[4]     // -, allocation fraction to soma
#define E_G parms[5]     // J/cm^3, spec cost for structure
#define param_f parms[6] // -, scaled functional response
#define E_Hj parms[7]    // J, maturity at metamorphosis
#define E_Hp parms[8]    // J, maturity at puberty
#define param_kap_R parms[9]   // -, reproduction efficiency
#define ke parms[10]     // d-1, elimination rate constant
#define c0 parms[11]     // ug/l, no-effect concentration sub-lethal
#define cT parms[12]     // ug/l, tolerance concentration
#define L_b parms[13]    // cm, structural length at birth
#define L_j parms[14]    // cm, structural length at metamorphosis
/*
 * Aliases for forcing data
 */
#define c forc[0] // ug/l, external concentration

/*
 * Parameter initializer
 */
void deb_abj_init(void (*odeparms)(int*, double*))
{
  int N=16;
  odeparms(&N, parms);

  // Mode of action switch
  MoA = (unsigned short)parms[15];
  /*if(MoA == 0)
   warning("No Mode of Action was selected");*/
}

/*
 * Forcings initializer
 */
void deb_abj_forc(void (* odeforcs)(int *, double *))
{
  int N=1;
  odeforcs(&N, forc);
}

/*
 * Derivatives
 */
void deb_abj_func(int *neq, double *t, double *y, double *ydot, double *yout, int*ip)
{
  // calculate the acceleration factor
  double MV;
  if(H < E_Hj) // at birth
    MV = fmax(L_b, fmin(L, L_j))/L_b; // -, shape correction function
  else // at/after metamorphosis
    MV = L_j/L_b;

  // update parameters
  double f = param_f;
  double v = param_v*MV; double p_Am = param_p_Am*MV;
  double Lm = p_Am*kap/p_M; // cm, maximum structural length
  double k_M = p_M/E_G;
  double kap_R = param_kap_R;
  double k_J = param_k_J;

  //// physyological modes of action

  // calculate the stress factor
  double s  = (1/cT)*fmax(0,cV-c0); // stress factor
  // mode of action: effect on feeding
  if(MoA & moaFeeding) {
    f = param_f*fmax(0,1-s);
  }

  // mode of action: effect on maintenance costs
  if(MoA & moaMaintCost) {
    k_M = k_M * (1 + s); k_J = k_J * (1 + s);
  }

  // mode of action: effect on overhead costs for making an egg
  if(MoA & moaOverheadEgg) {
    kap_R = kap_R / (1 + s); //implemented via the reproduction efficency
  }

  // mode of action: hazard during oogenesis
  if(MoA & moaOogenesis) {
    kap_R = kap_R * exp(-s); //implemented via the reproduction efficency
  }

  // mode of action: energy conductance
  if(MoA & moaEnergyCond) {
    v = v / (1 + s);
  }

  //// energetic processes

  // growth rate
  double L2 = L*L;
  double L3 = L2*L;
  double r = ((E*v/(L3*L)) - (k_M*E_G/kap)) / ((E/L3) + (E_G/kap)); // 1/d, specific growth rate
  // fluxes
  double pC = E*(v/L - r); // J/d, mobilization
  double pA = f*p_Am*L2;   // J/d, assimilation
  double pJ = H*k_J;       // J/d; energy invested in maturity

  // calculate changes in state variables
  dL = L*r/3; // cm/d growth
  dLmax = fmax(0, dL);

  dE = pA - pC; // J/d reserve dynamics
  dH = dR = 0;
  if(H < E_Hp)
    dH = fmax(0,(1-kap)*pC - pJ); // J/d increase in maturation
  else
    dR = kap_R * fmax(0,(1-kap)*pC - pJ); // J/d investment in reproduction

  dcV = ke*(Lm/L)*(c-cV) - cV*(3/L)*dL; // change in scaled internal conc.

  // add fluxes to output
  if(*ip>=3) {
    yout[0] = pC;
    yout[1] = pA;
    yout[2] = pJ;
  }
  // add acceleration factor to output
  if(*ip>=4) {
    yout[3] = MV;
  }
}

/*
 * Root function to find organism length at metamorphosis, H = E_Hj
 */
void deb_abj_root_Lj(int *neq, double *t, double *y, int *ng, double *gout, double *out, int *ip)
{
  gout[0] = H - E_Hj;
}
