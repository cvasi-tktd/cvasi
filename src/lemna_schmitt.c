#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <math.h>
/*******************************************************************
 *
 * Lemna (threshold) model by Schmitt et al. (2013)
 * DOI: 10.1016/j.ecolmodel.2013.01.017
 *
 * The model equation include a number of sub-models which are enabled
 * or disabled depending on the input parameters. The model only considers
 * an area under the curve (AUC) exposure threshold if parameter 'threshold'
 * is larger than zero.
 *
 * The model provides additional output on intermediary variables on
 * request; please refer to deSolve's manual on the 'nout' parameter.
 *
 *******************************************************************/

/*
 * Allocate memory for global parameter array
 */
static double parms[31] = {0};

/*
 * Allocate memory for forcing function data
 *
 * Array's values get updated by ODE solver in every time step.
 */
static double forc[3] = {0};

/*
 * Constant helper value
 */
static double const_f_PN = 0;

/*
 * Constant helper value
 */
static double const_Q10perm = 0;

/*
 * Define aliases
 */
// state variable aliases
#define BM y[0]
#define E y[1]
#define M_int y[2]
#define AUC y[3]

// derivative aliases
#define dBMdt ydot[0]
#define dEdt ydot[1]
#define dM_intdt ydot[2]
#define dAUC ydot[3]

// parameter aliases
#define Emax parms[0]
#define EC50 parms[1]
#define b parms[2]
#define P_up parms[3]
#define AperBM parms[4]
#define Kbm parms[5]
#define P_Temp parms[6]
#define MolWeight parms[7]
#define k_phot_fix parms[8]
#define k_phot_max parms[9]
#define k_resp parms[10]
#define k_loss parms[11]
#define Tmin parms[12]
#define Tmax parms[13]
#define Topt parms[14]
#define t_ref parms[15]
#define Q10 parms[16]
#define k_0 parms[17]
#define a_k parms[18]
#define C_P parms[19]
#define CP50 parms[20]
#define a_P parms[21]
#define KiP parms[22]
#define C_N parms[23]
#define CN50 parms[24]
#define a_N parms[25]
#define KiN parms[26]
#define BM50 parms[27]
#define mass_per_frond parms[28]
#define BMw2BMd parms[29]
#define threshold parms[30]

// forcing by external variables
#define actConc forc[0]
#define actTemp forc[1]
#define actRad forc[2]

// declare function headers
double f_N(void);
double f_P(void);

// declare constants for toxicodynamic submodel selection
#define TD_direct 0
#define TD_delayed 1

/*
 * Parameter initializer
 */
void lemna_init(void (* odeparms)(int *, double *))
{
  int N=31;
  odeparms(&N, parms);

  const_f_PN = f_P()*f_N();
  const_Q10perm = exp(10 * (0.307 * MolWeight / 1.4 + 95) / 0.008314 / (300 * 300));  // see Baur Publication 7
}

/*
 * Forcings initializer
 */
void lemna_forc(void (* odeforcs)(int *, double *))
{
  int N=3;
  odeforcs(&N, forc);
}

/*
 *  Calculation of temperature dependent permeability
 */
double P_T(double temp)
{
  //double Eact = 0.307 * MolWeight / 1.4 + 95; // Activation energy
  //double Q10perm = exp(10 * Eact / 0.008314 / (300 * 300));  // see Baur Publication 7
  return P_up * pow(const_Q10perm, (temp-20) / 10);
}


/*
 * Functions for calculating reduction factors for growth rate
 */
// Light
double f_R(double rad)
{
  double photfac = a_k*rad + k_0;
  if(photfac > 1)
    photfac = 1;
  return(photfac);
}

// Temperature effect on k_phot
double f_T(double temp)
{
  double Tx = temp <= Topt ? Tmin : Tmax;
  double f = (temp - Topt) / (Tx - Topt);
  return exp(-2.3 * (f * f));
}

// effect on k_resp
double f_T_resp(double temp)
{
  return pow(Q10, (temp - t_ref)/10);
}

// Phosphorus
double f_P(void)
{
  return pow(C_P, a_P)/(pow(C_P, a_P) + pow(CP50, a_P)) * KiP/(KiP + C_P);
}

// Nitrogen
double f_N(void)
{
  return pow(C_N, a_N)/(pow(C_N, a_N) + pow(CN50, a_N)) * KiN/(KiN + C_N);
}

// Biomass (crowding)
double f_BM(double bm)
{
  return (BM50 - bm)/BM50;
}

// Effect
double f_E(double C_active)
{
  return 1 - Emax * pow(C_active, b) / (pow(EC50, b) + pow(C_active, b));
}


/**
 * Derivatives
 */
void lemna_func(int *neq, double *t, double *y, double *ydot, double *yout, int*ip)
{
  if(*neq < 3)
    Rf_error("invalid number of state variables");
  if(threshold >= 0 && *neq < 4)
    Rf_error("threshold defined but AUC state variable missing");

  // Default values
  int TDMod = TD_direct; // todo get from parameters
  double k_E_in = 100;
  double k_E_out = 100;

  // Calculate internal toxicant concentrations from amount in biomass
  double BM_fresh = BM * BMw2BMd;
  double C_int = M_int / BM_fresh;
  double C_int_u = fabs(C_int / Kbm); // Unbound internal concentration

  // Calculate effective growth rate
  double k_phot_eff, k_resp_eff;
  if(k_phot_fix==0) {
    k_phot_eff = k_phot_max * f_R(actRad);
    k_phot_eff = k_phot_eff * f_T(actTemp);
    k_phot_eff = k_phot_eff * const_f_PN;
    k_phot_eff = k_phot_eff * f_BM(BM);
    k_resp_eff = k_resp*f_T_resp(actTemp);
  } else {
    k_phot_eff = k_phot_max;
    k_resp_eff = k_resp;
  }

  // Consider toxic effect
  double f_Eff = f_E(C_int_u);
  if(TDMod == TD_delayed) {  // delayed effects could be considered
    f_Eff = E;
  }
  k_phot_eff = k_phot_eff * f_Eff;
  // Reset photo degradation rate if exposure AUC threshold was exceeded
  if(threshold >= 0 && AUC >= threshold) {
    k_phot_eff = 0;
  }

  // Biomass
  dBMdt = BM * (k_phot_eff - k_resp_eff - k_loss);
  // let population extinct if less than one frond/m?
  //if(BM<5*mass_per_frond){dBMdt <- 0}

  // Effect  (this is the delayed TD model)
  if(TDMod == TD_delayed) {
    dEdt = k_E_in * f_E(C_int_u) - k_E_out * E;
  } else {
    dEdt = 0;
  }

  // TK part: Internal amount of toxicant
  double P_up_eff = P_up;
  if(P_Temp == 1) { // Temperature dependence of permeability
    P_up_eff = P_T(actTemp);
  }
  dM_intdt = P_up_eff * AperBM * BM * (actConc - C_int_u) - C_int * BM_fresh*(k_resp_eff + k_loss);

  // Only calculate exposure AUC if a threshold was defined
  if(*neq >= 4) {
    dAUC = threshold>=0 ? actConc : 0;
  }

  // Additional outputs, if requested
  if(*ip >= 1) {
    yout[0] = C_int;
  }
  if(*ip >= 2) {
    yout[1] = BM / mass_per_frond;
  }
  if(*ip >= 3) {
    yout[2] = C_int_u;
  }
  if(*ip >= 8) {
    yout[3] = BM_fresh;
    yout[4] = k_phot_eff;
    yout[5] = k_resp_eff;
    yout[6] = f_Eff;
    yout[7] = P_up_eff;
  }
  if(*ip >= 11) {
    yout[8] = actConc;
    yout[9] = actTemp;
    yout[10] = actRad;
  }
  if(*ip >= 14) {
    yout[11] = ydot[0];
    yout[12] = ydot[1];
    yout[13] = ydot[2];
  }
}
