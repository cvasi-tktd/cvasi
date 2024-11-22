/*******************************************************************
 *
 * Lemna model as described by Klein et al. (2022)
 * SETAC Europe Interest Group Effect Modeling. Version 1.1
 *
 * The model provides additional output on intermediary variables on
 * request; please refer to deSolve's manual on the 'nout' parameter.
 *
 *******************************************************************/
#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <math.h>

/**
 * Allocate memory for global parameter array
 */
static double parms[24] = {0};
/**
 * Allocate memory for forcing function data
 *
 * Array's' values get updated by ODE solver in every time step.
 */
static double forc[5] = {0};

/*
 * Define aliases
 */
// state variable aliases
#define BM    y[0]
#define M_int y[1]

// derivative aliases
#define dBM    ydot[0]
#define dM_int ydot[1]

// parameter aliases
// growth model parameters
#define k_photo_fixed parms[0]
#define k_photo_max   parms[1]
#define k_loss        parms[2]
#define BM_min        parms[3]
// response parameters
#define T_opt         parms[4]
#define T_min         parms[5]
#define T_max         parms[6]
#define Q10           parms[7]
#define T_ref         parms[8]
#define alpha         parms[9]
#define beta          parms[10]
#define N_50          parms[11]
#define P_50          parms[12]
#define BM_L          parms[13]
// toxicodynamic parameters
#define E_max         parms[14]
#define EC50_int      parms[15]
#define b             parms[16]
// toxicokinetic parameters
#define P             parms[17]
#define r_A_DW        parms[18]
#define r_FW_DW       parms[19]
#define r_FW_V        parms[20]
#define r_DW_FN       parms[21]
#define K_pw          parms[22]
#define k_met         parms[23]
// forcings by environmental variables
#define C_ext forc[0]
#define Tmp forc[1]
#define Irr forc[2]
#define Phs forc[3]
#define Ntr forc[4]


/**
 * Parameter initializer
 */
void lemna_setac_init(void (* odeparms)(int *, double *))
{
  int N=24;
  odeparms(&N, parms);

}

/**
 * Forcings initializer
 */
void lemna_setac_forc(void (* odeforcs)(int *, double *))
{
  int N=5;
  odeforcs(&N, forc);
}


/* Temperature response of biomass loss rate (Box 5)
 * @param Tmp temperature (°C)
 * @param Q10 temperature coefficient (-)
 * @param T_ref ref temperature for response=1 (°C)
 * @return value from the interval [0,1]
 */
double fT_loss(void) {
  return(pow(Q10, (Tmp - T_ref) / 10));
}

/* Temperature response of photosynthesis (Box 4)
 * @param Tmp temperature (°C)
 * @param T_opt optimum growth temperature (°C)
 * @param T_min minimum growth temperature (°C)
 * @param T_max maximum growth temperature (°C)
 * @return value from the interval [0,1]
 */
double fT_photo(void) {
  double T_m = Tmp <= T_opt ? T_min : T_max;
  return(pow(10, -(Tmp - T_opt)*(Tmp - T_opt) / ((T_m - T_opt)*(T_m - T_opt))));
}

/* Irradiance response of photosynthesis (Box 6)
 * @param Irr irradiance (kJ m-2 d-1)
 * @param alpha slope of irradiance response of photosynthesis (m2 d kJ-1)
 * @param beta intercept of irradiance response of photosynthesis (-)
 * @return value from the interval [0,1]
 */
double fI_photo(void) {
  return(fmin(1, alpha * Irr + beta));
}

/* Nitrogen response of photosynthesis (Box 7)
 * @param Ntr nitrogen concentration (mg N L-1)
 * @param N_50 half-saturation constant of Nitrogen response (mg N L-1)
 * @return value from the interval [0,1]
 */
double fN_photo(void) {
  return(Ntr / (Ntr + N_50));
}

/* Phosphorus response of photosynthesis (Box 7)
 * @param Phs phosphorus concentration (mg P L-1)
 * @param P_50 half-saturation constant of Phosphorus response (mg P L-1)
 * @return value from the interval [0,1]
 */
double fP_photo(void) {
  return(Phs / (Phs + P_50));
}

/* Density dependence of photosynthesis (Box 8)
 * @param BM biomass (g dw m-2)
 * @param BM_L carrying capacity (g dw m-2)
 * @return value from the interval [0,1]
 */
double fBM_photo(double bm) {
  return(1 - bm / BM_L);
}

/* Concentration response of photosynthesis [Toxicodynamics] (Box 9)
 * @param C_int internal toxicant concentration (mass per volume, e.g. ug L-1)
 * @param E_max maximum inhibition (-)
 * @param EC50_int int. conc. resulting in 50% effect (mass per volume, e.g. ug L-1)
 * @param b slope parameter (-)
 * @return value from the interval [0,1]
 */
double fCint_photo(double C_int) {
  double pow_C_int_b = pow(C_int, b);
  return(1 - E_max * pow_C_int_b / (pow(EC50_int, b) + pow_C_int_b));
}

/**
 * Derivatives
 */
void lemna_setac_func(int *neq, double *t, double *y, double *ydot, double *yout, int*ip)
{
  if(*neq != 2) {
    Rf_error("invalid number of state variables");
  }

  // Respiration dependency function (Box 3)
  double f_loss;
  if(k_photo_fixed) { // unlimited growth conditions
    f_loss = 1;
  } else {
    f_loss = fT_loss();
  }

  //
  // Toxicokinetics
  //

  // Internal toxicant concentration (ug L-1) (Box 10)
  double C_int, C_int_unb;
  if(BM <= 0) { // avoid division by zero
    C_int = 0;
    C_int_unb = 0;
  } else {
    C_int = M_int * r_FW_V / (BM * r_FW_DW);
    C_int_unb = C_int / K_pw; // unbound internal concentration
  }

  // TK model ODE (Box 10)
  dM_int = P * BM * r_A_DW * (C_ext - C_int_unb) -
           M_int / K_pw * k_met - M_int * k_loss * f_loss;

  //
  // Effects on photosynthesis
  //

  // Photosynthesis dependency function including Liebig's Law (Box 2)
  double f_photo;
  if(k_photo_fixed) { // unlimited growth conditions, except exposure effects
    f_photo = fCint_photo(C_int_unb);
  } else {
    f_photo = fmin(fT_photo(),
              fmin(fI_photo(),
              fmin(fP_photo(),
                   fN_photo()
              ))) * fBM_photo(BM) * fCint_photo(C_int_unb);
  }

  //
  // Population growth
  //

  // Growth model ODE (Box 1)
  dBM = (k_photo_max * f_photo - k_loss * f_loss) * BM;
  // avoid biomass decrease below BM_min
  if(BM <= BM_min && dBM < 0) {
    dBM = 0;
  }

  // Additional model outputs, if requested by user
  if(*ip > 0)
  {
    if(*ip > 0) yout[0] = C_int; // internal concentration
    if(*ip > 1) yout[1] = BM/ r_DW_FN; // Frond number
	  // lumped response functions
    if(*ip > 2) yout[2] = f_loss;
    if(*ip > 3) yout[3] = f_photo;
	  // individual response functions
    if(*ip > 4) yout[4] = fT_photo();
    if(*ip > 5) yout[5] = fI_photo();
    if(*ip > 6) yout[6] = fP_photo();
    if(*ip > 7) yout[7] = fN_photo();
    if(*ip > 8) yout[8] = fBM_photo(BM);
    if(*ip > 9) yout[9] = fCint_photo(C_int_unb);
    // environmental variables
    if(*ip > 10) yout[10] = C_int_unb;
    if(*ip > 11) yout[11] = C_ext;
    if(*ip > 12) yout[12] = Tmp;
    if(*ip > 13) yout[13] = Irr;
    if(*ip > 14) yout[14] = Phs;
    if(*ip > 15) yout[15] = Ntr;
    // derivatives
    if(*ip > 16) yout[16] = dBM;
    if(*ip > 17) yout[17] = dM_int;
  }
}
