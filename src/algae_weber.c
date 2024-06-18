#include <R.h>
#include <Rinternals.h>
#include <math.h>

/*******************************************************************
 *
 * Algae is a simplified model for the growth of algae as described
 * by Weber (2012)
 *
 * The model provides no additional output on intermediary variables on
 * request
 *
 *******************************************************************/

/**
 * Allocate memory for global parameter array
 */
static double parms[15] = {0};
/**
 * Allocate memory for forcing function data
 *
 * Array values get updated by the ODE solver in every time step.
 */
static double forc[3] = {0};
/*
 * Define aliases
 */
// State variable aliases
#define A y[0]
#define Q y[1]
#define P y[2]
#define C y[3]

// Derivative aliases
#define dA ydot[0]
#define dQ ydot[1]
#define dP ydot[2]
#define dC ydot[3]

// Parameter aliases
// Growth model parameters
#define mu_max parms[0]
#define m_max parms[1]
#define v_max parms[2]
#define k_s parms[3]
#define Q_min parms[4]
#define Q_max parms[5]
#define R_0 parms[6]
#define D parms[7]
#define T_opt parms[8]
#define T_min parms[9]
#define T_max parms[10]
#define I_opt parms[11]
// Toxicodynamic parameters
#define EC_50 parms[12]
#define b parms[13]
// Toxicokinetic parameters
#define k parms[14]
//#define I parms[15]
//#define T_act parms[16]
// Forcings by environmental variables
#define C_in  forc[0]
#define I     forc[1]
#define T_act forc[2]

/**
 * Parameter initializer
 */
void algae_init(void (*odeparms)(int *, double *))
{
  int N = 15;
  odeparms(&N, parms);
}

/**
 * Forcings initializer
 */
void algae_forc(void (*odeforcs)(int *, double *))
{
  int N = 3;
  odeforcs(&N, forc);
}

/*
 * Functions for calculating reduction factors for growth rate
 */
// Temperature effect on mu_max
double f_temp(double temp)
{
  double f = (temp < T_opt) ? (temp - T_opt) / (T_min - T_opt) : (temp - T_opt) / (T_max - T_opt);
  return exp(-2.3 * pow(f, 2));
}

// Irradiance effect on mu_max
double f_I(double I_param)
{
  return I_param / I_opt * exp(1 - (I_param / I_opt));
}

// Phosphorus effect on mu_max
double f_Q(double Q_param, double A_param)
{
  return (1 - exp(-log(2)*((Q_param / (Q_min * A_param)) - 1)));
}

// Effect on internal phosphorus
double f_Q_P(double A_param, double Q_param, double P_param)
{
  return (((Q_max * A_param - Q_param) / ((Q_max - Q_min) * A_param)) * (P_param / (k_s + P_param)));
}

// Effect of concentration on growth
double f_C(double C_param)
{
  return 1 - (1 / (1 + (pow(C_param / EC_50, -b))));
}

/**
 * Derivatives
 */
void algae_func(int *neq, double *t, double *y, double *ydot, double *yout, int *ip)
{

  // Biomass
  dA = (mu_max * f_temp(T_act) * f_I(I) * f_Q(Q, A) * f_C(C_in) - m_max - D) * A;
  //printf("f_Q: %f, A: %f\n", f_Q(A, Q), A);

  // Internal concentration of P
  dQ = v_max * f_Q_P(A, Q, P) * A - (m_max + D) * Q;

  // External concentration of P
  dP = D * R_0 - D * P + Q * m_max - (v_max * f_Q_P(A, Q, P) * A);

  // Actual concentration
  dC = C_in * D - k * C - D * C;

  // derivatives as additional output
  if(*ip >= 4) {
    yout[0] = dA;
    yout[1] = dQ;
    yout[2] = dP;
    yout[3] = C;
  }
}
