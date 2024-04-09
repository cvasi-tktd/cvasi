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
static double parms[14];
/**
 * Allocate memory for forcing function data
 *
 * Array values get updated by the ODE solver in every time step.
 */
static double forc[3];
/*
 * Define aliases
 */
// State variable aliases
#define A y[0]
#define Q y[1]
#define P y[2]
#define Dw y[3]

// Derivative aliases
#define dA ydot[0]
#define dQ ydot[1]
#define dP ydot[2]
#define dDw ydot[3]

// Parameter aliases
// Growth model parameters
#define mu_max parms[0]
#define m_max parms[1]
#define v_max parms[2]
#define k_s parms[3]
#define Q_min parms[4]
#define Q_max parms[5]
#define T_opt parms[6]
#define T_min parms[7]
#define T_max parms[8]
#define I_opt parms[9]
// Toxicodynamic parameters
#define EC_50 parms[10]
#define b parms[11]
// Toxicokinetic parameters
#define kD parms[12]
#define dose_resp parms[13]  // 0 = logit, 1 = probit
// Forcings by environmental variables
#define Cw    forc[0]
#define I     forc[1]
#define T_act forc[2]

/**
 * Parameter initializer
 */
void algae_TKTD_init(void (*odeparms)(int *, double *))
{
  int N = 14;
  odeparms(&N, parms);
}

/**
 * Forcings initializer
 */
void algae_TKTD_forc(void (*odeforcs)(int *, double *))
{
  int N = 3;
  odeforcs(&N, forc);
}

/*
 * Functions for calculating reduction factors for growth rate
 */
// Temperature effect on mu_max
double f_temp_TKTD(double temp)
{
  double f = (temp < T_opt) ? (temp - T_opt) / (T_min - T_opt) : (temp - T_opt) / (T_max - T_opt);
  return exp(-2.3 * pow(f, 2));
}

// Irradiance effect on mu_max
double f_I_TKTD(double I_param)
{
  return I_param / I_opt * exp(1 - (I_param / I_opt));
}

// Phosphorus effect on mu_max
double f_Q_TKTD(double Q_param, double A_param)
{
  return (1 - exp(-log(2)*((Q_param / (Q_min * A_param)) - 1)));
}

// Effect on internal phosphorus
double f_Q_P_TKTD(double A_param, double Q_param, double P_param)
{
  return (((Q_max * A_param - Q_param) / ((Q_max - Q_min) * A_param)) * (P_param / (k_s + P_param)));
}

// Probit function
double probit_TKTD(double x) {
  return 0.5 * (1 + erf(x / sqrt(2.0)));
}
// Logit_TKTD function
double f_Dw_logit(double Dw_param)
{
  return 1 - (1 / (1 + (pow(Dw_param / EC_50, -b))));
}

// Effect of concentration on growth using probit
double f_Dw_probit(double Dw_param) {
  return 1 - probit_TKTD((log(Dw_param / EC_50) + b) / sqrt(2.0));
}

// Effect of concentration on growth
//double f_Dw(double Dw_param)
//{
//  return 1 / (1 + ((Dw_param / EC_50) * exp(-b))); //ToDo change to probit
//}

/**
 * Derivatives
 */
void algae_TKTD_func(int *neq, double *t, double *y, double *ydot, double *yout, int *ip)
{

  // Biomass
  if(dose_resp == 0) {
    dA = (mu_max * f_temp_TKTD(T_act) * f_I_TKTD(I) * f_Q_TKTD(Q, A) * f_Dw_logit(Dw) - m_max) * A;
  } else {
    dA = (mu_max * f_temp_TKTD(T_act) * f_I_TKTD(I) * f_Q_TKTD(Q, A) * f_Dw_probit(Dw) - m_max) * A;
  }

  // Internal concentration of P
  dQ = v_max * f_Q_P_TKTD(A, Q, P) * A - (m_max) * Q;

  // External concentration of P
  dP = P + Q * m_max - (v_max * f_Q_P_TKTD(A, Q, P) * A);

  // Actual concentration
  //dC = C_in * D - k * C - D * C;

  // TKTD damage concentration
  dDw = kD * (Cw - Dw);

  // Store forcing values in yout
  // derivatives as additional output
  if(*ip >= 3) {
    yout[0] = Cw;
    yout[1] = I;
    yout[2] = T_act;
  }
  if(*ip >= 7) {
    yout[3] = dA;
    yout[4] = dQ;
    yout[5] = dP;
    yout[6] = dDw;
  }
}
