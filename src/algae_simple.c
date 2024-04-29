#include <R.h>
#include <Rinternals.h>
#include <math.h>
#include <stdio.h>

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
static double parms[6];
/**
 * Allocate memory for forcing function data
 *
 * Array values get updated by the ODE solver in every time step.
 */
static double forc[2];
/*
 * Constant helper value
 */
static double log_EC50;
/*
 * Define aliases
 */
// State variable aliases
#define A y[0]
#define Dw y[1]

// Derivative aliases
#define dA ydot[0]
#define dDw ydot[1]

// Parameter aliases
// Growth model parameters
#define mu_max parms[0]
// Toxicodynamic parameters
#define EC_50 parms[1]
#define b parms[2]
// Toxicokinetic parameters
#define kD parms[3]
#define scaled ((parms[4] != 0.0) ? 1 : 0) //0 = no, 1 = yes
// dose-response option
#define dose_response ((parms[5] != 0.0) ? 1 : 0) //0 = logit, 1 = probit
// Forcings by environmental variables
#define Cw    forc[0]
#define f_growth forc[1] //if constant growth was not observed in lab study

/**
 * Parameter initializer
 */
void algae_simple_init(void (*odeparms)(int *, double *))
{
  int N = 6;
  odeparms(&N, parms);

  log_EC50 = log(EC_50);
}

/**
 * Forcings initializer
 */
void algae_simple_forc(void (*odeforcs)(int *, double *))
{
    int N = 2;
    odeforcs(&N, forc);
}

/*
 * Functions for calculating reduction factors for growth rate
 */

// Effect of concentration on growth using probit (OECD 54, page 67)
double f_C_probit(double C_param) {
  //return gsl_cdf_ugaussian_P(-b * (log(C_param) - log(EC_50)));
  //Function to calculate the cumulative distribution function (CDF) of the standard normal distribution
  double z = -b * (log(C_param) - log_EC50);
  return 0.5 * erfc(-z / sqrt(2));
}

// Effect of concentration on growth using logit (OECD 54, page 67)
double f_C_logit(double C_param)
{
  return 1 / (1 + exp(b * (log(C_param) - log_EC50)));
}

/**
 * Derivatives
 */
void algae_simple_func(int *neq, double *t, double *y, double *ydot, double *yout, int *ip)
{

  if(scaled == 1) {
    // Scaled damage concentration
    dDw = kD * (Cw - Dw);
    // Biomass
    if(dose_response == 0) {
      dA = (mu_max * f_growth * f_C_logit(Dw)) * A;
    } else {
      dA = (mu_max * f_growth * f_C_probit(Dw)) * A;
    }
  } else {
    // Biomass
    dDw = 0;
    if(dose_response == 0) {
      dA = (mu_max * f_growth * f_C_logit(Cw)) * A;
    } else {
      dA = (mu_max * f_growth * f_C_probit(Cw)) * A;
    }
  }

  if (Dw < 0) {
    Dw = 0;
  }

  // derivatives as additional output
  if(*ip >= 2) {
    yout[0] = dA;
    yout[1] = dDw;
  }
  // settings as additional output
  if(*ip >= 6) {
    yout[2] = dose_response;
    yout[3] = scaled;
    yout[4] = f_growth;
    yout[5] = log(A);
  }
}
