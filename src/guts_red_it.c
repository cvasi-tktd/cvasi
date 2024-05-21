#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <math.h>
/*******************************************************************
 *
 * General Unified Threshold model of Survival,
 * REDuced Individual Threshold (GUTS-RED-IT)
 *
 * The model provides the external concentration in water as additional
 * output on request; please refer to deSolve's manual on the 'nout'
 * parameter.
 *
 *******************************************************************/

/*
 * Allocate memory for global parameter array
 */
static double parms[2] = {0};

/*
 * Allocate memory for forcing function data
 *
 * Its value gets updated by ODE solver in every time step.
 */
static double forc[1] = {0};

/*
 * Define aliases for global objects to ease code reading
 *
 * Order of passed parameters kd and hb is a convention that needs to
 * followed by the calling R code.
 */
#define kd parms[0]
#define hb parms[1]
#define Cw forc[0]

/*
 * Parameter initializer
 */
void gutsredit_init(void (* odeparms)(int *, double *))
{
  int N = 2;
  odeparms(&N, parms);

  if(kd < 0)
    Rf_error("invalid argument: kd is smaller than zero");
  if(hb < 0)
    Rf_error("invalid argument: hb is smaller than zero");
}

/*
 * Forcings initializer
 */
void gutsredit_forc(void (* odeforcs)(int *, double *))
{
  int N = 1;
  odeforcs(&N, forc);
}

/*
 * Derivatives
 */
void gutsredit_func(int *neq, double *t, double *y, double *ydot, double *yout, int*ip)
{
  // check if exposure series provides valid data
  if(Cw < 0)
    Rf_error("invalid forcings data: exposure concentration is smaller than zero");

  // dDw/dt
  ydot[0] = kd * (Cw - y[0]);
  // dH/dt
  ydot[1] = hb;

  // additional output variables requested (nout > 0)?
  if(*ip > 0)
    yout[0] = Cw;
}
