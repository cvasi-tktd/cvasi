#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <math.h>
/*******************************************************************
 *
 * General Unified Threshold model of Survival, Full Stochastic Death
 * (GUTS-SD) model with damage level as dose metric.
 *
 * Equations and parameter names were chosen according to Jager et al.
 * (2011), DOI: 10.1021/es103092a
 *
 * The model provides the external concentration in water as additional
 * output on request; please refer to deSolve's manual on the 'nout'
 * parameter.
 *
 *******************************************************************/

/*
 * Allocate memory for global parameter array
 */
static double parms[6] = {0};
static int M_id = 0;

/*
 * Allocate memory for forcing function data
 *
 * Its value gets updated by ODE solver in every time step.
 */
static double forc[1] = {0};

/*
 * Define aliases for global objects to ease code reading
 *
 * Order of passed parameters is a convention that needs to
 * followed by the calling R code.
 */
#define ki parms[0] // accumulation rate into body (equal to `ke * Kiw` for scaled Ci)
#define ke parms[1] // elimination rate from body
#define kr parms[2] // damage recovery rate
#define kk parms[3] // killing rate constant
#define hb parms[4] // background hazard rate
#define z  parms[5] // threshold for effects

#define Cw forc[0]  // external concentration

#define Ci y[0] // internal concentration
#define D y[1]  // damage level
#define H y[2]  // cumulative hazard

/*
 * Parameter initializer
 */
void gutssd_init(void (* odeparms)(int *, double *))
{
  int N = 7;
  odeparms(&N, parms);

  // Dose metric selection switch
  M_id = (int)parms[6];
  // Other parameters, may be NaN
  if(ki < 0)
    Rf_error("invalid argument: ki is smaller than zero");
  if(ke < 0)
    Rf_error("invalid argument: ke is smaller than zero");
  if(kr < 0)
    Rf_error("invalid argument: kr is smaller than zero");
  if(kk < 0)
    Rf_error("invalid argument: kk is smaller than zero");
  if(hb < 0)
    Rf_error("invalid argument: hb is smaller than zero");
  if(z < 0)
    Rf_error("invalid argument: z is smaller than zero");
}

/*
 * Forcings initializer
 */
void gutssd_forc(void (* odeforcs)(int *, double *))
{
  int N = 1;
  odeforcs(&N, forc);
}

/*
 * Derivatives
 */
void gutssd_func(int *neq, double *t, double *y, double *ydot, double *yout, int *ip)
{
  if(*neq != 3)  {
    Rf_error("invalid number of state variables");
  }

  // dCi/dt
  ydot[0] = ki * Cw - ke * Ci;
  // dD/dt
  ydot[1] = kr * (Ci - D);

  // Select the dose metric
  double M = 0;
  switch(M_id) {
    case 0: M = D; break;
    case 1: M = Ci; break;
    case 2: M = Cw; break;
    default:
      Rf_error("invalid dose metric selected");
  }

  // dH/dt
  ydot[2] = kk * fmax(0, M - z) + hb;

  // additional output variables requested (nout > 0)?
  if(*ip > 0)
    yout[0] = Cw;
}
