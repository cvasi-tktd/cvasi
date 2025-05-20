#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <math.h>
/*******************************************************************
 *
 * General Unified Threshold model of Survival, Individual Tolerance
 * Distribution (GUTS-IT) model.
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
static double parms[5] = {0};
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
#define hb parms[3] // background hazard rate

#define Cw forc[0]  // external concentration

#define Ci y[0] // internal concentration
#define D y[1]  // damage level
#define H y[2]  // cumulative hazard

/*
 * Parameter initializer
 */
void gutsit_init(void (* odeparms)(int *, double *))
{
  int N = 5;
  odeparms(&N, parms);

  // Dose metric selection switch
  M_id = (int)parms[4];
  // Other parameters, may be NaN
  if(ki < 0)
    Rf_error("invalid argument: ki is smaller than zero");
  if(ke < 0)
    Rf_error("invalid argument: ke is smaller than zero");
  if(kr < 0)
    Rf_error("invalid argument: kr is smaller than zero");
  if(hb < 0)
    Rf_error("invalid argument: hb is smaller than zero");
}

/*
 * Forcings initializer
 */
void gutsit_forc(void (* odeforcs)(int *, double *))
{
  int N = 1;
  odeforcs(&N, forc);
}

/*
 * Derivatives
 */
void gutsit_func(int *neq, double *t, double *y, double *ydot, double *yout, int *ip)
{
  if(*neq != 4)  {
    Rf_error("invalid number of state variables");
  }

  // dCi/dt
  ydot[0] = ki * Cw - ke * Ci;
  // dD/dt
  ydot[1] = kr * (Ci - D);
  // dH/dt
  ydot[2] = hb;

  // Select the dose metric
  double M = 0, Mdot = 0;
  switch(M_id) {
    case 0: M = D; Mdot = ydot[1]; break;
    case 1: M = Ci; Mdot = ydot[0]; break;
    case 2: M = Cw; break;
    default:
     Rf_error("invalid dose metric selected");
  }

  // Cumulative maximum of M(t) expressed as an ODE
  ydot[3] = 0;
  if(y[3] <= M) {
    ydot[3] = fmax(Mdot, M - y[3]);
  }

  // additional output variables requested (nout > 0)?
  if(*ip > 0)
    yout[0] = Cw;
  if(*ip > 1)
    yout[1] = M;
}
