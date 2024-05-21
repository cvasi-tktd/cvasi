#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <math.h>
/*******************************************************************
 *
 * DEBtox Daphnia model adapted from Matlab code
 *
 * Copyright (c) 2012-2021, Tjalling Jager
 * Copyright (c) 2021, Nils Kehrein
 *
 * This source code is licensed under the MIT-style license found in the
 * LICENSE_DEBtox.txt file in the package directory inst/licenses/
 *
 * The model provides additional output on intermediary variables on
 * request; please refer to deSolve's manual on the 'nout' parameter.
 *
 *******************************************************************/

/*
 * Allocate memory for global parameter array
 */
static double parms[22] = {0};

/*
 * Allocate memory for forcing function data
 * Array's values get updated by ODE solver in every time step.
 */
static double forc[1] = {0};

/*
 * Mode of Action switch and identifiers
 */
static unsigned short MoA;
static unsigned short moaFeeding            = 0x01;
static unsigned short moaMaintenance        = 0x02;
static unsigned short moaGrowth             = 0x04;
static unsigned short moaReproduction       = 0x08;
static unsigned short moaHazardReproduction = 0x10;

/*
 * Feedbacks to use on damage dynamics
 */
static unsigned short FB;
static unsigned short fbUptake       = 0x01;
static unsigned short fbElimination  = 0x02;
static unsigned short fbGrowth       = 0x04;
static unsigned short fbReproduction = 0x08;

/*
 * Aliases for state variable
 */
#define Dw y[0] // [C], scaled damage (referenced to water)
#define L  y[1] // mm, body length
#define R  y[2] // -, cumulative reproduction
#define S  y[3] // -, survival probability

/*
 * Aliases for derivatives
 */
#define dDw ydot[0] // 1/d, change in scaled damage
#define dL  ydot[1] // mm/d, change in body length
#define dRc ydot[2] // 1/d, cumulative reproduction rate
#define dS  ydot[3] // 1/d, change in survival probability (incl. background mort.)

/*
 * Aliases for parameters
 */
// parameters for basic life history
#define L0       parms[0] // mm, body length at start
#define Lp       parms[1] // mm, body length at puberty
#define Lm       parms[2] // mm, maximum body length
#define rB       parms[3] // 1/d, von Bertalanffy growth rate constant
#define Rm       parms[4] // #/d, maximum reproduction rate
#define param_f  parms[5] // -, scaled functional response
#define hb       parms[6] // 1/d, background hazard rate
// parameters for specific cases
#define Lf       parms[7] // mm, body length at half-saturation feeding
#define Lj       parms[8] // mm, body length at which acceleration stops
#define Tlag     parms[9] // d, lag time for start development
// parameters for the response to toxicants
#define param_kd parms[10] // 1/d, dominant rate constant
#define zb       parms[11] // [C], effect threshold energy budget
#define bb       parms[12] // 1/[C], effect strength energy-budget effects
#define zs       parms[13] // [C], effect threshold survival
#define bs       parms[14] // 1/([C] d), effect strength survival
// (formerly) global model parameters
#define FBV      parms[15] // -, dry weight of egg as fraction of dry body weight
#define KRV      parms[16] // kg/kg, part. coeff. repro buffer and structure
#define kap      parms[17] // -, approximation for kappa
#define yP       parms[18] // -, product of yVA and yAV
#define Lm_ref   parms[19] // mm, reference max length for scaling rate constants

/*
 * Aliases for forcing data
 */
#define c forc[0] // [C], external concentration

/*
 * Precalculated constants to speed up simulations
 */
static double Lf3 = 0;
static double Lp3 = 0;
static double Lm3 = 0;

/*
 * Parameter initializer
 */
void debtox_daphnia_init(void (*odeparms)(int*, double*))
{
  int N=22;
  odeparms(&N, parms);

  // Mode of Action switch
  MoA = (unsigned short)parms[20];
  // Feedback switch
  FB = (unsigned short)parms[21];
  // calculate some constants
  Lf3 = Lf*Lf*Lf;
  Lp3 = Lp*Lp*Lp;
  Lm3 = Lm*Lm*Lm;
}

/*
 * Forcings initializer
 */
void debtox_daphnia_forc(void (* odeforcs)(int *, double *))
{
  int N=1;
  odeforcs(&N, forc);
}

/*
 * Derivatives
 */
void debtox_daphnia_func(int *neq, double *t, double *y, double *ydot, double *yout, int*ip)
{
  // ensure the states cannot become negative due to numerical issues
  Dw = fmax(Dw,0);
  L = fmax(L,0);
  S = fmax(S,0);

  // Calculate the derivatives
  // This is the actual model, specified as a system of ODEs. This is the
  // DEBkiss model, with toxicant effects and starvation module, expressed in
  // terms of compound parameters, as presented in the publication (Jager, 2020).

  L = fmax(1e-3*L0,L); // make sure that body length is not negative or almost zero (extreme shrinking may do that)
  // This should not be needed as shrinking is limited at the bottom of this
  // function.

  // copy parameters that may get modified locally
  double f = param_f;
  double kd = param_kd;

  if(Lf > 0) { // to include feeding limitation for juveniles ...
    f  = f / (1+(Lf3)/(L*L*L)); // hyperbolic relationship for f with body volume
    // kd = kd*f; // also reduce dominant rate by same factor? (open for discussion!)
  }
  if(Lj > 0) { // to include acceleration until metamorphosis ...
    f = f * fmin(1,L/Lj); // this implies lower f for L<Lj
  }

  // calculate stress factor and hazard rate
  double s = bb*fmax(0,Dw-zb); // stress level for metabolic effects
  double h = bs*fmax(0,Dw-zs); // hazard rate for effects on survival

  // Define specific stress factors s*, depending on the mode of action
  double sA=0, sM=0, sG=0, sR=0, sH=0;
  if(MoA & moaFeeding)
    sA = fmin(1, s); // assimilation/feeding (maximise to 1 to avoid negative values for 1-sA)
  if(MoA & moaMaintenance)
    sM = s; // maintenance (somatic and maturity)
  if(MoA & moaGrowth)
    sG = s; // growth costs
  if(MoA & moaReproduction)
    sR = s; // reproduction costs
  if(MoA & moaHazardReproduction)
    sH = s; // also include hazard to reproduction

  // calculate the actual derivatives, with stress implemented
  dL = rB * ((1+sM)/(1+sG)) * (f*Lm*((1-sA)/(1+sM)) - L); // ODE for body length

  double fR = f; // if there is no starvation, f for reproduction is the standard f
  // starvation rules can modify the outputs here
  if(dL < 0) { // then we are looking at starvation and need to correct things
    fR = (f - kap * (L/Lm) * ((1+sM)/(1-sA)))/(1-kap); // new f for reproduction alone
    if(fR >= 0) { // then we are in the first stage of starvation: 1-kappa branch can help pay maintenance
      dL = 0;     // stop growth, but don't shrink
    } else {      // we are in stage 2 of starvation and need to shrink to pay maintenance
      fR = 0;     // nothing left for reproduction
      dL = (rB*(1+sM)/yP) * ((f*Lm/kap)*((1-sA)/(1+sM)) - L); // shrinking rate
    }
  }

  R = 0; // reproduction rate is zero, unless ...
  if(L >= Lp) { // if we are above the length at puberty, reproduce
    R = fmax(0,(exp(-sH)*Rm/(1+sR)) * (fR*Lm*(L*L)*(1-sA) - (Lp3)*(1+sM))/(Lm3 - Lp3));
    // Note: hazard to reproduction added with sH
  }

  dRc = R;             // cumulative reproduction rate
  dS  = -(h + hb) * S; // change in survival probability (incl. background mort.)

  // For the damage dynamics, there are four feedback factors x* that obtain a
  // value based on the settings in the configuration vector glo.feedb: a
  // vector with switches for various feedbacks: [surface:volume on uptake,
  // surface:volume on elimination, growth dilution, losses with
  // reproduction].

  // if switch for surf:vol scaling is zero, the factor must be 1 and not 0!
  double xu=0, xe=0, xG=0, xR=0;
  if(FB & fbUptake)
    xu = Lm_ref/L; // factor for surf:vol scaling uptake rate
  if(FB & fbElimination)
    xe = Lm_ref/L; // factor for surf:vol scaling elimination rate
  if(FB & fbGrowth)
    xG = (3/L)*dL; // factor for growth dilution
  if(FB & fbReproduction)
    xR = R*FBV*KRV; // factor for losses with repro

  // if switch for surf:vol scaling is zero, the factor must be 1 and not 0!
  // Note: this was previously done with a max-to-1 command. However, that is
  // not a good idea in combination with Lm_ref (which is not necessarily
  // equal to or larger than Lm).
  if(!(FB & fbUptake))
    xu = 1;
  if(!(FB & fbElimination))
    xe = 1;

  xG = fmax(0, xG); // stop reverse growth dilution
  // NOTE: reverse growth dilution (concentration by shrinking) is now
  // turned OFF as it leads to runaway situations that lead to failure of the
  // ODE solvers. However, this needs some further thought!

  dDw = kd * (xu * c - xe * Dw) - (xG + xR) * Dw; // ODE for scaled damage

  if(L <= 0.5 * L0) // if an animal has size less than half the start size ...
    dL = 0; // don't let it grow or shrink any further (to avoid numerical issues)

  if(*t < Tlag) { // when we are NOT past the lag time ...
    dDw = dL = dRc = dS = 0; // return zeros
  }

  // Additional model outputs, if requested by user
  // response function
  if(*ip>=3) {
    yout[0] = f;
    yout[1] = fR;
    yout[2] = kd;
  }
  // hazard and stress
  if(*ip>=6) {
    yout[3] = s;
    yout[4] = sA;
    yout[5] = h;
  }
  // damage feedbacks
  if(*ip>=10) {
    yout[6] = xu;
    yout[7] = xe;
    yout[8] = xG;
    yout[9] = xR;
  }
}
