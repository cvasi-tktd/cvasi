#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <math.h>
/*******************************************************************
 *
 * Simplified DEBtox model as described by Jager (2020). The code has
 * been adapted from the BYOM module "DEBtox2019", version 4.7,
 * published on 13 Dec 2022. It implements optional features of the
 * 'ERA_special' model variant which supports a reference Lm, which
 * is needed to properly compare different data sets.
 *
 * Copyright (c) 2012-2024, Tjalling Jager
 * Copyright (c) 2024, Nils Kehrein
 *
 * This source code is licensed under the MIT-style license found in the
 * debtox_license.txt file in the package directory.
 *
 * Literature:
 * Jager T, 2020: Revisiting simplified DEBtox models for analysing
 * ecotoxicity data. Ecol. Model. 416. DOI: 10.1016/j.ecolmodel.2019.108904
 *
 * Romoli et al., 2024: Environmental risk assessment with energy budget
 * models: a comparison between two models of different complexity.
 * Environ Toxicol Chem 43(2):440-449. DOI: 10.1002/etc.5795
 *
 *******************************************************************/

/*
 * Array to store parameter values.
 */
static double parms[23] = {0};

/*
 * Array to store forcing function data.
 * The array gets updated by the ODE solver in every time step.
 */
static double forc[1] = {0};

/*
 * Aliases for state variable
 */
#define Dw y[0] // scaled damage (referenced to water) ([C])
#define L  y[1] // body length (mm)
#define R  y[2] // cumulative reproduction (-)
#define S  y[3] // survival probability (-)

/*
 * Aliases for derivatives
 */
#define dDw ydot[0] // change in scaled damage ([C]/d)
#define dL  ydot[1] // change in body length (mm/d)
#define dRc ydot[2] // cumulative reproduction rate (1/d)
#define dS  ydot[3] // change in survival probability (incl. background mort.) (1/d)

/*
 * Aliases for static parameters
 */
// parameters for basic life history
#define L0       parms[0] // body length at start (mm)
#define Lp       parms[1] // body length at puberty (mm)
#define Lm       parms[2] // maximum body length (mm)
#define rB       parms[3] // von Bertalanffy growth rate constant (1/d)
#define Rm       parms[4] // maximum reproduction rate (#/d)
#define f_orig   parms[5] // scaled functional response (-)
#define hb_orig  parms[6] // background hazard rate (d-1)
#define a        parms[7] // Weibull background hazard coefficient (-)
// parameters for specific cases
#define Lf       parms[8] // body length at half-saturation feeding (mm)
#define Lj       parms[9] // body length at which acceleration stops (mm)
#define Tlag     parms[10] // lag time for start development (d)
// parameters for the response to toxicants
#define kd_orig  parms[11] // dominant rate constant (1/d)
#define zb       parms[12] // effect threshold energy budget ([C])
#define bb       parms[13] // effect strength energy-budget effects (1/[C])
#define zs       parms[14] // effect threshold survival ([C])
#define bs       parms[15] // effect strength survival (1/([C] d))
// (formerly global) model parameters
#define FBV      parms[16] // dry weight of egg as fraction of dry body weight (-)
#define KRV      parms[17] // part. coeff. repro buffer and structure (kg/kg)
#define kap      parms[18] // approximation for kappa (-)
#define yP       parms[19] // product of yVA and yAV (-)
#define Lm_ref   parms[20] // product of yVA and yAV (-)
#define MoA (unsigned short)(parms[21]) // Mode of Action bit array
#define FB  (unsigned short)(parms[22]) // Damage feedback bit array
/*
 Mode of action switches [0=off, 1=on]
  1st bit: assimilation/feeding
  2nd bit: maintenance (somatic and maturity)
  3rd bit: growth costs
  4th bit: reproduction costs
  5th bit: hazard to reproduction

 Feedback on damage dynamic switches  [0=off, 1=on]
  1st bit: surf:vol scaling uptake rate
  2nd bit: surf:vol scaling elimination rate
  3rd bit: growth dilution
  4th bit: losses with repro
*/

/*
 * Aliases for forcing data
 */
#define c forc[0] // external concentration ([C])

/*
 * Precalculated constants to speed up simulations
 */
static double Lf3 = 0;
static double Lp3 = 0;
static double Lm3 = 0;

/*
 * Parameter initializer
 */
void debtox_init(void (*odeparms)(int*, double*))
{
  int N=23;
  odeparms(&N, parms);

  // Is a reference Lm provided? If not, set parameter value to Lm to disable
  // this feature of the 'ERA_special' model variant
  if(Lm_ref <= 0) {
	  Lm_ref = Lm;
  }

  // calculate some constants
  Lf3 = Lf * Lf * Lf;
  Lp3 = Lp * Lp * Lp;
  Lm3 = Lm * Lm * Lm;
}

/*
 * Forcings initializer
 */
void debtox_forc(void (* odeforcs)(int *, double *))
{
  int N=1;
  odeforcs(&N, forc);
}

/*
 * Derivatives
 *
 * @param *neq number of state variables
 * @param *t time point
 * @param *y (values of) state variables
 * @param *ydot derivatives of state variables
 * @param *yout additional output variables
 * @param *ip number of (requested) output variables
 */
void debtox_func(int *neq, double *t, double *y, double *ydot, double *yout, int *ip)
{
  // ensure the states cannot become negative due to numerical issues
  Dw = fmax(Dw, 0);
  L = fmax(L, 0);
  S = fmax(S, 0);

  double hb = hb_orig;
  // option for Weibull mortality when a is not 1
  if(a != 1) {
    hb = a * pow(hb, a) * pow(*t, a - 1);
  }

  // Calculate the derivatives
  // This is the actual model, specified as a system of ODEs. This is the
  // DEBkiss model, with toxicant effects and starvation module, expressed in
  // terms of compound parameters, as presented in the publication (Jager, 2020).

  L = fmax(1e-3 * L0, L); // make sure that body length is not negative or almost zero (extreme shrinking may do that)
  // This should not be needed as shrinking is limited at the bottom of this
  // function.

  // copy parameters that may get modified locally
  double f = f_orig;
  double kd = kd_orig;

  if(Lf > 0) { // to include feeding limitation for juveniles ...
    f  = f / (1 + (Lf3) / (L * L * L)); // hyperbolic relationship for f with body volume
    // kd = kd * f; // also reduce dominant rate by same factor? (open for discussion!)
  }
  if(Lj > 0) { // to include acceleration until metamorphosis ...
    f = f * fmin(1, L / Lj); // this implies lower f for L<Lj
  }

  // calculate stress factor and hazard rate.
  double s = bb * fmax(0, Dw - zb); // stress level for metabolic effects
  double h = bs * fmax(0, Dw - zs); // hazard rate for effects on survival

  h = fmin(111, h); // maximise the hazard rate to 99% mortality in 1 hour
  // Note: this helps in extreme conditions, as the system becomes stiff for
  // very high hazard rates. This is especially needed for EPx calculations,
  // where the MF is increased until there is effect on all endpoints!

  // Define specific stress factors s*, depending on the mode of action
  double sA=0, sM=0, sG=0, sR=0, sH=0;
  if(MoA & 0x01) {
    sA = fmin(1, s); // assimilation/feeding (maximise to 1 to avoid negative values for 1-sA)
  }
  if(MoA & 0x02) {
    sM = s; // maintenance (somatic and maturity)
  }
  if(MoA & 0x04) {
    sG = s; // growth costs
  }
  if(MoA & 0x08) {
    sR = s; // reproduction costs
  }
  if(MoA & 0x10) {
    sH = s; // also include hazard to reproduction
  }

  // Calculate the actual derivatives, with stress implemented.
  dL = rB * ((1 + sM) / (1 + sG)) * (f * Lm * ((1 - sA) / (1 + sM)) - L); // ODE for body length

  double fR = f; // if there is no starvation, f for reproduction is the standard f
  // starvation rules can modify the outputs here
  if(dL < 0) { // then we are looking at starvation and need to correct things
    fR = (f - kap * (L / Lm) * ((1 + sM) / (1 - sA)))/(1 - kap); // new f for reproduction alone
    if(fR >= 0) { // then we are in the first stage of starvation: 1-kappa branch can help pay maintenance
      dL = 0;     // stop growth, but don't shrink
    } else {      // we are in stage 2 of starvation and need to shrink to pay maintenance
      fR = 0;     // nothing left for reproduction
      dL = (rB * (1 + sM) / yP) * ((f * Lm / kap) * ((1 - sA) / (1 + sM)) - L); // shrinking rate
    }
  }

  R = 0; // reproduction rate is zero, unless ...
  if(L >= Lp) { // if we are above the length at puberty, reproduce
    R = fmax(0, (exp(-sH) * Rm / (1 + sR)) * (fR * Lm * (L * L) * (1 - sA) - (Lp3) * (1 + sM)) / (Lm3 - Lp3));
    // Note: hazard to reproduction added with sH
  }

  dRc = R;             // cumulative reproduction rate
  dS  = -(h + hb) * S; // change in survival probability (incl. background mort.)

  // For the damage dynamics, there are four feedback factors x* that obtain a
  // value based on the settings in the parameter `FB`: a bit field
  // with switches for various feedbacks: [surface:volume on uptake,
  // surface:volume on elimination, growth dilution, losses with
  // reproduction].
  double xu=1, xe=1, xG=0, xR=0;
  if(FB & 0x01) {
    xu = Lm_ref / L; // factor for surf:vol scaling uptake rate
  }
  if(FB & 0x02) {
    xe = Lm_ref / L; // factor for surf:vol scaling elimination rate
  }
  if(FB & 0x04) {
    xG = (3 / L) * dL; // factor for growth dilution
  }
  if(FB & 0x08) {
    xR = R * FBV * KRV; // factor for losses with repro
  }

  xG = fmax(0, xG); // stop reverse growth dilution
  // NOTE: reverse growth dilution (concentration by shrinking) is now
  // turned OFF as it leads to runaway situations that lead to failure of the
  // ODE solvers. However, this needs some further thought!

  dDw = kd * (xu * c - xe * Dw) - (xG + xR) * Dw; // ODE for scaled damage

  if(L <= 0.5 * L0) { // if an animal has size less than half the start size ...
    dL = 0; // don't let it grow or shrink any further (to avoid numerical issues)
  }

  if(*t < Tlag) { // when we are NOT past the lag time, set derivatives to zero
    dDw = dL = dRc = dS = 0;
  }

  // Additional model outputs, if requested by user
  if(*ip > 0)
  {
    // response function
    yout[0] = f;
    if(*ip > 1) yout[1] = fR;
    if(*ip > 2) yout[2] = kd;
    // hazard and stress
    if(*ip > 3) yout[3] = s;
    if(*ip > 4) yout[4] = h;
    if(*ip > 5) yout[5] = sA;
    if(*ip > 6) yout[6] = sM;
    if(*ip > 7) yout[7] = sG;
    if(*ip > 8) yout[8] = sR;
    if(*ip > 9) yout[9] = sH;
    // damage feedbacks
    if(*ip > 10) yout[10] = xu;
    if(*ip > 11) yout[11] = xe;
    if(*ip > 12) yout[12] = xG;
    if(*ip > 13) yout[13] = xR;
  }
}
