% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/explore_space.R
\name{explore_space}
\alias{explore_space}
\title{Explore parameter space}
\usage{
explore_space(
  x,
  par,
  res,
  output,
  sample_size = 1000,
  max_runs = 30,
  nr_accept = 100,
  sample_factor = 1.2,
  ...
)
}
\arguments{
\item{x}{a list of \link{caliset} objects}

\item{par}{best fit parameters from joined calibration}

\item{res}{output of 'lik_profile()' function}

\item{output}{character vector, name of output column of \code{\link[=simulate]{simulate()}} that
is used in calibration}

\item{sample_size}{number of samples to draw from each parameter interval}

\item{max_runs}{max number of times to redraw samples (within a smaller space), and repeat the process}

\item{nr_accept}{threshold for number of points sampled within the inner circle}

\item{sample_factor}{multiplication factor for sampling (95\% interval * sample factor)}

\item{...}{additional parameters passed through to \code{\link[=simulate]{simulate()}}}
}
\value{
a list containing a plot to explore the parameter space, and the \code{data.frame}
supporting it
}
\description{
The function is aimed at getting an idea of how the parameter space
of a model behaves, so that parameter identifiability problems and correlations
between parameters can be explored. Therefore, the function samples a large
number of parameter sets by randomly drawing from each parameter's 95\%
confidence interval (generated by \code{\link[=lik_profile]{lik_profile()}}). It then
checks how many of the parameter sets are within acceptable limits by comparing
the likelihood ratio of a parameter set vs. the original parameter set against
a chi-square distribution as degrees of freedom (df) the total number of profile
parameters (outer rim) or one df (inner rim). If needed, the function resamples
until at least \code{nr_accept} parameters sets are within the inner rim
}
\examples{
\donttest{
library(dplyr)
# Example with Lemna model - physiological params
# Before applying the function, a model needs to be calibrated and its parameters profiled
# Inputs for likelihood profiling

# exposure - control run
exp <- Schmitt2013 \%>\%
  filter(ID == "T0") \%>\%
  select(time=t, conc)

# observations - control run
obs <- Schmitt2013 \%>\%
  filter(ID == "T0") \%>\%
  select(t, BM=obs)

# parameters after calibration
params <- c(
  k_phot_max = 5.663571,
  k_resp = 1.938689,
  Topt = 26.7
)

# set parameter boundaries (if different from defaults)
bounds <- list(
  k_resp = list(0, 10),
  k_phot_max = list(0, 30),
  Topt = list(20, 30)
)

# update metsulfuron
myscenario <- metsulfuron \%>\%
  set_init(c(BM = 5, E = 1, M_int = 0)) \%>\%
  set_param(list(
    k_0 = 5E-5,
    a_k = 0.25,
    BM50 = 17600,
    mass_per_frond = 0.1
  )) \%>\%
  set_exposure(exp) \%>\%
  set_param(params) \%>\%
  set_bounds(bounds)

# Likelihood profiling
res <- lik_profile(
  x = myscenario,
  data = obs,
  output = "BM",
  par = params,
  refit = FALSE,
  type = "fine",
  method = "Brent"
)
# plot
plot_lik_profile(res)

# parameter space explorer
set.seed(1) # for reproducibility
res_space <- explore_space(
  x = list(caliset(myscenario, obs)),
  par = params,
  res = res,
  output = "BM",
  sample_size = 1000,
  max_runs = 20,
  nr_accept = 100)

plot_param_space(res_space)

}
}
