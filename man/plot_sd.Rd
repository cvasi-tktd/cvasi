% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.r
\name{plot_sd}
\alias{plot_sd}
\title{Creates plot of model results (uncertainties optional)}
\usage{
plot_sd(
  model_base,
  treatments,
  rs_mean,
  rs_range = NULL,
  obs_mean = NULL,
  obs_full = NULL,
  x_breaks = NULL,
  y_lim = NULL,
  grid_labels = NULL,
  grid_ncol = 2,
  plot_col = 2,
  y_title = NULL,
  ...
)
}
\arguments{
\item{model_base}{effect scenario object with mean parameters}

\item{treatments}{treatments exposure levels as data frame}

\item{rs_mean}{\code{data.frame}, model results best fit params}

\item{rs_range}{\code{data.frame}, uncertainties as data frame}

\item{obs_mean}{\code{data.frame}, observation data with means per treatment level}

\item{obs_full}{\code{data.frame}, full set including results for replicates}

\item{x_breaks}{optional vector of breaks of x-axis}

\item{y_lim}{optional vector containing limits of y-axis}

\item{grid_labels}{optional labels of grid headers}

\item{grid_ncol}{optional number of grid columns}

\item{plot_col}{output column which should be plotted}

\item{y_title}{optional title of y-axis}

\item{...}{any additional parameters}
}
\value{
a ggplot2 plot object
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}
}
\details{
All parameter combinations and exposure patterns are simulated and the mean
of predictions is derived for a single study. The uncertainty is
passed to the function due to computation time. Results are displayed by
plotting the time series including the uncertainty interval. Observation data
can be optionally displayed. Data should be provided in long format. Function
plots the time (column 1) and the predictions (column 2, can be changed by
the user plot_col)
}
\examples{
set.seed(124)
exposure <- data.frame(
  time = 0:21,
  conc = rnorm(n = 22, mean = 0.1, sd = 0.06),
  trial = "T1"
)
forcings <- list(temp = 12, rad = 15000)
param <- list(EC50 = 0.3, b = 4.16, P_up = 0.0054)
inits <- list(BM = 0.0012, E = 1, M_int = 0)

scenario <- Lemna_Schmitt() \%>\%
  set_forcings(forcings) \%>\%
  set_param(param) \%>\%
  set_init(inits)

sim_result <- simulate_batch(
  model_base = scenario,
  treatments = exposure,
  param_sample = NULL
)

plot_sd(
  model_base = scenario,
  treatments = exposure,
  rs_mean = sim_result
)
}
