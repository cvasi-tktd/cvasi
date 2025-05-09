% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_bounds.R
\name{set_bounds}
\alias{set_bounds}
\alias{set_bounds,EffectScenario,list-method}
\alias{set_bounds,CalibrationSet,list-method}
\alias{set_bounds,list,list-method}
\alias{set_bounds,ScenarioSequence,list-method}
\title{Set boundaries of model parameters}
\usage{
set_bounds(x, bounds)

\S4method{set_bounds}{EffectScenario,list}(x, bounds)

\S4method{set_bounds}{CalibrationSet,list}(x, bounds)

\S4method{set_bounds}{list,list}(x, bounds)

\S4method{set_bounds}{ScenarioSequence,list}(x, bounds)
}
\arguments{
\item{x}{vector of \link{scenario} or \link{caliset} objects}

\item{bounds}{named list of numerical vectors, where the first level lists the parameters
by name, and the second level lists the lower and upper boundary}
}
\value{
\link{scenario} or \link{caliset} with modified parameter boundaries
}
\description{
Modifies the boundaries of model parameters for one or more \link{scenario} or
\link{caliset} objects.
}
\examples{
metsulfuron \%>\%
   set_bounds(list(k_phot_max = c(0, 30),
                   k_resp = c(0, 10)))

}
