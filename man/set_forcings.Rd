% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_forcings.R
\name{set_forcings}
\alias{set_forcings}
\alias{set_forcings,EffectScenario-method}
\alias{set_forcings,list-method}
\title{Set time-dependent parameters}
\usage{
set_forcings(x, ...)

\S4method{set_forcings}{EffectScenario}(x, ...)

\S4method{set_forcings}{list}(x, ...)
}
\arguments{
\item{x}{(vector of) \link{scenario} objects}

\item{...}{named argument list to set as forcings}
}
\value{
Modified \link{scenarios}
}
\description{
Parameters which change their value over time are referred to as \emph{forcings}.
If and what parameters can vary over time depends on the model in question.
In many cases, \emph{forcings} represent time-series of environmental properties.
}
\details{
Forcing time-series are always represented by a
\code{data.frame} containing two columns. The first column representing time,
the second representing the parameter that is a function of time. The
ordering of columns is mandatory. The column names are essentially irrelevant
but may help documenting the scenario and its data. The rows must be
ordered chronologically. A time-series can consist of only a single row; in
this case it will represent constant conditions.

Handling forcing time-series is a costly task for the ODE solver due to consistency
checks and interpolation between timesteps. How the solver interpolates
the forcing time-series can be controlled by certain arguments to functions
such as \code{\link[=simulate]{simulate()}} and \code{\link[=effect]{effect()}}. Please refer to \code{\link[=simulate]{simulate()}} for a brief
overview and \link[deSolve:forcings]{deSolve::forcings} for a detailed description.

Forcing time-series should be kept as short as possible and as complex as
needed for optimal computational efficiency.
}
\examples{
# constant values will be automatically converted to a data.frame
Lemna_Schmitt() \%>\% set_forcings(temp=20) -> lemna
lemna@forcings

# setting multiple forcings at once
df <- data.frame(t=0:14, temp=rnorm(15, mean=20)) # random temperature series
Lemna_Schmitt() \%>\% set_forcings(temp=df, rad=15000) -> lemna
lemna@forcings

# forcings can also be supplied as a named list
Lemna_Schmitt() \%>\% set_forcings(list(temp=20, rad=15000)) -> lemna
lemna@forcings
}
