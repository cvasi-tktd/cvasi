% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set.R
\name{set_mode_of_action}
\alias{set_mode_of_action}
\alias{set_moa}
\title{Set mode of action}
\usage{
set_mode_of_action(x, code)

set_moa(x, code)
}
\arguments{
\item{x}{vector of \link{scenarios}}

\item{code}{a code for a mode of action, refer to model description for
details}
}
\value{
modified \link{scenarios}
}
\description{
Updates the model parameter \code{MoA} to a certain value
}
\section{Functions}{
\itemize{
\item \code{set_moa()}: Shorthand version

}}
\examples{
# Set MoA=8, i.e. hazard during oogenesis
americamysis \%>\%
  set_mode_of_action(8) \%>\%
  effect(method="ode45")

# alternative approach using the parameter directly
americamysis \%>\%
  set_param(c(MoA=8)) \%>\%
  effect(method="ode45")
}
