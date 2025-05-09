% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sequence.R
\name{sequence.extract}
\alias{sequence.extract}
\alias{[[<-,ScenarioSequence,numeric,ANY,ANY-method}
\alias{[,ScenarioSequence,numeric,missing,missing-method}
\alias{[[,ScenarioSequence,numeric-method}
\alias{[[<-,ScenarioSequence,numeric,missing,EffectScenario-method}
\alias{length,ScenarioSequence-method}
\title{Extract and replace elements of a sequence}
\usage{
\S4method{[}{ScenarioSequence,numeric,missing,missing}(x, i)

\S4method{[[}{ScenarioSequence,numeric}(x, i)

\S4method{[[}{ScenarioSequence,numeric,missing,EffectScenario}(x, i, j) <- value

\S4method{length}{ScenarioSequence}(x)
}
\arguments{
\item{x}{\link{sequence}}

\item{i}{index of elements to extract or replace}

\item{j}{\emph{not used}}

\item{value}{new scenario}
}
\value{
various
}
\description{
The array accessor generics allow extracting and replacing scenarios within
am existing sequence. \code{[} and \code{[[} work identical to
}
\section{Functions}{
\itemize{
\item \code{x[i}: Returns a list of scenarios from the sequence.

\item \code{x[[i}: Returns a single scenario from the sequence.

\item \code{`[[`(x = ScenarioSequence, i = numeric, j = missing) <- value}: Replaces a single scenario in the sequence.

\item \code{length(ScenarioSequence)}: Returns the number of scenarios in the sequence.

}}
\examples{
# create a sequence
seq <- sequence(list(minnow_it, minnow_it), breaks=3)

seq[1]       # first element, as a list of scenarios
seq[c(1)]    # the same
seq[c(1, 2)] # both elements as a list of scenarios
seq[[1]]     # first element as a scenario

# replacing single elements
seq[[1]] <- minnow_sd \%>\% set_times(1:3)
}
