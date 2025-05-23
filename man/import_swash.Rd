% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_toxswa.R
\name{import_swash}
\alias{import_swash}
\title{SWASH project exposure profile import}
\usage{
import_swash(swash_dir, ...)
}
\arguments{
\item{swash_dir}{path to the SWASH project directory}

\item{...}{arguments passed on to \code{\link[=import_toxswa]{import_toxswa()}}}
}
\value{
a list of imported exposure series, see \code{\link[=import_toxswa]{import_toxswa()}} for details
}
\description{
Read all TOXSWA files within a SWASH project directory.
}
