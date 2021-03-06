% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graphics_themes_RescaleSignal.R
\name{rescaleSignalsValues.DataFrame}
\alias{rescaleSignalsValues.DataFrame}
\title{rescaleSignalsValues.DataFrame}
\usage{
rescaleSignalsValues.DataFrame(
  model,
  col.to.rescale = "signal",
  col.rescaled = "signal_rescaled",
  ...
)
}
\arguments{
\item{model}{SCRCModel}

\item{col.to.rescale}{character, define column that must be rescaled}

\item{col.rescaled}{character, define name of the rescaled}

\item{...}{
  Arguments passed on to \code{\link[=rescaleSignalsValues]{rescaleSignalsValues}}
  \describe{
    \item{\code{rescale.fun}}{parameter, that defines a function used for rescaling signals in plots.
There are three built-in functions, that can be chosen:
(1) \code{'factor'} - signals treated as factors (default) with levels defined in list \code{rescale.fun.args},
(2) \code{'numeric'},
(3) \code{logarithmic} - with base defined in \code{rescale.fun.args} - default: \code{e = exp(1)}.
Function must be defined as a lambda construct \code{function(x, ...){...}}.}
    \item{\code{rescale.fun.args}}{list of the arguments to defaults \code{rescale.fun}}
  }}
}
\description{
rescaleSignalsValues.DataFrame
}
