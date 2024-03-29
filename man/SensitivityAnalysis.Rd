% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sensitivity-analysis.R
\docType{class}
\name{SensitivityAnalysis}
\alias{SensitivityAnalysis}
\title{SensitivityAnalysis}
\description{
Supports Sensitivity Analysis workflow to assess the impact of input parameters on the simulation outputs
}
\section{Super classes}{
\code{\link[ospsuite.utils:Printable]{ospsuite.utils::Printable}} -> \code{\link[ospsuite:DotNetWrapper]{ospsuite::DotNetWrapper}} -> \code{SensitivityAnalysis}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{simulation}}{Reference to the \code{Simulation} used to calculate or import the sensitivity analysis results (Read-Only).}

\item{\code{numberOfSteps}}{Number of steps used for the variation of each parameter (optional, default specified in \code{ospsuiteEnv$sensitivityAnalysisConfig})}

\item{\code{variationRange}}{Variation applied to the parameter (optional, default specified in \code{ospsuiteEnv$sensitivityAnalysisConfig})}

\item{\code{parameterPaths}}{List of parameters to use for sensitivity calculation.If empty, the sensitivity will be performed automatically
on all constant parameters that are really in use in the simulation. Constant parameter means all parameters with a constant value or a formula parameter
with a value that was overridden by the user}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-SensitivityAnalysis-new}{\code{SensitivityAnalysis$new()}}
\item \href{#method-SensitivityAnalysis-addParameterPaths}{\code{SensitivityAnalysis$addParameterPaths()}}
\item \href{#method-SensitivityAnalysis-clearParameterPaths}{\code{SensitivityAnalysis$clearParameterPaths()}}
\item \href{#method-SensitivityAnalysis-print}{\code{SensitivityAnalysis$print()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SensitivityAnalysis-new"></a>}}
\if{latex}{\out{\hypertarget{method-SensitivityAnalysis-new}{}}}
\subsection{Method \code{new()}}{
Initialize a new instance of the class
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SensitivityAnalysis$new(
  simulation,
  parameterPaths = NULL,
  numberOfSteps = ospsuiteEnv$sensitivityAnalysisConfig$numberOfSteps,
  variationRange = ospsuiteEnv$sensitivityAnalysisConfig$variationRange
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{simulation}}{Simulation for which a sensitivity analysis should be performed}

\item{\code{parameterPaths}}{Vector of parameter paths to use for sensitivity calculation (optional).If undefined, the sensitivity will be performed automatically
on all constant parameters of the simulation. Constant parameter means all parameters with a constant value or a formula parameter
with a value that was overridden by the user}

\item{\code{numberOfSteps}}{Number of steps used for the variation of each parameter (optional, default specified in \code{getOSPSuiteSetting("sensitivityAnalysisConfig")})}

\item{\code{variationRange}}{Variation applied to the parameter (optional, default specified in \code{getOSPSuiteSetting("sensitivityAnalysisConfig")})}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new \code{SensitivityAnalysis} object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SensitivityAnalysis-addParameterPaths"></a>}}
\if{latex}{\out{\hypertarget{method-SensitivityAnalysis-addParameterPaths}{}}}
\subsection{Method \code{addParameterPaths()}}{
Adds the parameterPaths to the list of parameter path to vary in the sensitivity analysis
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SensitivityAnalysis$addParameterPaths(parameterPaths)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{parameterPaths}}{Parameter paths to add (single or multiple values)
If no parameters were specified during creating of a \code{SensitivityAnalysis} (all constant parameters are considered),
calling \code{addParameterPaths} will make only the manually added parameters being varied.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SensitivityAnalysis-clearParameterPaths"></a>}}
\if{latex}{\out{\hypertarget{method-SensitivityAnalysis-clearParameterPaths}{}}}
\subsection{Method \code{clearParameterPaths()}}{
Removes all parameter paths defined in the Sensitivity Analysis
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SensitivityAnalysis$clearParameterPaths()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SensitivityAnalysis-print"></a>}}
\if{latex}{\out{\hypertarget{method-SensitivityAnalysis-print}{}}}
\subsection{Method \code{print()}}{
Print the object to the console
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SensitivityAnalysis$print(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Rest arguments.}
}
\if{html}{\out{</div>}}
}
}
}
