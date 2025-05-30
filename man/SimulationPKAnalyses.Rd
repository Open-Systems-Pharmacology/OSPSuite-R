% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulation-pk-analyses.R
\docType{class}
\name{SimulationPKAnalyses}
\alias{SimulationPKAnalyses}
\title{SimulationPKAnalyses}
\description{
pK-Analyses of a simulation (either individual or population simulation).
}
\section{Super classes}{
\code{\link[rSharp:NetObject]{rSharp::NetObject}} -> \code{\link[ospsuite:DotNetWrapper]{ospsuite::DotNetWrapper}} -> \code{SimulationPKAnalyses}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{simulation}}{Reference to the \code{Simulation} used to calculate or import the PK-Analyses (Read-Only)}

\item{\code{allPKParameterNames}}{Returns the name of all pk parameters for which a value is available}

\item{\code{allQuantityPaths}}{Returns the path of all quantities for which pk parameters were calculated}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-SimulationPKAnalyses-new}{\code{SimulationPKAnalyses$new()}}
\item \href{#method-SimulationPKAnalyses-allPKParametersFor}{\code{SimulationPKAnalyses$allPKParametersFor()}}
\item \href{#method-SimulationPKAnalyses-pKParameterFor}{\code{SimulationPKAnalyses$pKParameterFor()}}
\item \href{#method-SimulationPKAnalyses-print}{\code{SimulationPKAnalyses$print()}}
}
}
\if{html}{\out{
<details><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="rSharp" data-topic="NetObject" data-id=".printClass"><a href='../../rSharp/html/NetObject.html#method-NetObject-.printClass'><code>rSharp::NetObject$.printClass()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rSharp" data-topic="NetObject" data-id=".printLine"><a href='../../rSharp/html/NetObject.html#method-NetObject-.printLine'><code>rSharp::NetObject$.printLine()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rSharp" data-topic="NetObject" data-id="call"><a href='../../rSharp/html/NetObject.html#method-NetObject-call'><code>rSharp::NetObject$call()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rSharp" data-topic="NetObject" data-id="get"><a href='../../rSharp/html/NetObject.html#method-NetObject-get'><code>rSharp::NetObject$get()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rSharp" data-topic="NetObject" data-id="getFields"><a href='../../rSharp/html/NetObject.html#method-NetObject-getFields'><code>rSharp::NetObject$getFields()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rSharp" data-topic="NetObject" data-id="getMemberSignature"><a href='../../rSharp/html/NetObject.html#method-NetObject-getMemberSignature'><code>rSharp::NetObject$getMemberSignature()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rSharp" data-topic="NetObject" data-id="getMethods"><a href='../../rSharp/html/NetObject.html#method-NetObject-getMethods'><code>rSharp::NetObject$getMethods()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rSharp" data-topic="NetObject" data-id="getProperties"><a href='../../rSharp/html/NetObject.html#method-NetObject-getProperties'><code>rSharp::NetObject$getProperties()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rSharp" data-topic="NetObject" data-id="getStaticFields"><a href='../../rSharp/html/NetObject.html#method-NetObject-getStaticFields'><code>rSharp::NetObject$getStaticFields()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rSharp" data-topic="NetObject" data-id="getStaticMethods"><a href='../../rSharp/html/NetObject.html#method-NetObject-getStaticMethods'><code>rSharp::NetObject$getStaticMethods()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rSharp" data-topic="NetObject" data-id="getStaticProperties"><a href='../../rSharp/html/NetObject.html#method-NetObject-getStaticProperties'><code>rSharp::NetObject$getStaticProperties()</code></a></span></li>
<li><span class="pkg-link" data-pkg="rSharp" data-topic="NetObject" data-id="set"><a href='../../rSharp/html/NetObject.html#method-NetObject-set'><code>rSharp::NetObject$set()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SimulationPKAnalyses-new"></a>}}
\if{latex}{\out{\hypertarget{method-SimulationPKAnalyses-new}{}}}
\subsection{Method \code{new()}}{
Initialize a new instance of the class
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SimulationPKAnalyses$new(netObject, simulation)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{netObject}}{A \code{NetObject}}

\item{\code{simulation}}{Simulation for which the pkParameters were calculated}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new \code{SimulationPKAnalyses} object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SimulationPKAnalyses-allPKParametersFor"></a>}}
\if{latex}{\out{\hypertarget{method-SimulationPKAnalyses-allPKParametersFor}{}}}
\subsection{Method \code{allPKParametersFor()}}{
Returns all QuantityPKParameter defined for a given path
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SimulationPKAnalyses$allPKParametersFor(quantityPath)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{quantityPath}}{Path for which pkParameters should be retrieved}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SimulationPKAnalyses-pKParameterFor"></a>}}
\if{latex}{\out{\hypertarget{method-SimulationPKAnalyses-pKParameterFor}{}}}
\subsection{Method \code{pKParameterFor()}}{
The pK Parameter defined for the given path and name
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SimulationPKAnalyses$pKParameterFor(quantityPath, pkParameter)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{quantityPath}}{Path for which the pkParameter named \code{pkParameter}
should be retrieved}

\item{\code{pkParameter}}{Name of the pkParameter to retrieve}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SimulationPKAnalyses-print"></a>}}
\if{latex}{\out{\hypertarget{method-SimulationPKAnalyses-print}{}}}
\subsection{Method \code{print()}}{
Print the object to the console
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SimulationPKAnalyses$print(...)}\if{html}{\out{</div>}}
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
