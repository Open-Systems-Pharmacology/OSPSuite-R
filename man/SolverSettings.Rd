% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/solver-settings.R
\docType{class}
\name{SolverSettings}
\alias{SolverSettings}
\title{SolverSettings}
\description{
Solver settings associated with a given simulation
}
\section{Super classes}{
\code{\link[rSharp:NetObject]{rSharp::NetObject}} -> \code{\link[ospsuite:DotNetWrapper]{ospsuite::DotNetWrapper}} -> \code{SolverSettings}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{useJacobian}}{Use of Jacobian matrix during calculations}

\item{\code{h0}}{Initial time step size}

\item{\code{hMin}}{Minimum absolute value of step size allowed}

\item{\code{hMax}}{Maximum absolute value of step size allowed}

\item{\code{mxStep}}{Maximum number of internal steps to be taken by the solver in its attempt to reach tout}

\item{\code{relTol}}{Relative tolerance of unknowns}

\item{\code{absTol}}{Absolute tolerance of unknowns}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-SolverSettings-print}{\code{SolverSettings$print()}}
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
<li><span class="pkg-link" data-pkg="ospsuite" data-topic="DotNetWrapper" data-id="initialize"><a href='../../ospsuite/html/DotNetWrapper.html#method-DotNetWrapper-initialize'><code>ospsuite::DotNetWrapper$initialize()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SolverSettings-print"></a>}}
\if{latex}{\out{\hypertarget{method-SolverSettings-print}{}}}
\subsection{Method \code{print()}}{
Print the object to the console
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SolverSettings$print(...)}\if{html}{\out{</div>}}
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
