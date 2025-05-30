% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aging-data.R
\docType{class}
\name{AgingData}
\alias{AgingData}
\title{AgingData}
\description{
Results of a sensitivity analysis run (either individual or population simulation)
}
\section{Super classes}{
\code{\link[rSharp:NetObject]{rSharp::NetObject}} -> \code{\link[ospsuite:DotNetWrapper]{ospsuite::DotNetWrapper}} -> \code{AgingData}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{individualIds}}{Array of individual ids (corresponding to the individualId column of the aging table)}

\item{\code{parameterPaths}}{Array of parameter paths (corresponding to the ParameterPath column of the aging table)}

\item{\code{times}}{Array of time values (corresponding to the Time column of the aging table)}

\item{\code{values}}{Array of parameter values (corresponding to the Value column of the aging table)}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-AgingData-new}{\code{AgingData$new()}}
\item \href{#method-AgingData-print}{\code{AgingData$print()}}
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
\if{html}{\out{<a id="method-AgingData-new"></a>}}
\if{latex}{\out{\hypertarget{method-AgingData-new}{}}}
\subsection{Method \code{new()}}{
Initialize a new instance of the class
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AgingData$new()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A new \code{OSPSuite.R.Domain.AgingData} object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-AgingData-print"></a>}}
\if{latex}{\out{\hypertarget{method-AgingData-print}{}}}
\subsection{Method \code{print()}}{
Print the object to the console
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AgingData$print(...)}\if{html}{\out{</div>}}
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
