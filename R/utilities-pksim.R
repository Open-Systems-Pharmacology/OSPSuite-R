#' Loads the `PKSim.R` dll that will enable create individual and create population workflows.
#'
#' @note PKSim dlls are included in the package.
#'
#' @import rSharp
#' @export
initPKSim <- function() {
  if (ospsuiteEnv$isPKSimLoaded) {
    return(invisible())
  }

  pksimR <- system.file("lib", "PKSim.R.dll", package = ospsuiteEnv$packageName)

  if (!file.exists(pksimR)) {
    stop(messages$pkSimRPathInvalid(pksimR))
  }

  rSharp::loadAssembly(pksimR)
  rSharp::callStatic("PKSim.R.Api", "InitializeOnce")

  # Only set the flag if initialization was successful
  ospsuiteEnv$isPKSimLoaded <- TRUE
  invisible()
}
