#' Load the OSPSuite R to .NET Binding
#'
#' This will be called once when the package is loaded
#'
#' @import rClr
initPackage <- function() {
  filePathFor <- function(name) {
    system.file("lib", name, package = ospsuiteEnv$packageName)
  }

  addPackageLibToPath()

  rClr::clrLoadAssembly(filePathFor("OSPSuite.R.dll"))

  # Initialize once
  apiConfig <- ApiConfig$new()
  apiConfig$dimensionFilePath <- filePathFor("OSPSuite.Dimensions.xml")
  apiConfig$pkParametersFilePath <- filePathFor("OSPSuite.PKParameters.xml")

  rClr::clrCallStatic("OSPSuite.R.Api", "InitializeOnce", apiConfig$ref)
}

addPackageLibToPath <- function() {
  libPath <- system.file("lib", package = ospsuiteEnv$packageName)
  addPathToSystemPath(libPath)
}

addPathToSystemPath <- function(pathToAdd){
  if (file.exists(pathToAdd)) {
    Sys.setenv(path = paste(pathToAdd, Sys.getenv("path"), sep = ";"))
  }
}

#' Loads the PKSim.R that will enable create individual and create population workflows.
#' @param pksimFolderPath Path where PK-Sim is installed. If this is not specified, path will be read from registry using the package version
#'
#' @note  This will only work on Windows machine and should not be called on any other OS
#'
#' @import rClr
initPKSim <- function(pksimFolderPath = NULL) {
  addPathToSystemPath(pksimFolderPath)
  pksimR <-  file.path(pksimFolderPath, 'PKSim.R.dll')
  if (!file.exists(pksimR)) {
    stop(messages$pkSimRPathInvalid(pksimR))
  }
  rClr::clrLoadAssembly(pksimR)
  rClr::clrCallStatic("PKSim.R.Api", "InitializeOnce")
}
