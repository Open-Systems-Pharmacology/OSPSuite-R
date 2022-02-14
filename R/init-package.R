#' Load the OSPSuite R to .NET Binding
#'
#' This will be called once when the package is loaded
#'
#' @import rClr
#' @keywords internal
initPackage <- function() {
  filePathFor <- function(name) {
    system.file("lib", name, package = ospsuiteEnv$packageName)
  }

  .addPackageLibToPath()

  rClr::clrLoadAssembly(filePathFor("OSPSuite.R.dll"))

  # Initialize once
  apiConfig <- ApiConfig$new()
  apiConfig$dimensionFilePath <- filePathFor("OSPSuite.Dimensions.xml")
  apiConfig$pkParametersFilePath <- filePathFor("OSPSuite.PKParameters.xml")

  rClr::clrCallStatic("OSPSuite.R.Api", "InitializeOnce", apiConfig$ref)

  initializeDimensionAndUnitLists()
}
