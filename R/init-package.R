#' Load the OSPSuite R to .NET Binding
#'
#' This will be called once when the package is loaded
#'
#' @import rSharp
#' @keywords internal
.initPackage <- function() {
  filePathFor <- function(name) {
    system.file("lib", name, package = ospsuiteEnv$packageName)
  }

  .addPackageLibToPath()

  rSharp::loadAssembly(filePathFor("OSPSuite.R.dll"))

  # Initialize once
  apiConfig <- rSharp::newObjectFromName("OSPSuite.R.ApiConfig", R6objectClass = ApiConfig)
  apiConfig$dimensionFilePath <- filePathFor("OSPSuite.Dimensions.xml")
  apiConfig$pkParametersFilePath <- filePathFor("OSPSuite.PKParameters.xml")

  rSharp::callStatic("OSPSuite.R.Api", "InitializeOnce", apiConfig)

  .initializeDimensionAndUnitLists()
}
