#' Load the OSPSuite R to .NET Binding
#'
#' This will be called once when the package is loaded
#'
initPackage <- function() {
  filePathFor <- function(name) {
    system.file("lib", name, package = "ospsuite")
  }

  rClr::clrLoadAssembly(filePathFor("OSPSuite.R.dll"))


  # Initialize once
  apiConfigNet <- rClr::clrNew("OSPSuite.R.ApiConfig")
  apiConfig <- ApiConfig$new(apiConfigNet)
  apiConfig$dimensionFilePath <- filePathFor("OSPSuite.Dimensions.xml")

  rClr::clrCallStatic("OSPSuite.R.Api", "InitializeOnce", apiConfig$ref)

  # initialize global variables (mostly usef for performance optimization)
  ospsuiteEnv$containerTask <- getNetTask("ContainerTask")
}
