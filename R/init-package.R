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

  clrLoadAssembly(filePathFor("OSPSuite.R.dll"))


  # Initialize once
  apiConfigNet <- clrNew("OSPSuite.R.ApiConfig")
  apiConfig <- ApiConfig$new(apiConfigNet)
  apiConfig$dimensionFilePath <- filePathFor("OSPSuite.Dimensions.xml")

  clrCallStatic("OSPSuite.R.Api", "InitializeOnce", apiConfig$ref)

}


addPackageLibToPath <- function() {
  libPath <- system.file("lib", package = ospsuiteEnv$packageName)

  if (file.exists(libPath)) {
    Sys.setenv(path = paste(libPath, Sys.getenv("path"), sep = ";"))
  }
}
