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


  # Make .dll binaries available on windows by extending PATH
  if (.Platform$OS.type == "windows") {
    Sys.setenv(PATH = paste(system.file("lib", package = ospsuiteEnv$packageName),
      Sys.getenv("PATH"),
      sep = ";"
    ))
  }

  # Load the .so binary files on unix
  if (.Platform$OS.type == "unix") {
    for (soFile in list.files(system.file("lib", package = ospsuiteEnv$packageName), pattern = "\\.so$", full.names = TRUE)) {
      dyn.load(soFile)
    }
  }

  rSharp::loadAssembly(filePathFor("OSPSuite.R.dll"))
  # Initialize once
  netObject <- rSharp::newObjectFromName("OSPSuite.R.ApiConfig")
  apiConfig <- ApiConfig$new(netObject)
  apiConfig$dimensionFilePath <- filePathFor("OSPSuite.Dimensions.xml")
  apiConfig$pkParametersFilePath <- filePathFor("OSPSuite.PKParameters.xml")

  # Load MoBi.R
  mobiR <- system.file("lib", "MoBi.R.dll", package = ospsuiteEnv$packageName)

  rSharp::loadAssembly(mobiR)
  rSharp::callStatic("MoBi.R.Api", "InitializeOnce", apiConfig)

  .initializeDimensionAndUnitLists()
  .loadEnums()
}


#' Load enums from .NET
#' This function must be called after initializing the package.
#' @noRd
.loadEnums <- function() {
  # MergeBehavior enum

  # -1 because the indexing in .NET starts at 0
  mergeBehaviorNetEnum <- seq_along(enum(rSharp::getEnumNames("OSPSuite.Core.Domain.MergeBehavior"))) - 1
  names(mergeBehaviorNetEnum) <- enum(rSharp::getEnumNames("OSPSuite.Core.Domain.MergeBehavior"))
  # Enum with the merge behaviors for modules available in MoBi
  MergeBehavior <<- enum(mergeBehaviorNetEnum)
}
