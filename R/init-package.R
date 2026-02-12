#' Load the OSPSuite R to .NET Binding
#'
#' This will be called once when the package is loaded
#'
#' @import rSharp
#' @keywords internal
.initPackage <- function() {
  # Get library directory path once
  libDir <- system.file("lib", package = ospsuiteEnv$packageName)

  # Helper function for file paths
  libPathFor <- function(name) {
    file.path(libDir, name)
  }

  # Setup platform-specific library loading
  if (.Platform$OS.type == "windows") {
    # Windows: Extend PATH for DLL access
    Sys.setenv(PATH = paste(libDir, Sys.getenv("PATH"), sep = ";"))
  } else if (.Platform$OS.type == "unix") {
    sysname <- Sys.info()[["sysname"]]

    if (sysname == "Linux") {
      # Load shared object files (.so) on Linux
      soFiles <- list.files(libDir, pattern = "\\.so$", full.names = TRUE)
      for (soFile in soFiles) {
        dyn.load(soFile)
      }
    } else if (sysname == "Darwin") {
      # Only arm64 (Apple Silicon) is supported on macOS
      machine <- Sys.info()[["machine"]]
      if (machine != "arm64") {
        stop(
          "Unsupported architecture for macOS: ",
          machine,
          ". Only arm64 (Apple Silicon) is supported."
        )
      }

      # Load macOS dynamic libraries (.dylib)
      dylibFiles <- list.files(libDir, pattern = "\\.dylib$", full.names = TRUE)
      for (dylibFile in dylibFiles) {
        dyn.load(dylibFile)
      }
    }
  }

  # Initialize .NET bindings
  rSharp::loadAssembly(libPathFor("OSPSuite.R.dll"))

  # Initialize API configuration
  netObject <- rSharp::newObjectFromName("OSPSuite.R.ApiConfig")
  apiConfig <- ApiConfig$new(netObject)
  apiConfig$dimensionFilePath <- libPathFor("OSPSuite.Dimensions.xml")
  apiConfig$pkParametersFilePath <- libPathFor("OSPSuite.PKParameters.xml")

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
  mergeBehaviorNetEnum <- seq_along(enum(rSharp::getEnumNames(
    "OSPSuite.Core.Domain.MergeBehavior"
  ))) -
    1
  names(mergeBehaviorNetEnum) <- enum(rSharp::getEnumNames(
    "OSPSuite.Core.Domain.MergeBehavior"
  ))
  # Enum with the merge behaviors for modules available in MoBi
  MergeBehavior <<- enum(mergeBehaviorNetEnum)
}
