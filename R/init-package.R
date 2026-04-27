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

  # MoleculeType: curated subset of OSPSuite.Core.Domain.QuantityType flag
  # values, with names mapped to the integer flag value defined in .NET.
  MoleculeType <<- .loadMoleculeTypeEnum()
}

#' Build the `MoleculeType` enum by reading flag values from the .NET
#' `OSPSuite.Core.Domain.QuantityType` enum and keeping only molecule-relevant
#' members.
#' @keywords internal
.loadMoleculeTypeEnum <- function() {
  quantityType <- rSharp::getType("OSPSuite.Core.Domain.QuantityType")
  netValues <- rSharp::callStatic("System.Enum", "GetValues", quantityType)
  flagByName <- list()
  for (netValue in netValues) {
    name <- netValue$call("ToString")
    flagByName[[name]] <- as.integer(rSharp::callStatic(
      "System.Convert", "ToInt32", netValue
    ))
  }
  # Mapping of user-facing names to .NET flag names. `Binding Partner` is the
  # public label for the .NET `OtherProtein` flag.
  curatedNames <- c(
    "Drug" = "Drug",
    "Metabolite" = "Metabolite",
    "Enzyme" = "Enzyme",
    "Transporter" = "Transporter",
    "Binding Partner" = "OtherProtein",
    "Complex" = "Complex",
    "Protein" = "Protein"
  )
  curated <- vapply(curatedNames, function(netKey) {
    if (is.null(flagByName[[netKey]])) {
      stop(sprintf(
        "QuantityType flag '%s' was not found in the .NET enum.",
        netKey
      ))
    }
    flagByName[[netKey]]
  }, integer(1))
  names(curated) <- names(curatedNames)
  enum(curated)
}
