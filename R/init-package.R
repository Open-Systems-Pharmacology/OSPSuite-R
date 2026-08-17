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

  # Windows needs the lib directory on PATH for the native DLLs to resolve. This
  # is inexpensive and side-effect free, so it happens regardless of whether the
  # runtime can be initialised below.
  if (.Platform$OS.type == "windows") {
    Sys.setenv(PATH = paste(libDir, Sys.getenv("PATH"), sep = ";"))
  }

  # Loading the native libraries and initialising the .NET bindings can fail on
  # machines and build environments (CRAN check machines, R-universe builders)
  # that lack the .NET runtime or a system library the native code needs (for
  # example a `libxml2` whose soname differs from the one the shipped `.so` was
  # linked against). Loading the package must never fail in that case: `ospsuite`
  # still has to install and load so it can be built, checked, and resolved as a
  # dependency. When initialisation fails we record the reason in
  # `ospsuiteEnv$loadError` (and leave `ospsuiteEnv$initialized` FALSE) and
  # return quietly. The reason is surfaced to the user in `.onAttach()`; calls
  # into the .NET API then fail with rSharp's own runtime error.
  ospsuiteEnv$loadError <- tryCatch(
    {
      .loadNativeLibraries(libDir)

      # Initialize .NET bindings
      rSharp::loadAssembly(libPathFor("OSPSuite.R.dll"))

      # Initialize API configuration
      netObject <- rSharp::newObjectFromName("OSPSuite.R.ApiConfig")
      apiConfig <- ApiConfig$new(netObject)
      apiConfig$dimensionFilePath <- libPathFor("OSPSuite.Dimensions.xml")
      apiConfig$pkParametersFilePath <- libPathFor("OSPSuite.PKParameters.xml")

      # Load MoBi.R
      mobiR <- system.file(
        "lib",
        "MoBi.R.dll",
        package = ospsuiteEnv$packageName
      )

      rSharp::loadAssembly(mobiR)
      rSharp::callStatic("MoBi.R.Api, MoBi.R", "InitializeOnce", apiConfig)

      .initializeDimensionAndUnitLists()
      .loadEnums()

      ospsuiteEnv$initialized <- TRUE
      NULL
    },
    error = function(e) {
      ospsuiteEnv$initialized <- FALSE
      conditionMessage(e)
    }
  )

  invisible()
}

# Loads the native shared libraries shipped in `inst/lib`. Kept separate so the
# platform handling is isolated and the caller can guard the whole native/.NET
# initialisation in one place.
.loadNativeLibraries <- function(libDir) {
  if (.Platform$OS.type != "unix") {
    return(invisible())
  }
  sysname <- Sys.info()[["sysname"]]
  if (sysname == "Linux") {
    soFiles <- list.files(libDir, pattern = "\\.so$", full.names = TRUE)
    for (soFile in soFiles) {
      dyn.load(soFile)
    }
  } else if (sysname == "Darwin") {
    # The unsupported-architecture guard for macOS lives in `.onLoad()`, which
    # aborts before this graceful-degradation path runs, so by the time we get
    # here macOS is known to be arm64 (Apple Silicon).
    dylibFiles <- list.files(libDir, pattern = "\\.dylib$", full.names = TRUE)
    for (dylibFile in dylibFiles) {
      dyn.load(dylibFile)
    }
  }
  invisible()
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

  # MoleculeType: curated subset of the underlying QuantityType flag values,
  # with user-facing names mapped to the integer flag value.
  MoleculeType <<- .loadMoleculeTypeEnum()
}

#' Build the `MoleculeType` enum by reading flag values from the underlying
#' `OSPSuite.Core.Domain.QuantityType` enum and keeping only molecule-relevant
#' members.
#' @keywords internal
.loadMoleculeTypeEnum <- function() {
  quantityType <- rSharp::getType("OSPSuite.Core.Domain.QuantityType")
  netValues <- rSharp::callStatic(
    "System.Enum, System.Runtime",
    "GetValues",
    quantityType
  )
  flagByName <- list()
  for (netValue in netValues) {
    name <- netValue$call("ToString")
    flagByName[[name]] <- as.integer(rSharp::callStatic(
      "System.Convert, System.Runtime",
      "ToInt32",
      netValue
    ))
  }
  # Mapping of user-facing names to internal flag names. `Binding Partner` is
  # the public label for the internal `OtherProtein` flag.
  curatedNames <- c(
    "Drug" = "Drug",
    "Metabolite" = "Metabolite",
    "Enzyme" = "Enzyme",
    "Transporter" = "Transporter",
    "Binding Partner" = "OtherProtein",
    "Complex" = "Complex",
    "Protein" = "Protein"
  )
  curated <- vapply(
    curatedNames,
    function(netKey) {
      if (is.null(flagByName[[netKey]])) {
        stop(sprintf(
          "QuantityType flag '%s' was not found in the engine enum.",
          netKey
        ))
      }
      flagByName[[netKey]]
    },
    integer(1)
  )
  names(curated) <- names(curatedNames)
  enum(curated)
}
