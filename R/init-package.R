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

  if (!file.exists(libPathFor("System.Data.SQLite.dll"))) {
    cli::cli_warn(
      message = c(
        "x" = "Required library {.file System.Data.SQLite.dll} not found in {.file {libDir}}.",
        " " = "Some functionalities may not work properly and some tests will fail.",
        "i" = "If you are in a development environment, make sure to run:",
        " " = "  {.run source('tools/setup_dev.R')}",
        " " = "  {.run setup_dev()}",
        " " = "before using {.run devtools::load_all()} or {.run devtools::test()}"
      )
    )
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
  # `ospsuiteEnv$loadError` and return quietly. The reason is surfaced to the
  # user in `.onAttach()` and on the first call into the .NET API (see
  # `.ensureInitialised()`).
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

      rSharp::callStatic("OSPSuite.R.Api", "InitializeOnce", apiConfig)

      .initializeDimensionAndUnitLists()

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
    # Only arm64 (Apple Silicon) is supported on macOS
    machine <- Sys.info()[["machine"]]
    if (machine != "arm64") {
      stop(
        "Unsupported architecture for macOS: ",
        machine,
        ". Only arm64 (Apple Silicon) is supported."
      )
    }
    dylibFiles <- list.files(libDir, pattern = "\\.dylib$", full.names = TRUE)
    for (dylibFile in dylibFiles) {
      dyn.load(dylibFile)
    }
  }
  invisible()
}

# Ensures the .NET runtime and native libraries were initialised before a call
# into the .NET API. Aborts with the recorded reason so callers get an
# actionable error instead of a low-level failure from the native layer.
.ensureInitialised <- function() {
  if (isTRUE(ospsuiteEnv$initialized)) {
    return(invisible())
  }
  stop(
    paste(
      "The OSPSuite .NET runtime could not be initialised, so this function",
      "cannot be used. ospsuite is installed, but calls into the .NET API will",
      "fail until a working runtime is available.",
      if (!is.null(ospsuiteEnv$loadError)) {
        paste0("Details: ", ospsuiteEnv$loadError)
      },
      sep = "\n"
    ),
    call. = FALSE
  )
}
