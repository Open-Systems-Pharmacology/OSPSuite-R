#' Load the OSPSuite R to .NET Binding
#'
#' This will be called once when the package is loaded
#'
#'
#' @import rSharp
#' @keywords internal
.initPackage <- function() {
  # Get library directory path once
  libDir <- system.file("lib", package = ospsuiteEnv$packageName)

  # Helper function for file paths
  filePathFor <- function(name) {
    file.path(libDir, name)
  }

  # Setup platform-specific SQLite DLL
  targetDll <- file.path(libDir, "System.Data.SQLite.dll")
  sourceDll <- file.path(
    libDir,
    if (Sys.info()[["sysname"]] == "Darwin") {
      "System.Data.SQLite.mac.dll"
    } else {
      "System.Data.SQLite.windows_linux.dll"
    }
  )
  file.copy(sourceDll, targetDll, overwrite = TRUE)

  # Setup platform-specific library loading
  if (.Platform$OS.type == "windows") {
    # Windows: Extend PATH for DLL access
    Sys.setenv(PATH = paste(libDir, Sys.getenv("PATH"), sep = ";"))
  } else if (.Platform$OS.type == "unix") {
    sysname <- Sys.info()[['sysname']]

    if (sysname == "Linux") {
      # Load shared object files (.so) on Linux
      soFiles <- list.files(libDir, pattern = "\\.so$", full.names = TRUE)
      for (soFile in soFiles) {
        dyn.load(soFile)
      }
    } else if (sysname == "Darwin") {
      # macOS: Setup SQLite interop
      machine <- Sys.info()[['machine']]
      targetPath <- file.path(libDir, "SQLite.Interop.dll.dylib")

      # Determine source file based on architecture
      sourceFile <- if (machine == "arm64") {
        "SQLite.Interop.arm64.dylib"
      } else if (machine == "x86_64") {
        "SQLite.Interop.x64.dylib"
      } else {
        stop("Unknown architecture: ", machine)
      }

      # Copy the appropriate SQLite.Interop file to it generic name
      sourcePath <- file.path(libDir, sourceFile)
      file.copy(sourcePath, targetPath, overwrite = TRUE)

      # Setup architecture-specific native libraries
      nativeLibraries <- c(
        "libOSPSuite.FuncParserNative",
        "libOSPSuite.SimModelNative",
        "libOSPSuite.SimModelSolver_CVODES"
      )

      for (libName in nativeLibraries) {
        # Determine source file based on architecture
        sourceFile <- if (machine == "arm64") {
          paste0(libName, ".Arm64.dylib")
        } else if (machine == "x86_64") {
          paste0(libName, ".x64.dylib")
        } else {
          stop("Unknown architecture: ", machine)
        }
        # Copy the appropriate file to its generic name
        sourcePath <- file.path(libDir, sourceFile)
        targetPath <- file.path(libDir, paste0(libName, ".dylib"))
        file.copy(sourcePath, targetPath, overwrite = TRUE)
      }

      # Load macOS dynamic libraries (.dylib)
      dylibFiles <- list.files(libDir, pattern = "\\.dylib$", full.names = TRUE)
      # Filter out architecture-specific source files (x64.dylib, Arm64.dylib) but include renamed generic files
      filteredDylibs <- dylibFiles[
        !grepl("\\.(x64|Arm64)\\.dylib$", dylibFiles)
      ]
      for (dylibFile in filteredDylibs) {
        dyn.load(dylibFile)
      }
    }
  }

  # Initialize .NET bindings
  rSharp::loadAssembly(filePathFor("OSPSuite.R.dll"))

  # Initialize API configuration
  netObject <- rSharp::newObjectFromName("OSPSuite.R.ApiConfig")
  apiConfig <- ApiConfig$new(netObject)
  apiConfig$dimensionFilePath <- filePathFor("OSPSuite.Dimensions.xml")
  apiConfig$pkParametersFilePath <- filePathFor("OSPSuite.PKParameters.xml")

  rSharp::callStatic("OSPSuite.R.Api", "InitializeOnce", apiConfig)

  .initializeDimensionAndUnitLists()
}
