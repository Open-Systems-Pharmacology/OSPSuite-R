#' Load the OSPSuite R to .NET Binding
#'
#' This will be called once when the package is loaded
#'
#'
#' @import rSharp
#' @keywords internal
.initPackage <- function() {
  libDir <- system.file("lib", package = ospsuiteEnv$packageName)
  # Rename platform-specific SQLite dll
  targetDll <- file.path(libDir, "System.Data.SQLite.dll")

  if (Sys.info()[["sysname"]] == "Darwin") {
    macDll <- file.path(libDir, "System.Data.SQLite.mac.dll")
    file.copy(macDll, targetDll, overwrite = TRUE)
  } else {
    othersDll <- file.path(libDir, "System.Data.SQLite.others.dll")
    file.copy(othersDll, targetDll, overwrite = TRUE)
  }

  filePathFor <- function(name) {
    system.file("lib", name, package = ospsuiteEnv$packageName)
  }

  # Make .dll binaries available on windows by extending PATH
  if (.Platform$OS.type == "windows") {
    Sys.setenv(
      PATH = paste(
        system.file("lib", package = ospsuiteEnv$packageName),
        Sys.getenv("PATH"),
        sep = ";"
      )
    )
  }

  if (.Platform$OS.type == "unix") {
    if (Sys.info()[['sysname']] == "Linux") {
      # Load the .so binary files on unix/linux
      for (soFile in list.files(
        system.file("lib", package = ospsuiteEnv$packageName),
        pattern = "\\.so$",
        full.names = TRUE
      )) {
        dyn.load(soFile)
      }
    } else if (Sys.info()[['sysname']] == "Darwin") {
      # Handle SQLite interop separately
      lib_path <- system.file("lib", package = ospsuiteEnv$packageName)
      sqlite_interop_target_name <- "SQLite.Interop.dll.dylib"
      sqlite_interop_target_path <- file.path(
        lib_path,
        sqlite_interop_target_name
      )

      if (Sys.info()[['machine']] == "arm64") {
        sqlite_interop_source_name <- "SQLite.Interop.arm64.dylib"
        sqlite_interop_source_path <- file.path(
          lib_path,
          sqlite_interop_source_name
        )
        if (file.exists(sqlite_interop_source_path)) {
          file.copy(
            sqlite_interop_source_path,
            sqlite_interop_target_path,
            overwrite = TRUE
          )
        }
      } else if (Sys.info()[['machine']] == "x86_64") {
        sqlite_interop_source_name <- "SQLite.Interop.64.dylib"
        sqlite_interop_source_path <- file.path(
          lib_path,
          sqlite_interop_source_name
        )
        if (file.exists(sqlite_interop_source_path)) {
          file.copy(
            sqlite_interop_source_path,
            sqlite_interop_target_path,
            overwrite = TRUE
          )
        }
      }

      # Load the .dylib (not x64.dylib nor Arm64.dylib) binary files on mac
      dylibFile <- list.files(
        system.file("lib", package = ospsuiteEnv$packageName),
        pattern = ".dylib$",
        full.names = TRUE
      )
      filtered_dylibFile <- dylibFile[
        !grepl("\\.(x64|Arm64)\\.dylib$", dylibFile)
      ]
      for (dylibFile in filtered_dylibFile) {
        dyn.load(dylibFile)
      }
    }
  }

  rSharp::loadAssembly(filePathFor("OSPSuite.R.dll"))
  # Initialize once
  netObject <- rSharp::newObjectFromName("OSPSuite.R.ApiConfig")
  apiConfig <- ApiConfig$new(netObject)
  apiConfig$dimensionFilePath <- filePathFor("OSPSuite.Dimensions.xml")
  apiConfig$pkParametersFilePath <- filePathFor("OSPSuite.PKParameters.xml")

  rSharp::callStatic("OSPSuite.R.Api", "InitializeOnce", apiConfig)

  .initializeDimensionAndUnitLists()
}
