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

  # Get writable directory (fallback to temp if needed)
  writableDir <- .getWritableLibDir(libDir)

  # Store in environment for later use
  ospsuiteEnv$writeableLibDir <- writableDir

  # Helper function for file paths (use writable directory)
  filePathFor <- function(name) {
    file.path(writableDir, name)
  }

  # Copy all necessary files to writable directory if needed
  if (writableDir != libDir) {
    # Check if cache needs refresh
    if (.cacheNeedsRefresh(libDir, writableDir)) {
      # Copy all files from source to writable directory
      .copyFilesIfNeeded(
        libDir,
        writableDir
      )
      # Create marker file to track cache creation time
      .createCacheMarker(writableDir)
    }
  }

  # Setup platform-specific SQLite DLL
  targetDll <- file.path(writableDir, "System.Data.SQLite.dll")
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
    Sys.setenv(PATH = paste(writableDir, Sys.getenv("PATH"), sep = ";"))
  } else if (.Platform$OS.type == "unix") {
    sysname <- Sys.info()[['sysname']]

    if (sysname == "Linux") {
      # Load shared object files (.so) on Linux
      soFiles <- list.files(writableDir, pattern = "\\.so$", full.names = TRUE)
      for (soFile in soFiles) {
        dyn.load(soFile)
      }
    } else if (sysname == "Darwin") {
      # macOS: Setup SQLite interop
      machine <- Sys.info()[['machine']]
      targetPath <- file.path(writableDir, "SQLite.Interop.dll.dylib")

      # Determine source file based on architecture
      sourceFile <- if (machine == "arm64") {
        "SQLite.Interop.arm64.dylib"
      } else if (machine == "x86_64") {
        "SQLite.Interop.x64.dylib"
      } else {
        stop("Unknown architecture: ", machine)
      }

      # Copy the appropriate SQLite.Interop file to its generic name
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
        targetPath <- file.path(writableDir, paste0(libName, ".dylib"))
        file.copy(sourcePath, targetPath, overwrite = TRUE)
      }

      # Load macOS dynamic libraries (.dylib)
      dylibFiles <- list.files(
        writableDir,
        pattern = "\\.dylib$",
        full.names = TRUE
      )
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

#' Check if a directory is writable
#'
#' @param path Directory path to check
#' @return Logical indicating if directory is writable
#' @keywords internal
.isWritable <- function(path) {
  if (!dir.exists(path)) {
    return(FALSE)
  }

  # Try to create a temporary file to test write permissions
  testFile <- file.path(
    path,
    paste0(".write_test_", Sys.getpid(), "_", as.numeric(Sys.time()))
  )
  tryCatch(
    {
      writeLines("test", testFile)
      file.remove(testFile)
      TRUE
    },
    error = function(e) {
      FALSE
    },
    warning = function(w) {
      FALSE
    }
  )
}

#' Get or create a user-specific temp directory for OSPSuite files
#'
#' @param libDir Original library directory
#' @return Path to writable directory (either libDir or temp fallback)
#' @keywords internal
.getWritableLibDir <- function(libDir) {
  # Check if original lib directory is writable
  if (.isWritable(libDir)) {
    return(libDir)
  }

  # Create user-specific persistent temp directory using official R user directory
  # This follows CRAN policy and XDG Base Directory Specification
  userTempDir <- tools::R_user_dir("ospsuite", which = "cache")

  # Create directory if it doesn't exist
  if (!dir.exists(userTempDir)) {
    dir.create(userTempDir, recursive = TRUE, showWarnings = FALSE)
  }

  # Check if we can write to temp directory
  if (!.isWritable(userTempDir)) {
    stop("Cannot write to temp directory: ", userTempDir)
  }

  return(userTempDir)
}

#' Copy all files from source to writable directory
#'
#' @param sourceDir Source directory
#' @param targetDir Target directory
#' @return Logical indicating if any files were copied
#' @keywords internal
.copyFilesIfNeeded <- function(sourceDir, targetDir) {
  filesCopied <- FALSE

  # Get all files in source directory
  sourceFiles <- list.files(sourceDir, full.names = TRUE, recursive = TRUE)

  for (sourceFile in sourceFiles) {
    # Calculate relative path from source directory
    relPath <- gsub(paste0("^", sourceDir, "/?"), "", sourceFile)
    targetFile <- file.path(targetDir, relPath)

    # Create target directory if it doesn't exist
    targetDirPath <- dirname(targetFile)
    if (!dir.exists(targetDirPath)) {
      dir.create(targetDirPath, recursive = TRUE, showWarnings = FALSE)
    }

    # Copy file (mtime checking is handled at higher level)
    tryCatch(
      {
        file.copy(sourceFile, targetFile, overwrite = TRUE)
        filesCopied <- TRUE
      },
      error = function(e) {
        warning(
          "Failed to copy ",
          sourceFile,
          " to ",
          targetFile,
          ": ",
          e$message
        )
      }
    )
  }

  return(filesCopied)
}

#' Check if cache needs refresh by comparing source package mtime with cache marker
#'
#' @param sourceDir Source package lib directory
#' @param cacheDir Cache directory
#' @return Logical indicating if cache needs refresh
#' @keywords internal
.cacheNeedsRefresh <- function(sourceDir, cacheDir) {
  markerFile <- file.path(cacheDir, ".ospsuite_cache_marker")

  # If no marker file exists, cache needs refresh
  if (!file.exists(markerFile)) {
    return(TRUE)
  }

  # Get the most recent mtime from ALL files in source directory
  sourceFiles <- list.files(
    sourceDir,
    full.names = TRUE,
    recursive = TRUE
  )
  if (length(sourceFiles) == 0) {
    return(TRUE)
  }

  sourceMtime <- max(file.mtime(sourceFiles))
  cacheMtime <- file.mtime(markerFile)

  # If source files are newer than cache marker, cache needs refresh
  return(sourceMtime > cacheMtime)
}

#' Create cache marker file with current timestamp
#'
#' @param cacheDir Cache directory
#' @keywords internal
.createCacheMarker <- function(cacheDir) {
  markerFile <- file.path(cacheDir, ".ospsuite_cache_marker")
  writeLines(as.character(Sys.time()), markerFile)
}
