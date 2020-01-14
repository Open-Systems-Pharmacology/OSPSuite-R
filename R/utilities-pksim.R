
#' Loads the PKSim.R that will enable create individual and create population workflows.
#' @param pksimFolderPath Path where PK-Sim is installed. If this is not specified, path will be read from registry using the package version
#'
#' @note  This will only work on Windows machine and should not be called on any other OS.
#' This function should also only be called explicitely when using a portable install of the package
#'
#' @import rClr
#' @export
initPKSim <- function(pksimFolderPath = NULL) {
  if (ospsuiteEnv$isPKSimLoaded) {
    return()
  }

  pksimFolderPath <- pksimFolderPath %||% .getPathToPKSimInstallDir()
  .addPathToSystemPath(pksimFolderPath)
  pksimR <- file.path(pksimFolderPath, "PKSim.R.dll")
  if (!file.exists(pksimR)) {
    stop(messages$pkSimRPathInvalid(pksimR))
  }
  rClr::clrLoadAssembly(pksimR)
  rClr::clrCallStatic("PKSim.R.Api", "InitializeOnce")

  # Only set the flag if initialization was successful
  ospsuiteEnv$isPKSimLoaded <- TRUE
  print(ospsuiteEnv$isPKSimLoaded)
}


#' Tries to find the installation path for a specific version of PK-Sim from an Windows registry entry.
#'
#' @param pksim.version The version number of Pk-Sim as a string.
#'
#' @return
#' The path to the PK-Sim installation for version pksim.version or NA if no path could be found.
#' The path is separated with slashes (unix-style) and in compilance with base-R without a trailing slash.
#'
#' @examples
#'
#' \dontrun{
#' path <- .getPathToPKSimInstallDirFromRegistry("7.4")
#' }
.getPathToPKSimInstallDirFromRegistry <- function(pksim.version) {
  pksimVersion <- trimws(pksim.version)

  if (.Platform$OS.type != "windows") {
    stop("Only Windows platforms are supported")
  }

  suite.name <- ospsuiteEnv$suiteName
  product.name <- "PK-Sim"
  reg.path <- file.path("SOFTWARE",
    suite.name,
    product.name,
    pksim.version,
    fsep = "\\"
  )

  reg.entry <- NA
  try(reg.entry <- utils::readRegistry(reg.path, hive = "HLM", maxdepth = 1, view = "64-bit"),
    silent = TRUE
  )

  if ("InstallDir" %in% names(reg.entry)) {
    return(.homogenizePath(reg.entry$InstallDir))
  }

  return(NA)
}

#' Tries to find the installation path for a specific version of PK-Sim via the filesystem.
#' Searching is done in the following order:
#' 1. Search via filesystem in a guessed installation folder from the base.search.folder
#' 3. Search via filesystem for PKSim.exe recursivly from the defined base.search.folder (fallback)
#'
#' @param pksim.version The version number of Pk-Sim as a string.
#' @param base.search.folder The base folder for filesystem-lookup fallback (default: 64-bit program folder)
#'
#' @return
#' The path to the PK-Sim installation for version pksim.version or NA if no path could be found.
#' The path is separated with slashes (unix-style) and in compilance with base-R without a trailing slash.
#' If more than one matching path is found a warning is produced.
#'
#' @examples
#' \dontrun{
#' path <- .getPathToPKSimInstallDirFromFileSystem("7.4")
#' path2 <- .getPathToPKSimInstallDirFromFileSystem("7.5", "C:/MyOSPFolder/")
#' }
.getPathToPKSimInstallDirFromFileSystem <- function(pksim.version,
                                                    base.search.folder = Sys.getenv("ProgramW6432")) {
  pksim.version <- trimws(pksim.version)
  base.search.folder <- trimws(base.search.folder)
  base.search.folder <- normalizePath(base.search.folder)

  if (.Platform$OS.type != "windows") {
    stop("Only Windows platforms are supported")
  }

  if (!nzchar(base.search.folder)) {
    return(NA)
  }

  # First guess: OSP/PK-Sim folder
  suite.name <- ospsuiteEnv$suiteName
  product.name <- "PK-Sim"

  full.guess <- file.path(base.search.folder, suite.name, fsep = "\\")
  full.match <- dir(full.guess,
    pattern = product.name, full.names = TRUE,
    include.dirs = TRUE
  )
  if (length(full.match) == 0) {
    # Second guess: Search base folder recursivly for exe.name
    # This might be expensive !
    exe.name <- "PKSim.exe$"
    full.match <- list.files(base.search.folder,
      pattern = exe.name,
      recursive = TRUE, full.names = TRUE
    )
    full.match <- dirname(full.match)
  }

  if (length(full.match) != 0) {
    # match version
    full.match <- grep(pksim.version, full.match, fixed = TRUE, value = TRUE)
    if (length(full.match) > 1) {
      warning("Ambiguous matches for PK-Sim installation path found. First match is returned.")
    }

    return(.homogenizePath(full.match[1]))
  }

  return(NA)
}

#' Tries to find the installation path for a specific version of PK-Sim.
#' Searching is done in the following order:
#' 1. Search via Windows registry entry
#' 2. Search via filesystem in a guessed installation folder from the base.search.folder (fallback 1)
#' 3. Search via filesystem for PKSim.exe recursivly from the defined base.search.folder (fallback 2)
#'
#' @param pksim.version The version number of Pk-Sim as a string.
#' @param base.search.folder The base folder for filesystem-lookup fallback (default: 64-bit program folder)
#'
#' @return
#' The path to the PK-Sim installation for version pksim.version or NA if no path could be found.
#' The path is separated with slashes (unix-style) and in compilance with base-R without a trailing slash.
#' If more than one matching path is found a warning is produced.
#'
#' @examples
#' \dontrun{
#' path <- .getPathToPKSimInstallDir("7.4")
#' path2 <- .getPathToPKSimInstallDir("7.5", "C:/MyOSPFolder/")
#' }
.getPathToPKSimInstallDir <- function(pksim.version = ospsuiteEnv$packageVersion,
                                      base.search.folder = Sys.getenv("ProgramW6432")) {
  pksim.path <- .getPathToPKSimInstallDirFromRegistry(pksim.version)
  if (!is.na(pksim.path)) {
    print(pksim.path)
    return(pksim.path)
  }

  pksim.path <- .getPathToPKSimInstallDirFromFileSystem(pksim.version, base.search.folder)
  if (!is.na(pksim.path)) {
    return(pksim.path)
  }

  return(NA)
}
