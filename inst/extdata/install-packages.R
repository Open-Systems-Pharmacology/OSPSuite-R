# Message strings used in the setup script
packageInstallationMessages <- list(
  installRTools = "Install Rtools and / or specify a path to an existing installation, then run updateEnvironment(rtoolsPath = path)",
  RToolsNotFound = function(rToolsPath) {
    paste0("Rtools not found at ", rToolsPath, ", cannot continue")
  },
  PKSimLoadFails = "PK-Sim fails to load. The installation might be incompatible with your current version of PK-Sim"
)

# Download paths of released package versions
.releasePaths <- list(
  ospsuite.utils = "https://github.com/Open-Systems-Pharmacology/OSPSuite.RUtils/releases/download/v1.3.17/ospsuite.utils_1.3.17.zip",
  tlf = "https://github.com/Open-Systems-Pharmacology/TLF-Library/releases/download/v1.4.89/tlf_1.4.89.zip",
  ospsuite = "https://github.com/Open-Systems-Pharmacology/OSPSuite-R/releases/download/v11.0.123/ospsuite_11.0.123.zip"
)
# Download paths of latest develop package versions
.developPaths <- list(
  ospsuite.utils = "https://ci.appveyor.com/api/projects/open-systems-pharmacology-ci/ospsuite-rutils/artifacts/ospsuite.utils.zip",
  tlf = "https://ci.appveyor.com/api/projects/open-systems-pharmacology-ci/tlf-library/artifacts/tlf.zip",
  ospsuite = "https://ci.appveyor.com/api/projects/open-systems-pharmacology-ci/ospsuite-r/artifacts/ospsuite.zip"
)

#' Test connection to PK-Sim
#'
#' @param pkSimPath Path where PK-Sim is installed. If this is not specified
#' (`NULL`), path is estimated by the `ospsuite` package.
#'
#' @return `TRUE` if `ospsuite::initPKSim(pkSimPath)` is successful.
testPKSIMConnection <- function(pkSimPath = NULL) {
  # returns TRUE if PK-Sim.R dll can be loaded,
  # or raises errors otherwise
  library(ospsuite)
  ospsuite::initPKSim(pkSimPath)
  return(TRUE)
}

# Remove functions created by this script from the environment
cleanEnvironment <- function() {
  rm(
    packageInstallationMessages,
    .releasePaths,
    .developPaths,
    testPKSIMConnection,
    cleanEnvironment,
    displayProgress,
    installOSPPackages,
    installPackagesGlobally,

    pos = 1
  )
}

displayProgress <- function(current, success = TRUE, message = NULL, suppressOutput = TRUE) {
  states <- c(
    "Installing RENV", "Installing CRAN packages",
    "Checking RTOOLS", "Installing rClr", "Installing ospsuite.utils",
    "Installing tlf", "Installing ospsuite",
    "Testing PK-Sim connection",
    "Installation successful"
  )
  if (suppressOutput) {
    cat("\014")
    for (i in seq_along(states)) {
      cat(paste0(ifelse(i < which(states == current), "V", ifelse(i > which(states == current), ".", ifelse(success == FALSE, "X", ".")))), " ", states[[i]], "\n")
    }
  } else {
    cat(paste0(current, "\n"))
  }
  if (!is.null(message)) {
    message(message)
  }
}

#' Install all osps packages and their dependencies.
#'
#' @param rtoolsPath Path to where rtools are installed. If `NULL` (default),
#' the path is deduced from system environment variables.
#' @param rclrVersion Version of rClr package. Default is 0.9.2 for Windows R4.
#' @param developerVersion If `FALSE` (default), release verions of the packages
#' will be installed. If `TRUE`, latest developer builds of the osps packages
#' will be installed
#' @param suppressOutput
#'
#' @return
#' @export
#'
#' @examples
installOSPPackages <- function(rtoolsPath = NULL, rclrVersion = "0.9.2",
                               suppressOutput = TRUE,
                               developerVersion = FALSE) {
  # Install dependencies from CRAN
  displayProgress("Installing CRAN packages", suppressOutput = suppressOutput)
  install.packages(c("R6", "stringr", "readr", "readxl", "shiny", "dplyr", "tidyr", "ggplot2", "patchwork"),
    dependencies = TRUE
  )

  displayProgress("Checking RTOOLS", suppressOutput = suppressOutput)
  if (Sys.which("make") == "") { # rtools is not found
    if (!is.null(rtoolsPath)) { # adding an existing installation of rtools to path
      Sys.setenv(PATH = paste(rtoolsPath, Sys.getenv("PATH"), sep = ";"))
    } else {
      displayProgress("Checking RTOOLS", success = FALSE, message = packageInstallationMessages$installRTools, suppressOutput = suppressOutput)
      return()
    }
  }
  if (Sys.which("make") == "") {
    displayProgress("Checking RTOOLS", success = FALSE, message = packageInstallationMessages$RToolsNotFound, suppressOutput = suppressOutput)
    return()
  }

  displayProgress("Installing rClr", suppressOutput = suppressOutput)
  if (version$major == "4") {
    install.packages(paste0("https://github.com/Open-Systems-Pharmacology/rClr/releases/download/v", rclrVersion, "/rClr_", rclrVersion, ".zip"), repos = NULL)
  }
  if (version$major == "3") {
    install.packages(paste0("https://github.com/Open-Systems-Pharmacology/rClr/releases/download/v", rclrVersion, "-R3/rClr_", rclrVersion, ".zip"), repos = NULL)
  }

  packagePaths <- .releasePaths
  if (developerVersion) {
    packagePaths <- .developPaths
  }

  displayProgress("Installing ospsuite.utils", suppressOutput = suppressOutput)
  install.packages(packagePaths$ospsuite.utils, repos = NULL)
  displayProgress("Installing tlf", suppressOutput = suppressOutput)
  install.packages(packagePaths$tlf, repos = NULL)
  displayProgress("Installing ospsuite", suppressOutput = suppressOutput)
  install.packages(packagePaths$ospsuite, repos = NULL)
}

#' Install osps packages and their dependencies into global library.
#'
#' @param updatePackages If `TRUE` (default), all installed packages will be
#' updated prior to installation
#' @param pkSimPath Path where PK-Sim is installed. If this is not specified
#' (`NULL`), path is estimated by the `ospsuite` package.
#' @param rtoolsPath Path to where rtools are installed. If `NULL` (default),
#' the path is deduced from system environment variables.
#' @param rclrVersion Version of rClr package. Default is 0.9.2 for Windows R4.
#' @param developerVersion If `FALSE` (default), release verions of the packages
#' will be installed. If `TRUE`, latest developer builds of the osps packages
#' will be installed
#' @param suppressOutput
installPackagesGlobally <- function(updatePackages = TRUE, pkSimPath = NULL,
                                    rtoolsPath = NULL, rclrVersion = "0.9.2",
                                    suppressOutput = TRUE,
                                    developerVersion = FALSE) {
  installOSPPackages(
    rtoolsPath = rtoolsPath, rclrVersion = rclrVersion,
    suppressOutput = suppressOutput,
    developerVersion = developerVersion
  )

  # Update all installed packages from CRAN
  if (updatePackages) {
    update.packages(ask = FALSE)
  }

  displayProgress("Testing PK-Sim connection", suppressOutput = suppressOutput)
  flagConnection <- FALSE
  try(flagConnection <- testPKSIMConnection(pkSimPath = pkSimPath))
  if (!flagConnection) {
    stop(message = packageInstallationMessages$PKSimLoadFails)
  }

  displayProgress("Installation sucessful", suppressOutput = suppressOutput)
}

# Specify the path to latest portable PK-Sim if you are installing the development
# version of the packages
pkSimPath <- NULL
# pkSimPath <- "c:\\Program Files\\Open Systems Pharmacology\\PK-Sim 11.1"
# installPackagesGlobally(updatePackages = TRUE, pkSimPath = pkSimPath, suppressOutput = TRUE, developerVersion = FALSE)

# Clean the workspace
# cleanEnvironment()
