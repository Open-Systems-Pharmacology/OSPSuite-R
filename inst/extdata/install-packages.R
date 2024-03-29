# Message strings used in the setup script
packageInstallationMessages <- list(
  installRTools = "Install Rtools compatible with your R version, then run updateEnvironment() again",
  PKSimLoadFails = "PK-Sim fails to load. The installation might be incompatible with your current version of PK-Sim"
)

# Packages that will be installed from CRAN
.cranPackages <- c("R6", "stringr", "readr", "readxl", "shiny", "dplyr", "tidyr", "ggplot2", "purrr", "patchwork", "jsonlite")

# Download paths of released package versions
.releasePaths <- list(
  ospsuite.utils = "https://github.com/Open-Systems-Pharmacology/OSPSuite.RUtils/releases/download/v1.3.17/ospsuite.utils_1.3.17.tar.gz",
  tlf = "https://github.com/Open-Systems-Pharmacology/TLF-Library/releases/download/v1.4.89/tlf_1.4.89.tar.gz",
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
    .cranPackages,
    pos = 1
  )
}

displayProgress <- function(current, success = TRUE, message = NULL, suppressOutput = TRUE) {
  states <- c(
    "Installing RENV", "Checking RTOOLS", "Installing CRAN packages",
    "Installing rClr", "Installing ospsuite.utils",
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
#' @param rclrVersion Version of rClr package. Default is 0.9.2 for Windows R4.
#' @param developerVersion If `FALSE` (default), release verions of the packages
#' will be installed. If `TRUE`, latest developer builds of the osps packages
#' will be installed
#' @param lib character vector giving the library directories where to install
#' the packages. Recycled as needed. If missing, defaults to the first element of .libPaths().
#' @param suppressOutput
#'
#' @return
#' @export
#'
#' @examples
installOSPPackages <- function(rclrVersion = "0.9.2",
                               suppressOutput = FALSE,
                               developerVersion = FALSE,
                               lib = NULL,
                               ...) {
  install.packages("pkgbuild", lib = lib)
  displayProgress("Checking RTOOLS", suppressOutput = suppressOutput)
  if (!pkgbuild::has_rtools()) { # rtools is not found
      displayProgress("Checking RTOOLS", success = FALSE, message = packageInstallationMessages$installRTools, suppressOutput = suppressOutput)
      return()
    }

  # Install dependencies from CRAN
  displayProgress("Installing CRAN packages", suppressOutput = suppressOutput)
  install.packages(.cranPackages,
    dependencies = TRUE,
    lib = lib
  )

  displayProgress("Installing rClr", suppressOutput = suppressOutput)
  if (version$major == "4") {
    install.packages(paste0("https://github.com/Open-Systems-Pharmacology/rClr/releases/download/v", rclrVersion, "/rClr_", rclrVersion, ".zip"), repos = NULL,
                     lib = lib)
  }
  if (version$major == "3") {
    install.packages(paste0("https://github.com/Open-Systems-Pharmacology/rClr/releases/download/v", rclrVersion, "-R3/rClr_", rclrVersion, ".zip"), repos = NULL,
                     lib = lib)
  }

  packagePaths <- .releasePaths
  if (developerVersion) {
    packagePaths <- .developPaths
  }

  displayProgress("Installing ospsuite.utils", suppressOutput = suppressOutput)
  install.packages(packagePaths$ospsuite.utils, repos = NULL, lib = lib, ...)
  displayProgress("Installing tlf", suppressOutput = suppressOutput)
  install.packages(packagePaths$tlf, repos = NULL, lib = lib, ...)
  displayProgress("Installing ospsuite", suppressOutput = suppressOutput)
  install.packages(packagePaths$ospsuite, repos = NULL, lib = lib, ...)
}

# Run this function to install the packages
# installOSPPackages(developerVersion = FALSE)

# Test if PK-Sim connection can be established
# Specify the path to latest portable PK-Sim if you are installing the development
# version of the packages
pkSimPath <- NULL
# pkSimPath <- "c:\\Program Files\\Open Systems Pharmacology\\PK-Sim 11.1"
# testPKSIMConnection(pkSimPath = pkSimPath)


# Clean the workspace
# cleanEnvironment()
