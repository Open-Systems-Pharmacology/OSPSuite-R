# Returns a consistent base-R flavoured path
.homogenizePath <- function(winPath) {
  if (is.na(winPath) || !nzchar(winPath)) {
    return(NA)
  }

  # get consistency (unix speparators) with R-base
  result <- gsub("\\\\", "/", winPath)

  # remove trailing slash for consistancy with R-base
  result <- sub("/$", "", result)

  return(result)
}

# Adds the lib folder of the package to the path
.addPackageLibToPath <- function() {
  libPath <- system.file("lib", package = ospsuiteEnv$packageName)
  .addPathToSystemPath(libPath)
}

# Adds the paramter pathToAdd to the system path
.addPathToSystemPath <- function(pathToAdd) {
  if (!file.exists(pathToAdd)) {
    return()
  }

  # Adding path only supported on windows
  if (.Platform$OS.type != "windows") {
    return()
  }

  Sys.setenv(path = paste(pathToAdd, Sys.getenv("path"), sep = ";"))
}
