# Returns a consistent base-R flavoured path
.homogenizePath <- function(winPath) {
  if (is.na(winPath) || !nzchar(winPath))
    return(NA)

  # get consistency (unix speparators) with R-base
  result <- normalizePath(winPath, winslash="/", mustWork = FALSE)

  # remove trailing slash for consistancy with R-base
  result <- sub("/$", "", result)

  return(result)
}

#Adds the lib folder of the package to the path
.addPackageLibToPath <- function() {
  libPath <- system.file("lib", package = ospsuiteEnv$packageName)
  .addPathToSystemPath(libPath)
}

#Adds the paramter pathToAdd to the system path
.addPathToSystemPath <- function(pathToAdd){
  if (file.exists(pathToAdd)) {
    Sys.setenv(path = paste(pathToAdd, Sys.getenv("path"), sep = ";"))
  }
}
