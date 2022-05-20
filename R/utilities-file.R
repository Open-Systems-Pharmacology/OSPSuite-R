# Returns a consistent base-R flavored path
.homogenizePath <- function(winPath) {
  if (is.na(winPath) || !nzchar(winPath)) {
    return(NA_character_)
  }

  # get consistency (unix separators) with R-base
  result <- gsub("\\\\", "/", winPath)

  # remove trailing slash for consistency with R-base
  result <- sub("/$", "", result)

  return(result)
}

# Adds the lib folder of the package to the path
.addPackageLibToPath <- function() {
  libPath <- system.file("lib", package = ospsuiteEnv$packageName)
  .addPathToSystemPath(libPath)
}

# Adds the parameter `pathToAdd` to the system path
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


#' Returns the expanded path for `path` and ensure that encoding is applied properly
#'
#' @param  path Path to expand
expandPath <- function(path) {
  path.expand(enc2utf8(path))
}
