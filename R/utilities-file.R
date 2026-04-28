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

