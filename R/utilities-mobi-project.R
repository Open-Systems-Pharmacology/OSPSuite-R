#' Load a MoBi project
#'
#' @param filePath Path of 'mbp3' MoBi project file to load.
#'
#' @returns An object of the `MoBiProject` type.
#' @export
#'
#' @examples
#' \dontrun{
#' projectPath <- system.file("extdata", "simple.mbp3", package = "ospsuite")
#'
#' myProject <- loadMoBiProject(projectPath)
#' }
loadMoBiProject <- function(filePath) {
  if (!file.exists(filePath)) {
    stop(paste0("File does not exist: ", filePath))
  }
  validateIsFileExtension(filePath, "mbp3")
  filePath <- normalizePath(filePath, winslash = "/")
  # .NET task that handles loading of a MoBi project
  netTask <- .getMoBiTaskFromCache("ProjectTask")

  netObject <- netTask$call("LoadProject", .expandPath(filePath))
  mobiProject <- MoBiProject$new(netObject, filePath)

  return(mobiProject)
}
