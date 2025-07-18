#' Load a MoBi project
#'
#' @param filePath Path of 'mbp3' MoBi project file to load.
#'
#' @returns An object of the `MoBiProject` type.
#' @export
#'
#' @examples
#' projectPath <- system.file("extdata", "simple.mbp3", package = "ospsuite")
#'
#' myProject <- loadMoBiProject(projectPath)
loadMoBiProject <- function(filePath) {
  # .NET task that handles loading of a MoBi project
  netTask <- .getNetTask("SomeMoBiTask")

  netObject <- netTask$call("LoadMoBiProject", .expandPath(filePath))
  mobiProject <- MoBiProject$new(netObject, filePath)

  return(mobiProject)
}
