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

  # Load the MoBi project using the ProjectTask
  netObject <- .callProjectTask("LoadProject", .expandPath(filePath))
  mobiProject <- MoBiProject$new(netObject, filePath)

  return(mobiProject)
}

#' Call a method of a MoBi.CLI.Core.Services.ProjectTask
#'
#' @param property The name of the property or method to call on the `ProjectTask`.
#' @param ... Additional arguments to pass to the method.
#' @returns The result of the method call.
#' @internal
#' @noRd
.callProjectTask <- function(property, ...) {
  netTask <- .getMoBiTaskFromCache("ProjectTask")
  results <- netTask$call(property, ...)
  return(results)
}
