#' Returns a PK-Sim task instance from `PKSim.R.Api`
#'
#' @param taskName Task name suffix (without the `Get` prefix), for example `ExpressionProfileTask`.
#' @keywords internal
.getPKSimNetTask <- function(taskName) {
  initPKSim()
  rSharp::callStatic("PKSim.R.Api", paste0("Get", taskName))
}

#' Loads a PK-Sim building block from PKML
#'
#' @param filePath Full path of PKML file to load.
#' @param taskName PK-Sim task name suffix used to load the building block.
#' @param buildingBlockClass R6 wrapper class for the loaded building block.
#' @keywords internal
.loadBuildingBlockFromPKML <- function(filePath, taskName, buildingBlockClass) {
  validateIsString(filePath)
  filePath <- .expandPath(filePath)
  buildingBlockTask <- .getPKSimNetTask(taskName)
  buildingBlock <- buildingBlockTask$call("ImportFromPKML", filePath)
  buildingBlockClass$new(buildingBlock)
}

#' Saves a PK-Sim building block to PKML
#'
#' @param buildingBlock Building block object to save.
#' @param filePath Full path where the PKML file will be created.
#' @param taskName PK-Sim task name suffix used to save the building block.
#' @param type Expected type name of `buildingBlock`.
#' @keywords internal
.saveBuildingBlockToPKML <- function(buildingBlock, filePath, taskName, type) {
  validateIsOfType(buildingBlock, type = type)
  validateIsString(filePath)
  filePath <- .expandPath(filePath)
  outputDirectory <- dirname(filePath)
  if (!dir.exists(outputDirectory)) {
    stop(messages$errorDirectoryDoesNotExist(outputDirectory))
  }
  buildingBlockTask <- .getPKSimNetTask(taskName)
  tryCatch(
    {
      buildingBlockTask$call("ExportToPKML", buildingBlock, filePath)
    },
    error = function(e) {
      stop(messages$errorExportBuildingBlockToPKML(type, filePath, e$message))
    }
  )
  invisible()
}

#' Load an expression profile building block from PKML
#'
#' @param filePath Full path of PKML file containing an expression profile.
#'
#' @return `ExpressionProfile` object.
#' @export
loadExpressionProfileFromPKML <- function(filePath) {
  .loadBuildingBlockFromPKML(
    filePath = filePath,
    taskName = "ExpressionProfileTask",
    buildingBlockClass = ExpressionProfile
  )
}

#' Save an expression profile building block to PKML
#'
#' @param expressionProfile `ExpressionProfile` object.
#' @param filePath Full path where the PKML file should be saved.
#' @export
saveExpressionProfileToPKML <- function(expressionProfile, filePath) {
  .saveBuildingBlockToPKML(
    buildingBlock = expressionProfile,
    filePath = filePath,
    taskName = "ExpressionProfileTask",
    type = "ExpressionProfile"
  )
}

#' Load an individual building block from PKML
#'
#' @param filePath Full path of PKML file containing an individual building block.
#'
#' @return `Individual` object.
#' @export
loadIndividualFromPKML <- function(filePath) {
  .loadBuildingBlockFromPKML(
    filePath = filePath,
    taskName = "IndividualTask",
    buildingBlockClass = Individual
  )
}

#' Save an individual building block to PKML
#'
#' @param individual `Individual` object.
#' @param filePath Full path where the PKML file should be saved.
#' @export
saveIndividualToPKML <- function(individual, filePath) {
  .saveBuildingBlockToPKML(
    buildingBlock = individual,
    filePath = filePath,
    taskName = "IndividualTask",
    type = "Individual"
  )
}

#' Load an initial conditions building block from PKML
#'
#' @param filePath Full path of PKML file containing initial conditions.
#'
#' @return `InitialConditions` object.
#' @export
loadInitialConditionsFromPKML <- function(filePath) {
  .loadBuildingBlockFromPKML(
    filePath = filePath,
    taskName = "InitialConditionsTask",
    buildingBlockClass = InitialConditions
  )
}

#' Save an initial conditions building block to PKML
#'
#' @param initialConditions `InitialConditions` object.
#' @param filePath Full path where the PKML file should be saved.
#' @export
saveInitialConditionsToPKML <- function(initialConditions, filePath) {
  .saveBuildingBlockToPKML(
    buildingBlock = initialConditions,
    filePath = filePath,
    taskName = "InitialConditionsTask",
    type = "InitialConditions"
  )
}

#' Load a parameter values building block from PKML
#'
#' @param filePath Full path of PKML file containing parameter values.
#'
#' @return `ParameterValues` object.
#' @export
loadParameterValuesFromPKML <- function(filePath) {
  .loadBuildingBlockFromPKML(
    filePath = filePath,
    taskName = "ParameterValuesTask",
    buildingBlockClass = ParameterValues
  )
}

#' Save a parameter values building block to PKML
#'
#' @param parameterValues `ParameterValues` object.
#' @param filePath Full path where the PKML file should be saved.
#' @export
saveParameterValuesToPKML <- function(parameterValues, filePath) {
  .saveBuildingBlockToPKML(
    buildingBlock = parameterValues,
    filePath = filePath,
    taskName = "ParameterValuesTask",
    type = "ParameterValues"
  )
}
