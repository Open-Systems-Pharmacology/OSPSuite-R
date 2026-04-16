.getPKSimNetTask <- function(taskName) {
  initPKSim()
  rSharp::callStatic("PKSim.R.Api", paste0("Get", taskName))
}

.loadBuildingBlockFromPKML <- function(filePath, taskName, buildingBlockClass) {
  validateIsString(filePath)
  filePath <- .expandPath(filePath)
  buildingBlockTask <- .getPKSimNetTask(taskName)
  buildingBlock <- buildingBlockTask$call("ImportFromPKML", filePath)
  buildingBlockClass$new(buildingBlock)
}

.saveBuildingBlockToPKML <- function(buildingBlock, filePath, taskName, type) {
  validateIsOfType(buildingBlock, type = type)
  validateIsString(filePath)
  filePath <- .expandPath(filePath)
  buildingBlockTask <- .getPKSimNetTask(taskName)
  buildingBlockTask$call("ExportToPKML", buildingBlock, filePath)
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
