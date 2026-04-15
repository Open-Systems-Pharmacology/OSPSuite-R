#' Convert an Expression Profiles Building Block to data frames.
#'
#' Returns both the expression parameter values and the initial conditions
#' contained in the expression profile building block.
#'
#' @param expressionProfilesBuildingBlock A `BuildingBlock` object of type `Expression Profile`.
#'
#' @returns A named list with two data frames:
#' - `expressionParameters`: A data frame with the following columns:
#'   - `Container Path`: Full path to the container where the parameter is located.
#'   - `Parameter Name`: Name of the parameter.
#'   - `Value`: Value of the parameter. For values that are defined by a formula, the return value can be `NaN`.
#'   - `Unit`: Unit of the parameter value.
#'   - `Value Origin`: Origin of the parameter value.
#' - `initialConditions`: A data frame with the following columns:
#'   - `Container Path`: Full path to the container where the molecule is located.
#'   - `Molecule Name`: Name of the molecule.
#'   - `Is Present`: Boolean indicating if the molecule is present.
#'   - `Value`: Initial value of the molecule. For values that are defined by a formula, the return value can be `NaN`.
#'   - `Unit`: Unit of the initial value.
#'   - `Scale Divisor`: Scale divisor for the initial value.
#'   - `Neg. Values Allowed`: Boolean indicating if negative values are allowed.
#'
#' @export
expressionProfileBBToDataFrame <- function(expressionProfilesBuildingBlock) {
  .validateBuildingBlockType(
    expressionProfilesBuildingBlock,
    BuildingBlockTypes$`Expression Profile`
  )

  expressionParametersDf <- .bbWithParameterValuesToDataFrame(
    expressionProfilesBuildingBlock
  )

  initialConditionsDf <- .bbWithInitialConditionsToDataFrame(
    expressionProfilesBuildingBlock
  )

  return(list(
    expressionParameters = expressionParametersDf,
    initialConditions = initialConditionsDf
  ))
}

#' Create a MoBi Expression Profile Building Block
#'
#' @title Create a MoBi Expression Profile Building Block
#'
#' @description
#' Creates an expression profile building block in MoBi for a given molecule,
#' species, and category by calling the MoBi `ExpressionProfileTask`.
#'
#' @param category Category of the expression profile.
#' @param moleculeName Name of the molecule.
#' @param speciesName Name of the species.
#'
#' @return A `BuildingBlock` object representing the created expression profile.
#' @export
#'
#' @examples
#' \dontrun{
#' expressionProfile <- createMoBiExpressionProfileBuildingBlock(
#'   category = "MyCategory",
#'   moleculeName = "CYP3A4",
#'   speciesName = "Human"
#' )
#' }
createMoBiExpressionProfileBuildingBlock <- function(
  category,
  moleculeName,
  speciesName
) {
  ospsuite.utils::validateHasOnlyNonEmptyStrings(category)
  ospsuite.utils::validateHasOnlyNonEmptyStrings(moleculeName)
  validateEnumValue(speciesName, Species)

  netTask <- .getMoBiTaskFromCache("ExpressionProfileTask")
  netObject <- netTask$call(
    "CreateExpressionProfile",
    category,
    moleculeName,
    speciesName
  )

  return(BuildingBlock$new(
    netObject,
    type = BuildingBlockTypes$ExpressionProfile
  ))
}

#' Set Parameters of a MoBi Expression Profile Building Block
#'
#' @title Set Parameters of a MoBi Expression Profile Building Block
#'
#' @description
#' Sets one or more parameter values in an expression profile building block
#' by providing the corresponding quantity paths and values. The number of
#' paths and values must be equal.
#'
#' @param expressionProfileBuildingBlock A `BuildingBlock` object as returned
#'   by `createMoBiExpressionProfileBuildingBlock()`.
#' @param quantityPaths A character vector of quantity paths to set.
#' @param quantityValues A numeric vector of values to assign. Must have the
#'   same length as `quantityPaths`.
#'
#' @return `expressionProfileBuildingBlock`, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' expressionProfile <- createMoBiExpressionProfileBuildingBlock(
#'   category = "MyCategory",
#'   moleculeName = "CYP3A4",
#'   speciesName = "Human"
#' )
#' setMoBiExpressionProfileParameters(
#'   expressionProfile,
#'   quantityPaths = c("Liver|Intracellular|CYP3A4|RelativeExpression"),
#'   quantityValues = c(1.0)
#' )
#' }
setMoBiExpressionProfileParameters <- function(
  expressionProfileBuildingBlock,
  quantityPaths,
  quantityValues
) {
  validateIsOfType(expressionProfileBuildingBlock, "BuildingBlock")
  validateIsString(quantityPaths)
  validateIsNumeric(quantityValues)
  validateIsSameLength(quantityPaths, quantityValues)

  netTask <- .getMoBiTaskFromCache("ExpressionProfileTask")
  netTask$call(
    "SetExpressionParameter",
    expressionProfileBuildingBlock,
    quantityPaths,
    quantityValues
  )

  invisible(expressionProfileBuildingBlock)
}
