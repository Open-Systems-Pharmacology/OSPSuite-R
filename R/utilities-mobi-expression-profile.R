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
  validateIsNonEmptyString(category, "category")
  validateIsNonEmptyString(moleculeName, "moleculeName")
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
