#' Create a MoBi Expression Profile Building Block
#'
#' @param category Category of the expression profile.
#' @param moleculeName Name of the molecule.
#' @param speciesName Name of the species.
#'
#' @returns An object of type `BuildingBlock` representing an expression profile.
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
  validateIsString(category)
  validateIsString(moleculeName)
  validateIsString(speciesName)

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

#' Set parameters of a MoBi Expression Profile Building Block
#'
#' @param expressionProfileBuildingBlock A `BuildingBlock` object as returned
#'   by `createMoBiExpressionProfileBuildingBlock()`.
#' @param quantityPaths A character vector of quantity paths to set.
#' @param quantityValues A numeric vector of values to assign. Must have the
#'   same length as `quantityPaths`.
#'
#' @returns `expressionProfileBuildingBlock`, invisibly.
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
