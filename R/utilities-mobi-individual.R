#' @title Set Parameters of a MoBi Individual Building Block
#'
#' @description
#' Sets one or more parameter values in an individual building block by
#' providing the corresponding quantity paths and values. The number of
#' paths and values must be equal.
#'
#' @param individualBuildingBlock A `BuildingBlock` object as returned
#'   by `createIndividualBuildingBlock()`.
#' @param quantityPaths A character vector of quantity paths to set.
#' @param quantityValues A numeric vector of values to assign. Must have the
#'   same length as `quantityPaths`.
#'
#' @return `individualBuildingBlock`, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' individual <- createIndividualBuildingBlock(
#'   species = Species$Human,
#'   population = HumanPopulation$European_ICRP_2002
#' )
#' setMoBiIndividualParameters(
#'   individual,
#'   quantityPaths = c("Organism|Age", "Organism|Weight"),
#'   quantityValues = c(30, 73)
#' )
#' }
setMoBiIndividualParameters <- function(
  individualBuildingBlock,
  quantityPaths,
  quantityValues
) {
  validateIsOfType(individualBuildingBlock, "BuildingBlock")
  validateIsString(quantityPaths)
  validateIsNumeric(quantityValues)
  validateIsSameLength(quantityPaths, quantityValues)

  netTask <- .getMoBiTaskFromCache("IndividualTask")
  netTask$call(
    "SetIndividualParameter",
    individualBuildingBlock,
    quantityPaths,
    quantityValues
  )

  invisible(individualBuildingBlock)
}
