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
#' @param units A single unit string or a list of unit strings for the quantity values.
#' If a single string is provided, it will be used for all quantities.
#'
#' @return `individualBuildingBlock`, invisibly.
#' @export
#'
#' @examples
#' individual <- createIndividualBuildingBlock(
#'   species = Species$Human,
#'   population = HumanPopulation$European_ICRP_2002
#' )
#' setParameterValuesInIndividualBB(
#'   individual,
#'   quantityPaths = c("Organism|Age", "Organism|Weight"),
#'   quantityValues = c(30, 73),
#'   units = c("year(s)", "kg")
#' )
setParameterValuesInIndividualBB <- function(
  individualBuildingBlock,
  quantityPaths,
  quantityValues,
  units
) {
  .validateBuildingBlockType(
    individualBuildingBlock,
    BuildingBlockTypes$`Individual`
  )
  validatedInputs <- .validatePVBBInputs(
    individualBuildingBlock,
    quantityPaths,
    quantityValues,
    units,
    dimensions = NULL
  )
  baseValues <- validatedInputs$baseValues
  dimensions <- validatedInputs$dimensions

  netTask <- .getMoBiTaskFromCache("IndividualTask")
  # Get the dimensions for the provided paths from the building block
  refDimensions <- netTask$call(
    "AllDimensionsFrom",
    individualBuildingBlock,
    quantityPaths
  )
  # Check if units are compatible with the reference dimensions. In contrast to the `setParameterValuesInBB` function, changing of dimensions of existing entries is not allowed.
  isUnitSupported <- vapply(
    seq_along(quantityPaths),
    function(x) {
      validateUnit(units[x], refDimensions[x])
    },
    logical(1),
    USE.NAMES = FALSE
  )
  netTask$call(
    "SetIndividualParameter",
    individualBuildingBlock,
    quantityPaths,
    baseValues
  )

  invisible(individualBuildingBlock)
}
