#' Convert Expression Profile Building Blocks to data frames
#'
#' Returns both the expression parameter values and the initial conditions
#' contained in the expression profile building block(s).
#'
#' @param expressionProfilesBuildingBlock A single `BuildingBlock` object of
#'   type `Expression Profile`, or a list of such objects. When a list is
#'   provided, the resulting data frames are row-bound across all building
#'   blocks.
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
#' @examples
#' expressionProfile <- createExpressionProfileBuildingBlock(
#'   type = ExpressionProfileCategories$`Metabolizing Enzyme`,
#'   moleculeName = "CYP3A4",
#'   speciesName = "Human"
#' )
#' result <- expressionProfileBBToDataFrame(expressionProfile)
#' # Access the expression parameters
#' result$expressionParameters
#' # Access the initial conditions
#' result$initialConditions
expressionProfileBBToDataFrame <- function(expressionProfilesBuildingBlock) {
  buildingBlocks <- toList(expressionProfilesBuildingBlock)

  expressionParametersList <- list()
  initialConditionsList <- list()

  for (bb in buildingBlocks) {
    .validateBuildingBlockType(bb, BuildingBlockTypes$`Expression Profile`)

    expressionParametersList <- c(
      expressionParametersList,
      list(.bbWithParameterValuesToDataFrame(bb))
    )
    initialConditionsList <- c(
      initialConditionsList,
      list(.bbWithInitialConditionsToDataFrame(bb))
    )
  }

  return(list(
    expressionParameters = dplyr::bind_rows(expressionParametersList),
    initialConditions = dplyr::bind_rows(initialConditionsList)
  ))
}

#' Create an Expression Profile Building Block
#'
#' Creates an expression profile building block in MoBi for a given molecule,
#' species, and category. The expression profile is created with default parameter values, which can be modified using `setExpressionProfileParameters()`.
#' Important: The expression profile is not queried from the expression database.
#'
#' Disease states are currently not supported.
#'
#' @param type String. Type of the protein the profile will be created for. Must be one of the values defined in the `ExpressionProfileCategories` enum.
#' @param moleculeName Name of the protein molecule.
#' @param speciesName Name of the species. Must be one of the values defined in the `Species` enum.
#' @param phenotype Phenotype of the expression profile. Defaults to `"healthy"`.
#'
#' @return A `BuildingBlock` object representing the created expression profile.
#' @export
#'
#' @examples
#' expressionProfile <- createExpressionProfileBuildingBlock(
#'   type = ExpressionProfileCategories$`Metabolizing Enzyme`,
#'   moleculeName = "CYP3A4",
#'   speciesName = "Human"
#' )
createExpressionProfileBuildingBlock <- function(
  type,
  moleculeName,
  speciesName,
  phenotype = "healthy"
) {
  validateEnumValue(type, ExpressionProfileCategories)
  validateIsString(moleculeName)
  ospsuite.utils::validateHasOnlyNonEmptyStrings(moleculeName)
  validateEnumValue(speciesName, Species)
  validateIsString(phenotype)

  netTask <- .getMoBiTaskFromCache("ExpressionProfileTask")
  netObject <- netTask$call(
    "CreateExpressionProfile",
    type,
    moleculeName,
    speciesName,
    phenotype
  )

  return(BuildingBlock$new(
    netObject,
    type = BuildingBlockTypes$`Expression Profile`
  ))
}

#' Set Parameters of an Expression Profile Building Block
#'
#' Sets one or more parameter values in an expression profile building block
#' by providing the corresponding quantity paths and values. The number of
#' paths and values must be equal.
#'
#' Note: Setting initial conditions of an expression profile is currently not supported.
#'
#' @param expressionProfileBuildingBlock A `BuildingBlock` object as returned
#'   by `createExpressionProfileBuildingBlock()`.
#' @param quantityPaths A character vector of quantity paths to set.
#' @param quantityValues A numeric vector of values to assign. Must have the
#'   same length as `quantityPaths`.
#' @param units A character vector of units corresponding to the quantity values. Must have the same length as `quantityPaths`. Units must be compatible with the dimensions of the corresponding quantities in the building block. If a single string is provided, it will be used for all quantities.
#'
#' @return `expressionProfileBuildingBlock`, invisibly.
#' @export
#'
#' @examples
#' expressionProfile <- createExpressionProfileBuildingBlock(
#'   type = ExpressionProfileCategories$`Metabolizing Enzyme`,
#'   moleculeName = "CYP3A4",
#'   speciesName = "Human"
#' )
#' setExpressionProfileParameters(
#'   expressionProfile,
#'   quantityPaths = c("Organism|Kidney|Intracellular|CYP3A4|Relative expression"),
#'   quantityValues = c(1.0),
#'   units = ""
#' )
setExpressionProfileParameters <- function(
  expressionProfileBuildingBlock,
  quantityPaths,
  quantityValues,
  units
) {
  .setParameterValuesInBBInternal(
    buildingBlock = expressionProfileBuildingBlock,
    quantityPaths = quantityPaths,
    quantityValues = quantityValues,
    units = units,
    expectedType = BuildingBlockTypes$`Expression Profile`,
    taskName = "ExpressionProfileTask",
    setMethodName = "SetExpressionParameter",
    bbLabel = "Expression Profile"
  )
}

#' Save an Expression Profile Building Block to pkml
#'
#' Exports an `Expression Profile` building block to a pkml file that can be
#' loaded by MoBi.
#'
#' @param expressionProfileBuildingBlock A `BuildingBlock` object of type
#'   `Expression Profile`, as returned by `createExpressionProfileBuildingBlock()`.
#' @param filePath Path where the pkml file will be created. Must end with the
#'   `.pkml` extension.
#'
#' @returns `filePath`, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' expressionProfile <- createExpressionProfileBuildingBlock(
#'   type = ExpressionProfileCategories$`Metabolizing Enzyme`,
#'   moleculeName = "CYP3A4",
#'   speciesName = "Human"
#' )
#' saveExpressionProfileToPKML(expressionProfile, "CYP3A4-Human.pkml")
#' }
saveExpressionProfileToPKML <- function(
  expressionProfileBuildingBlock,
  filePath
) {
  .saveBuildingBlockToPKML(
    buildingBlock = expressionProfileBuildingBlock,
    filePath = filePath,
    expectedType = BuildingBlockTypes$`Expression Profile`,
    taskName = "ExpressionProfileTask"
  )
}
