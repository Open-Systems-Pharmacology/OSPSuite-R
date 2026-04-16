#' Create a MoBi Individual Building Block
#'
#' Creates an individual building block in MoBi for a given species and
#' optional demographic characteristics. Currently, disease states are not supported.
#'
#' @param name Optional name for the individual building block. If not provided, species will be used as name.
#'
#' @inheritParams createIndividualCharacteristics
#'
#' @returns An object of type `BuildingBlock` representing the created individual.
#' @export
#'
#' @examples
#' individual <- createIndividualBuildingBlock(
#'   name = "Standard Individual",
#'   species = Species$Human,
#'   population = HumanPopulation$European_ICRP_2002,
#'   gender = Gender$Male,
#'   weight = 73,
#'   age = 30
#' )
createIndividualBuildingBlock <- function(
  name = NULL,
  species,
  population = NULL,
  gender = NULL,
  weight = NULL,
  weightUnit = "kg",
  height = NULL,
  heightUnit = "cm",
  age = NULL,
  ageUnit = "year(s)",
  gestationalAge = 40,
  gestationalAgeUnit = "week(s)",
  seed = NULL
) {
  validateIsCharacter(name, nullAllowed = TRUE)
  if (is.null(name)) {
    name <- species
  }
  individualCharacteristics <- createIndividualCharacteristics(
    species = species,
    population = population,
    gender = gender,
    weight = weight,
    weightUnit = weightUnit,
    height = height,
    heightUnit = heightUnit,
    age = age,
    ageUnit = ageUnit,
    gestationalAge = gestationalAge,
    gestationalAgeUnit = gestationalAgeUnit,
    seed = seed
  )

  netTask <- .getMoBiTaskFromCache("IndividualTask")
  netObject <- netTask$call(
    "CreateIndividual",
    individualCharacteristics,
    name
  )

  return(BuildingBlock$new(netObject, type = BuildingBlockTypes$Individual))
}

#' Convert an Individual Building Block to a data frame.
#'
#' @param individualBuildingBlock A `BuildingBlock` object of type `Individual`.
#'
#' @inherit parameterValuesBBToDataFrame return
#'
#' @export
#'
#' @examples
#' individual <- createIndividualBuildingBlock(
#'   species = Species$Human,
#'   population = HumanPopulation$European_ICRP_2002
#' )
#' df <- individualsBBToDataFrame(individual)
individualsBBToDataFrame <- function(individualBuildingBlock) {
  .validateBuildingBlockType(
    individualBuildingBlock,
    BuildingBlockTypes$Individual
  )
  .bbWithParameterValuesToDataFrame(individualBuildingBlock)
}


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
#'   quantityPaths = c("Organism|Age", "Organism|Height"),
#'   quantityValues = c(30, 173),
#'   units = c("year(s)", "cm")
#' )
setParameterValuesInIndividualBB <- function(
  individualBuildingBlock,
  quantityPaths,
  quantityValues,
  units
) {
  .setParameterValuesInBBInternal(
    buildingBlock = individualBuildingBlock,
    quantityPaths = quantityPaths,
    quantityValues = quantityValues,
    units = units,
    expectedType = BuildingBlockTypes$Individual,
    taskName = "IndividualTask",
    setMethodName = "SetIndividualParameter",
    bbLabel = "Building Block"
  )
}
