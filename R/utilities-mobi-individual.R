#' @title Create a MoBi Individual Building Block
#'
#' @description
#' Creates an individual building block in MoBi for a given species and
#' optional demographic characteristics by calling the MoBi `IndividualTask`.
#'
#' @param species Species of the individual as defined in PK-Sim (see `Species` enum).
#' @param population Population to use to create the individual. Required when
#'   species is Human (see `HumanPopulation` enum).
#' @param gender Gender of the individual (see `Gender` enum).
#' @param weight Weight of the individual.
#' @param weightUnit Unit of the weight value. Default is `"kg"`.
#' @param height Height of the individual (human species only).
#' @param heightUnit Unit of the height value. Default is `"cm"`.
#' @param age Age of the individual (human species only).
#' @param ageUnit Unit of the age value. Default is `"year(s)"`.
#' @param gestationalAge Gestational age of the individual (for preterm human population).
#'   Default is 40.
#' @param gestationalAgeUnit Unit of the gestational age value. Default is `"week(s)"`.
#' @param seed Optional seed for the individual creation algorithm.
#' @param name Name for the building block.
#'
#' @return A `BuildingBlock` object representing the created individual.
#' @export
#'
#' @examples
#' \dontrun{
#' individual <- createMoBiIndividualBuildingBlock(
#'   name = "MyIndividual",
#'   species = Species$Human,
#'   population = HumanPopulation$European_ICRP_2002,
#'   gender = Gender$Male,
#'   weight = 73,
#'   age = 30
#' )
#' }
createMoBiIndividualBuildingBlock <- function(
  name,
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

#' @title Set Parameters of a MoBi Individual Building Block
#'
#' @description
#' Sets one or more parameter values in an individual building block by
#' providing the corresponding quantity paths and values. The number of
#' paths and values must be equal.
#'
#' @param individualBuildingBlock A `BuildingBlock` object as returned
#'   by `createMoBiIndividualBuildingBlock()`.
#' @param quantityPaths A character vector of quantity paths to set.
#' @param quantityValues A numeric vector of values to assign. Must have the
#'   same length as `quantityPaths`.
#'
#' @return `individualBuildingBlock`, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' individual <- createMoBiIndividualBuildingBlock(
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
