#' Creates an individual using the PK-Sim Database
#'
#' @param individualCharacteristics Characteristics of the individual to create
#'   as an instance of `IndividualCharacteristics`
#'
#' @return A list with three entries:
#'  * `distributedParameters` containing the actual parameter values modified by the create individual algorithm.
#'  * `derivedParameters` containing the parameter values modified indirectly by the algorithm. Those parameters are typically formula parameters.
#'  * `seed` containing the seed value used to generate random values
#'
#' @note When updating a simulation with the value for a new individual, only
#'   use the `distributedParameters` to ensure that you do not override formula
#'   parameters.
#'
#' @export
createIndividual <- function(individualCharacteristics) {
  validateIsOfType(individualCharacteristics, "IndividualCharacteristics")

  individualFactory <- rSharp::clrCallStatic("PKSim.R.Api", "GetIndividualFactory")
  createIndividualResults <- rSharp::clrCall(individualFactory, "CreateIndividual", individualCharacteristics$ref)

  distributedParameters <- .getPropertyValue(createIndividualResults, "DistributedParameters")
  derivedParameters <- .getPropertyValue(createIndividualResults, "DerivedParameters")
  seed <- .getPropertyValue(createIndividualResults, "Seed")

  distributedParameters <- .parameterValueListFrom(distributedParameters, addUnits = TRUE)
  derivedParameters <- .parameterValueListFrom(derivedParameters, addUnits = TRUE)

  list(distributedParameters = distributedParameters, derivedParameters = derivedParameters, seed = seed)
}

#' Creates the parameter distributions based on the given individual `individualCharacteristics`
#'
#' @param individualCharacteristics Characteristics of the individual to create as an instance of `OriginData`
#'
#' @return An array of `ParameterValue` containing the value of each individual parameter
#'
#' @export
createDistributions <- function(individualCharacteristics) {
  validateIsOfType(individualCharacteristics, "IndividualCharacteristics")

  individualFactory <- rSharp::clrCallStatic("PKSim.R.Api", "GetIndividualFactory")
  distributedParameters <- rSharp::clrCall(individualFactory, "DistributionsFor", individualCharacteristics$ref)

  list(
    paths = .getPropertyValues(distributedParameters, "ParameterPath"),
    values = .getPropertyValues(distributedParameters, "Value"),
    units = .getPropertyValues(distributedParameters, "Unit"),
    means = .getPropertyValues(distributedParameters, "Mean"),
    stds = .getPropertyValues(distributedParameters, "Std"),
    distributionTypes = .getPropertyValues(.getPropertyValues(distributedParameters, "DistributionType"), "DisplayName")
  )
}

#' Creates an individual using the PK-Sim Database.
#'
#' @param species Species of the individual as defined in PK-Sim (see Species enum)
#' @param population Population to use to create the individual. This is required only when the species is Human. (See HumanPopulation enum)
#' @param gender Gender to use to create the individual. (See Gender enum)
#' @param weight Weight of the created individual
#' @param weightUnit Unit in which the weight value is defined. Default is kg
#' @param height Height of the created individual (for human species only)
#' @param heightUnit Unit in which the height value is defined. Default is cm
#' @param age Age of the created individual (for human species only)
#' @param ageUnit Unit in which the age value is defined. Default is year(s)
#' @param gestationalAge Gestational age of the created individual (for human species only using the Preterm population). Default is 40 Weeks
#' @param gestationalAgeUnit Unit in which the gestational age value is defined. Default is week(s)
#' @param moleculeOntogenies Optional list of `MoleculeOntogeny` that will be used to retrieve ontogeny information for molecules.
#' @param seed Optional seed parameter to use to generate start values for the created individual algorithm.
#' A `MoleculeOntogeny` is an object with the name a `molecule` property (e.g the name of the molecule as defined in your simulation)
#' and an `ontogeny` property (e.g. the name of the predefined ontogeny to use for this molecule). The list of all available ontogenies
#' can be accessed programmatically using the enum `StandardOntogeny`
#'
#' @import ospsuite.utils
#'
#' @return An array of `ParameterValue` containing the value of each individual parameter
#'
#' @export
createIndividualCharacteristics <- function(species,
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
                                            moleculeOntogenies = NULL,
                                            seed = NULL) {
  # Assuming that if this function is called directly, PK-Sim was either initialized already
  # or should be initialized automatically
  initPKSim()

  validateIsString(species)
  validateIsString(population, nullAllowed = TRUE)
  validateIsString(gender, nullAllowed = TRUE)
  validateIsNumeric(weight, nullAllowed = TRUE)
  validateIsString(weightUnit)
  validateIsNumeric(height, nullAllowed = TRUE)
  validateIsString(heightUnit)
  validateIsNumeric(age, nullAllowed = TRUE)
  validateIsString(ageUnit)
  validateIsNumeric(gestationalAge)
  validateIsString(gestationalAgeUnit)
  validateIsOfType(moleculeOntogenies, "MoleculeOntogeny", nullAllowed = TRUE)
  validateIsNumeric(seed, nullAllowed = TRUE)

  moleculeOntogenies <- c(moleculeOntogenies)
  individualCharacteristics <- IndividualCharacteristics$new()
  individualCharacteristics$species <- species
  # Check for correct population if the species is `Human`
  if (species == Species$Human && !enumHasKey(key = population, enum = HumanPopulation)) {
    stop(messages$errorWrongPopulation(species, population))
  }
  individualCharacteristics$population <- population
  individualCharacteristics$gender <- gender
  individualCharacteristics$age <- .createSnapshotParameter(age, ageUnit)
  individualCharacteristics$weight <- .createSnapshotParameter(weight, weightUnit)
  individualCharacteristics$height <- .createSnapshotParameter(height, heightUnit)
  individualCharacteristics$gestationalAge <- .createSnapshotParameter(gestationalAge, gestationalAgeUnit)
  individualCharacteristics$seed <- seed
  for (moleculeOntogeny in moleculeOntogenies) {
    individualCharacteristics$addMoleculeOntogeny(moleculeOntogeny)
  }

  return(individualCharacteristics)
}
