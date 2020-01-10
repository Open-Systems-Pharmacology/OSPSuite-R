#' Creates an individual using the PKSim Database.
#'
#' @param individualCharacteristics Characteristics of the individual to create as an instance of \code{OriginData}
#' @param useDistribution Boolean. Indicates wheather the function should returns all parameters defining an individual on only those parameters
#' that are actually distributed parameters
#'
#' @return An array of \code{ParameterValue} containing the value of each individual parameter
#'
#' @export
createIndividual <- function(individualCharacteristics, useDistribution = FALSE) {
  validateIsOfType(individualCharacteristics, IndividualCharacteristics)
  validateIsLogical(useDistribution)

  individualFactory <- rClr::clrCallStatic("PKSim.R.Api", "GetIndividualFactory")
  methodToCall <- if (useDistribution) "DistributionsFor" else "CreateIndividual"
  netParams <- rClr::clrCall(individualFactory, methodToCall, individualCharacteristics$ref)

  outputList <- list(
    paths = .getPropertyValues(netParams, "ParameterPath"),
    values = .getPropertyValues(netParams, "Value")
  )

  if (useDistribution) {
    distributionList <- list(
      means = .getPropertyValues(netParams, "Mean"),
      stds = .getPropertyValues(netParams, "Std"),
      distributionTypes = .getPropertyValues(.getPropertyValues(netParams, "DistributionType"), "DisplayName")
    )
    outputList <- c(outputList, distributionList)
  }

  return(outputList)
}

.getPropertyValues <- function(netObjects, propertyName) {
  sapply(netObjects, function(x) rClr::clrGet(x, name = propertyName))
}

.createSnapshotParameter <- function(value, unit) {
  if (is.null(value)) {
    return(NULL)
  }
  return(SnapshotParameter$new(value = value, unit = unit))
}

#' Creates an individual using the PKSim Database.
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
#' @param moleculeOntogenies Optional list of \code{MoleculeOntogeny} that will be used to retrieve ontogeny information for molecules.
#'
#' @return An array of \code{ParameterValue} containing the value of each individual parameter
#'
#' @export
createIndividualCharacteristics <- function(
                             species,
                             population = NULL,
                             gender = NULL,
                             weight,
                             weightUnit = "kg",
                             height = NULL,
                             heightUnit = "cm",
                             age = NULL,
                             ageUnit = "year(s)",
                             gestationalAge = 40,
                             gestationalAgeUnit = "week(s)",
                             moleculeOntogenies = NULL) {

  # Assuming that if this function is called directly, PKSim was either initialized already
  # or should be initialized automatically
  initPKSim();

  validateIsString(species)
  validateIsString(population, nullAllowed = TRUE)
  validateIsString(gender, nullAllowed = TRUE)
  validateIsNumeric(weight)
  validateIsString(weightUnit)
  validateIsNumeric(height, nullAllowed = TRUE)
  validateIsString(heightUnit)
  validateIsNumeric(age, nullAllowed = TRUE)
  validateIsString(ageUnit)
  validateIsNumeric(gestationalAge)
  validateIsString(gestationalAgeUnit)
  validateIsOfType(moleculeOntogenies, MoleculeOntogeny, nullAllowed = TRUE)

  moleculeOntogenies <- c(moleculeOntogenies)
  individualCharacteristics <- IndividualCharacteristics$new()
  individualCharacteristics$species <- species
  individualCharacteristics$population <- population
  individualCharacteristics$gender <- gender
  individualCharacteristics$age <- .createSnapshotParameter(age, ageUnit)
  individualCharacteristics$weight <- .createSnapshotParameter(weight, weightUnit)
  individualCharacteristics$height <- .createSnapshotParameter(height, heightUnit)
  individualCharacteristics$gestationalAge <- .createSnapshotParameter(gestationalAge, gestationalAgeUnit)

  for (moleculeOntogeny in moleculeOntogenies) {
    individualCharacteristics$addMoleculeOntogeny(moleculeOntogeny)
  }

  return(individualCharacteristics)
}
