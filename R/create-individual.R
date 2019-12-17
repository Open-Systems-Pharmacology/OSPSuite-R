#' Creates an individual using the PKSim Database.
#'
#' @param individualCharacteristics Characteristics of the individual to create as an instance of \code{OriginData}
#' @param useDistribution Boolean. Indicates wheather the function should returns all parameters defining an individual on only those parameters
#' that are actually
#'
#' @return An array of \code{ParameterValue} containing the value of each individual parameter
#' @examples
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
