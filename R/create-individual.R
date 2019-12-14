#' Creates an individual using the PKSim Database.
#'
#' @param originData Characteristics of the individual to create as an instance of \code{OriginData}
#' @param useDistribution Boolean. Indicates wheather the function should returns all parameters defining an individual on only those parameters
#' that are actually
#'
#' @return An array of \code{ParameterValue} containing the value of each individual parameter
#' @examples
#'
#' @export
createIndividual <- function(originData, useDistribution = FALSE, moleculeOntogenies = NULL) {
  validateIsOfType(originData, OriginData)
  validateIsLogical(useDistribution)
  validateIsOfType(moleculeOntogenies, MoleculeOntogeny, nullAllowed = TRUE)
  individualFactory <- rClr::clrCallStatic("PKSim.R.Api", "GetIndividualFactory")
  moleculeOntogenies <- c(moleculeOntogenies)

  individualCharacteristics <-  rClr::clrNew("PKSim.R.Domain.IndividualCharacteristics")
  rClr::clrSet(individualCharacteristics, "OriginData", originData$ref);
  for (moleculeOntogeny in moleculeOntogenies) {
    rClr::clrCall(individualCharacteristics, "AddMoleculeOntogeny", moleculeOntogeny$ref);
  }

  methodToCall <- if (useDistribution) "DistributionsFor" else "CreateIndividual"
  netParams <- rClr::clrCall(individualFactory, methodToCall, individualCharacteristics)

  outputList <- list(
    paths = getPropertyValues(netParams, "ParameterPath"),
    values = getPropertyValues(netParams, "Value")
  )

  if (useDistribution) {
    distributionList <- list(
      means = getPropertyValues(netParams, "Mean"),
      stds = getPropertyValues(netParams, "Std"),
      distributionTypes = getPropertyValues(getPropertyValues(netParams, "DistributionType"), "DisplayName")
    )
    outputList <- c(outputList, distributionList)
  }

  return(outputList)
}

getPropertyValues <- function(netObjects, propertyName) {
  sapply(netObjects, function(x) rClr::clrGet(x, name = propertyName))
}

createOriginData <- function(
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
                             gestationalAgeUnit = "week(s)") {
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

  createParam <- function(value, unit) {
    if (is.null(value)) {
      return(NULL)
    }
    return(SnapshotParameter$new(value = value, unit = unit))
  }

  originData <- OriginData$new()
  originData$species <- species
  originData$population <- population
  originData$gender <- gender
  originData$age <- createParam(age, ageUnit)
  originData$weight <- createParam(weight, weightUnit)
  originData$height <- createParam(height, heightUnit)
  originData$gestationalAge <- createParam(gestationalAge, gestationalAgeUnit)

  return(originData)
}
