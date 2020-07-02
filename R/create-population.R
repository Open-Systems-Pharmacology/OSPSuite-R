#' Creates an population using the PKSim Database
#'
#' @param populationCharacteristics Characteristics of the population to create as an instance of \code{OriginData}
#' that are actually distributed parameters
#'
#' @return An list with two entries:
#' The \code{population} An instance of a population object.
#' The \code{derivedParameters} containing the parameter values modified indirectly by the algorithm. Those parameters are typically formula parameters.
#'
#' @export
createPopulation <- function(populationCharacteristics) {
  validateIsOfType(populationCharacteristics, PopulationCharacteristics)

  populationFactory <- rClr::clrCallStatic("PKSim.R.Api", "GetPopulationFactory")
  netPopulation <- rClr::clrCall(populationFactory, "CreatePopulation", populationCharacteristics$ref)
  population <- Population$new(netPopulation)

  individualCharacteristics <- NULL

  # NOTE THIS IS A WORKAROUND UNTIL THE CODE IN PKSIM IS UPDATED
  if (populationCharacteristics$species == Species$Human) {
    # create an individual with similar properites Species and population. WEIGHT AND AGE DO NOT MATTER as long as we can create an invidiual
    individualCharacteristics <- ospsuite::createIndividualCharacteristics(
      species = populationCharacteristics$species,
      population = populationCharacteristics$population,
      age = 30
    )
  }
  else {
    # create an individual with similar properites Species and population. WEIGHT AND AGE DO NOT MATTER as long as we can create an invidiual
    individualCharacteristics <- ospsuite::createIndividualCharacteristics(
      species = populationCharacteristics$species,
      population = populationCharacteristics$population,
    )
  }

  individual <- createIndividual(individualCharacteristics = individualCharacteristics)

  derivedParameters <- list()

  # Even though those parameters are derived parmaeters, we keep them in the population for consistency purpose with the PKSim export.
  standardDerivedParametersToKeep <- c(StandardPath$Weight, StandardPath$BMI, StandardPath$BSA)

  for (derivedParameterPath in individual$derivedParameters$paths) {
    if (derivedParameterPath %in% c(StandardPath$Weight, StandardPath$BMI, StandardPath$BSA)) {
      next
    }

    if (population$has(derivedParameterPath)) {
      derivedParameters[[derivedParameterPath]] <- population$getParameterValues(derivedParameterPath)
      population$remove(derivedParameterPath)
    }
  }

  # other parameters to remvove that should not have been exported in the first place
  standardDistributedParametersToRemove <- c(toPathString(StandardContainer$Organism, "MeanBW"), toPathString(StandardContainer$Organism, "MeanHeight"))
  for (parameterToRemove in standardDistributedParametersToRemove) {
    if (population$has(parameterToRemove)) {
      population$remove(parameterToRemove)
    }
  }

  return(list(population = population, derivedParameters = derivedParameters))
}

#' Creates the population characteristics used to create a population
#'
#' @param species Species of the individual as defined in PK-Sim (see Species enum)
#' @param population Population to use to create the individual. This is required only when the species is Human. (See HumanPopulation enum)
#' @param numberOfIndividuals Number of individuals in the population
#' @param proportionOfFemales Proportions of females. Default is 50 (50%)
#' @param weightMin min weight for the population (optional)
#' @param weightMax max weight for the population (optional)
#' @param weightUnit Unit in which the weight value is defined. Default is kg
#' @param heightMin min height for the population (optional, for human species only)
#' @param heightMax max height for the population (optional, for human species only)
#' @param heightUnit Unit in which the height value is defined. Default is cm
#' @param ageMin min age for the population (optional, for human species only)
#' @param ageMax max age for the population (optional, for human species only)
#' @param ageUnit Unit in which the age value is defined. Default is year(s)
#' @param BMIMin min BMI for the population (optional, for human species only)
#' @param BMIMax max BMI for the population (optional, for human species only)
#' @param BMIUnit Unit in which the BMI value is defined. Default is year(s)
#' @param gestationalAgeMin min gestational age for the population (optional, for human species only)
#' @param gestationalAgeMax max gestational age for the population (optional, for human species only)
#' @param gestationalAgeUnit Unit in which the gestational age value is defined. Default is kg/m2
#' @param moleculeOntogenies Optional list of \code{MoleculeOntogeny} that will be used to retrieve ontogeny information for molecules.
#'
#' @return An instance of \code{PopulationCharacteristics} to be used in conjunction with \code{createPopulation}
#'
#' @export
createPopulationCharacteristics <- function(
                                            species,
                                            population = NULL,
                                            numberOfIndividuals,
                                            proportionOfFemales = 50,
                                            weightMin = NULL,
                                            weightMax = NULL,
                                            weightUnit = "kg",
                                            heightMin = NULL,
                                            heightMax = NULL,
                                            heightUnit = "cm",
                                            ageMin = NULL,
                                            ageMax = NULL,
                                            ageUnit = "year(s)",
                                            BMIMin = NULL,
                                            BMIMax = NULL,
                                            BMIUnit = "kg/m²",
                                            gestationalAgeMin = NULL,
                                            gestationalAgeMax = NULL,
                                            gestationalAgeUnit = "week(s)",

                                            moleculeOntogenies = NULL) {

  # Assuming that if this function is called directly, PKSim was either initialized already
  # or should be initialized automatically
  initPKSim()

  validateIsString(species)
  validateIsString(population, nullAllowed = TRUE)
  validateIsNumeric(numberOfIndividuals)
  validateIsNumeric(proportionOfFemales)

  validateIsNumeric(weightMin, nullAllowed = TRUE)
  validateIsNumeric(weightMax, nullAllowed = TRUE)
  validateIsString(weightUnit)

  validateIsNumeric(heightMin, nullAllowed = TRUE)
  validateIsNumeric(heightMax, nullAllowed = TRUE)
  validateIsString(heightUnit)

  validateIsNumeric(ageMin, nullAllowed = TRUE)
  validateIsNumeric(ageMax, nullAllowed = TRUE)
  validateIsString(ageUnit)

  validateIsNumeric(BMIMin, nullAllowed = TRUE)
  validateIsNumeric(BMIMax, nullAllowed = TRUE)
  validateIsString(BMIUnit)

  validateIsInteger(gestationalAgeMin, nullAllowed = TRUE)
  validateIsInteger(gestationalAgeMax, nullAllowed = TRUE)
  validateIsString(gestationalAgeUnit)

  validateIsOfType(moleculeOntogenies, MoleculeOntogeny, nullAllowed = TRUE)

  moleculeOntogenies <- c(moleculeOntogenies)
  populationCharacteristics <- PopulationCharacteristics$new()
  populationCharacteristics$species <- species
  populationCharacteristics$population <- population
  populationCharacteristics$numberOfIndividuals <- numberOfIndividuals
  populationCharacteristics$proportionOfFemales <- proportionOfFemales
  populationCharacteristics$age <- createParameterRange(ageMin, ageMax, ageUnit)
  populationCharacteristics$weight <- createParameterRange(weightMin, weightMax, weightUnit)
  populationCharacteristics$height <- createParameterRange(heightMin, heightMax, heightUnit)
  populationCharacteristics$gestationalAge <- createParameterRange(gestationalAgeMin, gestationalAgeMax, gestationalAgeUnit)
  populationCharacteristics$BMI <- createParameterRange(BMIMin, BMIMax, BMIUnit)

  for (moleculeOntogeny in moleculeOntogenies) {
    populationCharacteristics$addMoleculeOntogeny(moleculeOntogeny)
  }

  return(populationCharacteristics)
}
