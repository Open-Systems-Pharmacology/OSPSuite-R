#' Creates an population using the PKSim Database
#'
#' @param populationCharacteristics Characteristics of the population to create as an instance of `OriginData`
#' that are actually distributed parameters
#'
#' @return An list with three entries:
#'  * `population` An instance of a population object.
#'  * `derivedParameters` containing the parameter values modified indirectly by the algorithm. Those parameters are typically formula parameters.
#'  * `seed` containing the seed value used to generate random values
#'
#' @export
createPopulation <- function(populationCharacteristics) {
  ospsuite.utils::validateIsOfType(populationCharacteristics, PopulationCharacteristics)

  populationFactory <- rClr::clrCallStatic("PKSim.R.Api", "GetPopulationFactory")
  results <- rClr::clrCall(populationFactory, "CreatePopulation", populationCharacteristics$ref)
  netPopulation <- getPropertyValue(results, "IndividualValuesCache")
  seed <- getPropertyValue(results, "Seed")
  population <- Population$new(netPopulation)

  individualCharacteristics <- NULL

  # create an individual with similar properties Species and population. WEIGHT AND AGE DO NOT MATTER as long as we can create an individual
  individualCharacteristics <- createIndividualCharacteristics(
    species = populationCharacteristics$species,
    population = populationCharacteristics$population,
  )

  individual <- createIndividual(individualCharacteristics = individualCharacteristics)

  derivedParameters <- list()

  # Even though those parameters are derived parameters, we keep them in the population for consistency purpose with the PKSim export.
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

  # other parameters to remove that should not have been exported in the first place
  standardDistributedParametersToRemove <- c(toPathString(StandardContainer$Organism, "MeanBW"), toPathString(StandardContainer$Organism, "MeanHeight"))
  for (parameterToRemove in standardDistributedParametersToRemove) {
    if (population$has(parameterToRemove)) {
      population$remove(parameterToRemove)
    }
  }

  return(list(population = population, derivedParameters = derivedParameters, seed = seed))
}

#' Creates the population characteristics used to create a population
#'
#' @param species Species of the individual as defined in PK-Sim (see `Species` enum)
#' @param population Population to use to create the individual. This is required only when the species is Human. (See `HumanPopulation` enum)
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
#' @param BMIUnit Unit in which the BMI value is defined. Default is kg/m²
#' @param gestationalAgeMin min gestational age for the population (optional, for human species only)
#' @param gestationalAgeMax max gestational age for the population (optional, for human species only)
#' @param gestationalAgeUnit Unit in which the gestational age value is defined. Default is kg/m2
#' @param moleculeOntogenies Optional list of `MoleculeOntogeny` that will be used to retrieve ontogeny information for molecules.
#' @param seed Optional Seed parameter used to generate random values. This is only useful in order to reproduce the same population
#'
#' @return An instance of `PopulationCharacteristics` to be used in conjunction with `createPopulation`
#'
#' @export
createPopulationCharacteristics <- function(species,
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
                                            moleculeOntogenies = NULL,
                                            seed = NULL) {

  # Assuming that if this function is called directly, PKSim was either initialized already
  # or should be initialized automatically
  initPKSim()

  ospsuite.utils::validateIsString(species)
  ospsuite.utils::validateIsString(population, nullAllowed = TRUE)
  ospsuite.utils::validateIsNumeric(numberOfIndividuals)
  ospsuite.utils::validateIsNumeric(proportionOfFemales)

  ospsuite.utils::validateIsNumeric(weightMin, nullAllowed = TRUE)
  ospsuite.utils::validateIsNumeric(weightMax, nullAllowed = TRUE)
  ospsuite.utils::validateIsString(weightUnit)

  ospsuite.utils::validateIsNumeric(heightMin, nullAllowed = TRUE)
  ospsuite.utils::validateIsNumeric(heightMax, nullAllowed = TRUE)
  ospsuite.utils::validateIsString(heightUnit)

  ospsuite.utils::validateIsNumeric(ageMin, nullAllowed = TRUE)
  ospsuite.utils::validateIsNumeric(ageMax, nullAllowed = TRUE)
  ospsuite.utils::validateIsString(ageUnit)

  ospsuite.utils::validateIsNumeric(BMIMin, nullAllowed = TRUE)
  ospsuite.utils::validateIsNumeric(BMIMax, nullAllowed = TRUE)
  ospsuite.utils::validateIsString(BMIUnit)

  ospsuite.utils::validateIsInteger(gestationalAgeMin, nullAllowed = TRUE)
  ospsuite.utils::validateIsInteger(gestationalAgeMax, nullAllowed = TRUE)
  ospsuite.utils::validateIsString(gestationalAgeUnit)

  ospsuite.utils::validateIsOfType(moleculeOntogenies, MoleculeOntogeny, nullAllowed = TRUE)
  ospsuite.utils::validateIsInteger(seed, nullAllowed = TRUE)

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
  populationCharacteristics$seed <- seed

  for (moleculeOntogeny in moleculeOntogenies) {
    populationCharacteristics$addMoleculeOntogeny(moleculeOntogeny)
  }

  return(populationCharacteristics)
}
