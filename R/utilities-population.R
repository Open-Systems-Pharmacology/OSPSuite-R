#' Loads a population from a csv file and returns the population.
#'
#' @param csvPopulationFile Full path of csv population file to load.
#'
#' @examples
#' csvPath <- system.file("extdata", "pop.csv", package = "ospsuite")
#'
#' population <- loadPopulation(csvPath)
#' @export
loadPopulation <- function(csvPopulationFile) {
  validateIsString(csvPopulationFile)
  csvPopulationFile <- .expandPath(csvPopulationFile)
  populationTask <- .getNetTask("PopulationTask")
  population <- populationTask$call("ImportPopulation", csvPopulationFile)
  Population$new(population)
}

#' Loads a population from the `csvPopulationFile` and split the loaded population according to
#' `numberOfCores`.
#' @param csvPopulationFile Full path of csv population file to split.
#' @param numberOfCores Number of cores used for parallelization computing. The population will be split across all cores.
#' @param outputFolder Folder where all split files will be created
#' @param outputFileName File names will be constructed using this parameter concatenated with the core index.
#' @return A string vector containing the full path of the population files created. Note that there might be less files than cores
#'
#' @examples
#' csvPath <- system.file("extdata", "pop.csv", package = "ospsuite")
#'
#' # Split the population in up to 3 files, saved in the temp folder
#' splitFiles <- splitPopulationFile(csvPath, 3, tempdir(), "PopFile")
#' @export
splitPopulationFile <- function(csvPopulationFile, numberOfCores, outputFolder, outputFileName) {
  validateIsString(csvPopulationFile)
  validateIsNumeric(numberOfCores)
  validateIsString(outputFolder)
  validateIsString(outputFileName)
  csvPopulationFile <- .expandPath(csvPopulationFile)
  populationTask <- .getNetTask("PopulationTask")
  populationTask$call("SplitPopulation", csvPopulationFile, as.integer(numberOfCores), outputFolder, outputFileName)
}


#' @title Creates a data.frame containing one column for each parameter defined in the population
#'
#' @param population Population to convert to data frame (typically imported from file using `loadPopulation`)
#'
#' @examples
#' csvPath <- system.file("extdata", "pop.csv", package = "ospsuite")
#'
#' population <- loadPopulation(csvPath)
#' df <- populationToDataFrame(population)
#' @export
populationToDataFrame <- function(population) {
  validateIsOfType(population, "Population")
  columns <- list()
  columns$IndividualId <- population$allIndividualIds

  for (covariateName in population$allCovariateNames) {
    columns[[covariateName]] <- population$getCovariateValues(covariateName)
  }

  for (parameterPath in population$allParameterPaths) {
    columns[[parameterPath]] <- population$getParameterValues(parameterPath)
  }

  popData <- data.frame(columns, stringsAsFactors = FALSE, check.names = FALSE)

  # consistently return a (classical) data frame
  return(popData)
}

#' @rdname populationToDataFrame
#'
#' @export
populationToTibble <- function(population) {
  popData <- populationToDataFrame(population)

  # consistently return a tibble data frame
  return(dplyr::as_tibble(popData))
}

#' Saves the population to csv file
#'
#' @param population Population to export to csv (typically imported from file using `loadPopulation`)
#' @param filePath Full path where the population will be saved.
#'
#' @examples
#' csvPath <- system.file("extdata", "pop.csv", package = "ospsuite")
#'
#' # Load the population
#' population <- loadPopulation(csvPath)
#'
#' # Exports the population
#' exportPopulationToCSV(population, tempfile())
#' @export
exportPopulationToCSV <- function(population, filePath) {
  validateIsOfType(population, "Population")
  validateIsString(filePath)
  filePath <- .expandPath(filePath)
  df <- populationToDataFrame(population)
  utils::write.csv(df, file = filePath, row.names = FALSE, fileEncoding = "UTF-8")
  invisible()
}

#' @inherit exportPopulationToCSV
.savePopulationToCSV <- function(population, filePath) {
  exportPopulationToCSV(population, filePath)
}

#' Loads aging data (typically generated from PK-Sim) i
#'
#' @param filePath Full path containing an aging data table.
#'
#' @examples
#' csvPath <- system.file("extdata", "aging_data.csv", package = "ospsuite")
#'
#' agingData <- loadAgingDataFromCSV(csvPath)
#' @export
loadAgingDataFromCSV <- function(filePath) {
  validateIsString(filePath)
  df <- readr::read_csv(filePath, locale = readr::locale(encoding = "UTF-8"), comment = "#", col_types = readr::cols())
  agingData <- AgingData$new()
  agingData$individualIds <- as.integer(df$IndividualId)
  agingData$parameterPaths <- df$ParameterPath
  agingData$times <- df$Time
  agingData$values <- df$Value
  return(agingData)
}


#' Creates an population using the PK-Sim Database
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
  validateIsOfType(populationCharacteristics, "PopulationCharacteristics")

  populationFactory <- rSharp::callStatic("PKSim.R.Api", "GetPopulationFactory")
  results <- populationFactory$call("CreatePopulation", populationCharacteristics)
  netPopulation <- .getPropertyValue(results, "IndividualValuesCache")
  seed <- .getPropertyValue(results, "Seed")
  population <- Population$new(netPopulation)

  individualCharacteristics <- NULL

  # create an individual with similar properties Species and population. WEIGHT AND AGE DO NOT MATTER as long as we can create an individual
  individualCharacteristics <- createIndividualCharacteristics(
    species = populationCharacteristics$species,
    population = populationCharacteristics$population,
  )

  individual <- createIndividual(individualCharacteristics = individualCharacteristics)

  derivedParameters <- list()

  # Even though those parameters are derived parameters, we keep them in the population for consistency purpose with the PK-Sim export.
  standardDerivedParametersToKeep <- c(StandardPath$Weight, StandardPath$BMI, StandardPath$BSA)

  for (derivedParameterPath in individual$derivedParameters$paths) {
    if (any(c(StandardPath$Weight, StandardPath$BMI, StandardPath$BSA) == derivedParameterPath)) {
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
#' @param BMIUnit Unit in which the BMI value is defined. Default is kg/m2
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
                                            BMIUnit = "kg/m\U00B2",
                                            gestationalAgeMin = NULL,
                                            gestationalAgeMax = NULL,
                                            gestationalAgeUnit = "week(s)",
                                            moleculeOntogenies = NULL,
                                            seed = NULL) {
  # Assuming that if this function is called directly, PK-Sim was either initialized already
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

  validateIsOfType(moleculeOntogenies, "MoleculeOntogeny", nullAllowed = TRUE)
  validateIsInteger(seed, nullAllowed = TRUE)

  moleculeOntogenies <- c(moleculeOntogenies)
  populationCharacteristics <- PopulationCharacteristics$new()
  populationCharacteristics$species <- species
  populationCharacteristics$population <- population
  populationCharacteristics$numberOfIndividuals <- numberOfIndividuals
  populationCharacteristics$proportionOfFemales <- proportionOfFemales
  populationCharacteristics$age <- .createParameterRange(ageMin, ageMax, ageUnit)
  populationCharacteristics$weight <- .createParameterRange(weightMin, weightMax, weightUnit)
  populationCharacteristics$height <- .createParameterRange(heightMin, heightMax, heightUnit)
  populationCharacteristics$gestationalAge <- .createParameterRange(gestationalAgeMin, gestationalAgeMax, gestationalAgeUnit)
  populationCharacteristics$BMI <- .createParameterRange(BMIMin, BMIMax, BMIUnit)
  populationCharacteristics$seed <- seed

  for (moleculeOntogeny in moleculeOntogenies) {
    populationCharacteristics$addMoleculeOntogeny(moleculeOntogeny)
  }

  return(populationCharacteristics)
}
