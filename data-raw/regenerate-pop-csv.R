# Rebuilds inst/extdata/pop.csv with the current population structure. Run from
# the repository root:
#
#   Rscript data-raw/regenerate-pop-csv.R
#
# The file that was committed until now came from PK-Sim 7.3 and had the old
# column set (100 columns, with RaceIndex and Population Name). This script
# creates the population in R, so the file follows whatever parameter structure
# the current libraries produce.
#
# The seed is fixed, so the file can be reproduced. Changing the seed changes
# every individual in it.
#
# See data-raw/README.md for what to check afterwards.

devtools::load_all(".", quiet = TRUE)

csvPath <- file.path("inst", "extdata", "pop.csv")

SEED <- 1234L
NUMBER_OF_INDIVIDUALS <- 10L

populationCharacteristics <- createPopulationCharacteristics(
  species = Species$Human,
  population = HumanPopulation$European_ICRP_2002,
  numberOfIndividuals = NUMBER_OF_INDIVIDUALS,
  proportionOfFemales = 50,
  seed = SEED
)

created <- createPopulation(populationCharacteristics)
exportPopulationToCSV(created$population, normalizePath(csvPath, mustWork = FALSE))
message("Wrote: ", csvPath, " (seed ", created$seed, ")")

# Report what came out. The tests in tests/testthat/test-population.R depend on
# the number of individuals, on the number of covariates, and on the gender of
# the individual with index 7.
population <- loadPopulation(csvPath)
df <- populationToDataFrame(population)
message("Individuals: ", population$count)
message("Columns: ", ncol(df))
message("Covariates: ", paste(population$allCovariateNames, collapse = ", "))
message("Gender values: ", paste(population$getCovariateValues("Gender"), collapse = ", "))
