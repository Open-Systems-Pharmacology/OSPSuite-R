# data to be used ---------------------------------------

context("plotPopulationTimeProfile")

skip_on_os("linux") # TODO enable again as soon as `createPopulation()` runs under Linux
skip_if_not_installed("vdiffr")
skip_if(getRversion() < "4.1")
skip_on_ci()

populationCharacteristics <- createPopulationCharacteristics(
  species = Species$Human,
  population = HumanPopulation$Asian_Tanaka_1996,
  numberOfIndividuals = 50,
  proportionOfFemales = 50,
  weightMin = 30,
  weightMax = 98,
  weightUnit = "kg",
  heightMin = NULL,
  heightMax = NULL,
  ageMin = 0,
  ageMax = 80,
  ageUnit = "year(s)"
)

# Create population from population characteristics
result <- createPopulation(populationCharacteristics = populationCharacteristics)
myPopulation <- result$population

# Load simulation
simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
sim <- loadSimulation(simFilePath)

populationResults <- runSimulation(
  simulation = sim,
  population = myPopulation
)

myDataComb <- DataCombined$new()
myDataComb$addSimulationResults(populationResults)

# only simulated ------------------------

test_that("It respects custom plot configuration", {
  myPlotConfiguration <- DefaultPlotConfiguration$new()
  myPlotConfiguration$yUnit <- ospUnits$Fraction$`%`
  myPlotConfiguration$title <- "My Plot Title"
  myPlotConfiguration$subtitle <- "My Plot Subtitle"
  myPlotConfiguration$caption <- "My Sources"
  myPlotConfiguration$pointsSize <- 2.5
  myPlotConfiguration$legendPosition <- tlf::LegendPositions$outsideRight
  myPlotConfiguration$pointsColor <- tlf::ColorMaps$default

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "custom plot config",
    fig = plotPopulationTimeProfile(myCombDat, myPlotConfiguration, quantiles = c(0.1, 0.5, 0.9))
  )
})

