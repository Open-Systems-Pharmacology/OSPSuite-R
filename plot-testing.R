ospsuite.plots::setDefaults()
options(ospsuite.plots.watermark_enabled = FALSE)

####
# plotIndividualTimeProfile ------------------------

oneObsDC <- readRDS(getTestDataFilePath("oneObsDC"))
manyObsDC <- readRDS(getTestDataFilePath("manyObsDC"))

oneSimDC <- readRDS(getTestDataFilePath("oneSimDC"))
manySimDC <- readRDS(getTestDataFilePath("manySimDC"))

oneObsSimDC <- readRDS(getTestDataFilePath("oneObsSimDC"))
manyObsSimDC <- readRDS(getTestDataFilePath("manyObsSimDC"))

oneObsGeometricDC <- readRDS(getTestDataFilePath("oneObsGeometricDC"))

customDPC <- readRDS(getTestDataFilePath("customDPC"))
defaultPlotConfig <- DefaultPlotConfiguration$new()

# only observed ------------------------

# https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/1670
# https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/1701
plotIndividualTimeProfile(oneObsDC)
plotTimeProfile(oneObsDC)

plotIndividualTimeProfile(manyObsDC)
plotTimeProfile(manyObsDC)


plotIndividualTimeProfile(manyObsDC, showLegendPerDataset = TRUE)
plotTimeProfile(manyObsDC, mapping = ggplot2::aes(shape = name))

plotIndividualTimeProfile(oneSimDC)
plotTimeProfile(oneSimDC)

# https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/1702
plotIndividualTimeProfile(manySimDC)
plotTimeProfile(manySimDC)

# https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/1700
plotIndividualTimeProfile(manySimDC, showLegendPerDataset = TRUE)
plotTimeProfile(manySimDC, mapping = ggplot2::aes(shape = name))

# https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/1703
plotIndividualTimeProfile(oneObsSimDC)
plotTimeProfile(oneObsSimDC)

# https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/1707
# https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/1705
# https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/1706
# https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/1708
plotIndividualTimeProfile(oneObsSimDC, customDPC)
plotTimeProfile(
  oneObsSimDC,
  yScale = "log",
  yScaleArgs = list(limits = c(0.01, 1000))
)

# https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/1671
plotIndividualTimeProfile(manyObsSimDC)
plotTimeProfile(manyObsSimDC)

plotIndividualTimeProfile(oneObsGeometricDC)
plotTimeProfile(oneObsGeometricDC)

# https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/1709
myCombDat <- DataCombined$new()
plotIndividualTimeProfile(myCombDat)
plotTimeProfile(myCombDat)


# LLOQ

set.seed(42)
dataSet <- DataSet$new("ds with lloq")
dataSet$setValues(
  1:7,
  c(10 * exp(1:-5) + rnorm(7, 0, .25)),
  abs(rnorm(7, 0, 0.1))
)
dataSet$LLOQ <- 0.15

dc <- DataCombined$new()
dc$addDataSets(dataSet)

LLOQ_DPC <- DefaultPlotConfiguration$new()
LLOQ_DPC$lloqDirection <- "both"

plotIndividualTimeProfile(dc, defaultPlotConfiguration = LLOQ_DPC)
plotTimeProfile(dc)

#############
# plotResidualsVsTime ------------------------

# load the simulation
sim <- loadTestSimulation("MinimalModel")
simResults <- importResultsFromCSV(
  simulation = sim,
  filePaths = getTestDataFilePath("Stevens_2012_placebo_indiv_results.csv")
)

# import observed data (will return a list of DataSet objects)
dataSet <- loadDataSetsFromExcel(
  xlsFilePath = getTestDataFilePath("CompiledDataSetStevens2012.xlsx"),
  importerConfigurationOrPath = loadDataImporterConfiguration(getTestDataFilePath(
    "ImporterConfiguration.xml"
  ))
)

# create a new instance and add datasets
myCombDat <- DataCombined$new()
myCombDat$addDataSets(dataSet)
myCombDat$addSimulationResults(
  simResults,
  quantitiesOrPaths = c(
    "Organism|Lumen|Stomach|Metformin|Gastric retention",
    "Organism|Lumen|Stomach|Metformin|Gastric retention distal",
    "Organism|Lumen|Stomach|Metformin|Gastric retention proximal"
  )
)

myCombDat$setGroups(
  names = c(
    "Organism|Lumen|Stomach|Metformin|Gastric retention",
    "Organism|Lumen|Stomach|Metformin|Gastric retention distal",
    "Organism|Lumen|Stomach|Metformin|Gastric retention proximal",
    "Stevens_2012_placebo.Placebo_total",
    "Stevens_2012_placebo.Placebo_distal",
    "Stevens_2012_placebo.Placebo_proximal"
  ),
  groups = c(
    "Solid total",
    "Solid distal",
    "Solid proximal",
    "Solid total",
    "Solid distal",
    "Solid proximal"
  )
)

# https://github.com/Open-Systems-Pharmacology/OSPSuite-R/pull/1698#pullrequestreview-3763979342 the other way around
plotResidualsVsTime(myCombDat)
plotResidualsVsCovariate(
  myCombDat,
  xAxis = "time",
  residualScale = "linear"
)
# https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/1710
plotResidualsVsCovariate(
  convertUnits(myCombDat, yUnit = ""),
  xAxis = "time",
  residualScale = "linear"
)

# https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/1705
myPlotConfiguration <- DefaultPlotConfiguration$new()
myPlotConfiguration$yUnit <- ospUnits$Fraction$`%`
myPlotConfiguration$title <- "My Plot Title"
myPlotConfiguration$subtitle <- "My Plot Subtitle"
myPlotConfiguration$caption <- "My Sources"
myPlotConfiguration$legendTitle <- "My legendary title"
myPlotConfiguration$legendTitleColor <- "red"
myPlotConfiguration$legendTitleSize <- 16
myPlotConfiguration$legendKeysColor <- "brown"
myPlotConfiguration$pointsSize <- 2.5
myPlotConfiguration$legendPosition <- tlf::LegendPositions$outsideRight
myPlotConfiguration$pointsColor <- tlf::ColorMaps$default
myPlotConfiguration$linesLinetype <- names(tlf::Linetypes)

plotResidualsVsTime(
  myCombDat,
  defaultPlotConfiguration = myPlotConfiguration
)

# https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/1709
myCombDat <- DataCombined$new()
plotResidualsVsTime(myCombDat)
plotResidualsVsCovariate(myCombDat)

# edge cases ------------------------

# It returns `NULL` when `DataCombined` doesn't have any pairable datasets
dataSet1 <- DataSet$new(name = "Dataset1")
dataSet1$setValues(1, 1)
dataSet1$yDimension <- ospDimensions$`Concentration (molar)`
dataSet1$molWeight <- 1

myCombDat <- DataCombined$new()
myCombDat$addDataSets(dataSet1)


plotResidualsVsTime(myCombDat)
#https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/1711
plotResidualsVsCovariate(myCombDat)

# "Different symbols for data sets within one group"
simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
sim <- loadSimulation(simFilePath)

simData <- withr::with_tempdir({
  df <- dplyr::tibble(
    IndividualId = c(0, 0, 0),
    `Time [min]` = c(0, 2, 4),
    `Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood) [µmol/l]` = c(
      0,
      4,
      8
    )
  )
  readr::write_csv(df, "SimResults.csv")
  importResultsFromCSV(sim, "SimResults.csv")
})

obsData <- DataSet$new(name = "Observed")
obsData$setValues(
  xValues = c(1, 3, 3.5, 4, 5),
  yValues = c(1.9, 6.1, 7, 8.2, 1)
)
obsData$xUnit <- "min"
obsData$yDimension <- ospDimensions$`Concentration (molar)`

myDC <- DataCombined$new()
myDC$addSimulationResults(simData, groups = "myGroup")
myDC$addDataSets(obsData, groups = "myGroup")

# Add second obs data
obsData2 <- DataSet$new(name = "Observed 2")
obsData2$setValues(
  xValues = c(0, 3, 4, 4.5, 5.5),
  yValues = c(2.9, 5.1, 3, 8.2, 1)
)
obsData2$xUnit <- "min"
obsData2$yDimension <- ospDimensions$`Concentration (molar)`
myDC$addDataSets(obsData2, groups = "myGroup")

# https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/1712
plotResidualsVsTime(myDC)
plotResidualsVsCovariate(myDC, xAxis = "time", residualScale = "linear")


# "scaling residuals argument works with log"
myCombDat <- DataCombined$new()
myCombDat$addDataSets(dataSet)
myCombDat$addSimulationResults(
  simResults,
  quantitiesOrPaths = c(
    "Organism|Lumen|Stomach|Metformin|Gastric retention",
    "Organism|Lumen|Stomach|Metformin|Gastric retention distal",
    "Organism|Lumen|Stomach|Metformin|Gastric retention proximal"
  )
)

myCombDat$setGroups(
  names = c(
    "Organism|Lumen|Stomach|Metformin|Gastric retention",
    "Organism|Lumen|Stomach|Metformin|Gastric retention distal",
    "Organism|Lumen|Stomach|Metformin|Gastric retention proximal",
    "Stevens_2012_placebo.Placebo_total",
    "Stevens_2012_placebo.Placebo_distal",
    "Stevens_2012_placebo.Placebo_proximal"
  ),
  groups = c(
    "Solid total",
    "Solid distal",
    "Solid proximal",
    "Solid total",
    "Solid distal",
    "Solid proximal"
  )
)

# https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/1713
plotResidualsVsTime(myCombDat, scaling = "log")
plotResidualsVsCovariate(
  myCombDat,
  xAxis = "time",
  residualScale = "log"
)

#####
# plotObservedVsSimulated

# load the simulation
sim <- loadTestSimulation("MinimalModel")
simResults <- importResultsFromCSV(
  simulation = sim,
  filePaths = getTestDataFilePath("Stevens_2012_placebo_indiv_results.csv")
)

# import observed data (will return a list of `DataSet` objects)
dataSet <- loadDataSetsFromExcel(
  xlsFilePath = getTestDataFilePath("CompiledDataSetStevens2012.xlsx"),
  importerConfiguration = loadDataImporterConfiguration(getTestDataFilePath(
    "ImporterConfiguration.xml"
  ))
)

# create a new instance and add datasets
myCombDat <- DataCombined$new()
myCombDat$addDataSets(dataSet)
myCombDat$addSimulationResults(
  simResults,
  quantitiesOrPaths = c(
    "Organism|Lumen|Stomach|Metformin|Gastric retention",
    "Organism|Lumen|Stomach|Metformin|Gastric retention distal",
    "Organism|Lumen|Stomach|Metformin|Gastric retention proximal"
  )
)

myCombDat$setGroups(
  names = c(
    "Organism|Lumen|Stomach|Metformin|Gastric retention",
    "Organism|Lumen|Stomach|Metformin|Gastric retention distal",
    "Organism|Lumen|Stomach|Metformin|Gastric retention proximal",
    "Stevens_2012_placebo.Placebo_total",
    "Stevens_2012_placebo.Placebo_distal",
    "Stevens_2012_placebo.Placebo_proximal"
  ),
  groups = c(
    "Solid total",
    "Solid distal",
    "Solid proximal",
    "Solid total",
    "Solid distal",
    "Solid proximal"
  )
)


# "It throws an error when foldDistance is lower that 1"
plotObservedVsSimulated(myCombDat, foldDistance = 0.5)
# https://github.com/Open-Systems-Pharmacology/OSPSuite.Plots/issues/72
plotPredictedVsObserved(
  myCombDat,
  comparisonLineVector = ospsuite.plots::getFoldDistanceList(0.5)
)
# https://github.com/Open-Systems-Pharmacology/OSPSuite.Plots/issues/72
plotObservedVsSimulated(myCombDat, foldDistance = c(1, 0.5))
plotPredictedVsObserved(
  myCombDat,
  comparisonLineVector = ospsuite.plots::getFoldDistanceList(c(1, 0.5))
)

# "It creates default plots as expected"
plotObservedVsSimulated(myCombDat)
plotPredictedVsObserved(myCombDat)


# test_that("It creates default plots as expected with 1 fold distance",
plotObservedVsSimulated(myCombDat, foldDistance = 2)
plotPredictedVsObserved(
  myCombDat,
  comparisonLineVector = ospsuite.plots::getFoldDistanceList(
    2,
    includeIdentity = TRUE
  )
)


# test_that("It creates default plots as expected with several fold distances"
plotObservedVsSimulated(myCombDat, foldDistance = c(2, 4, 6))
plotPredictedVsObserved(
  myCombDat,
  comparisonLineVector = ospsuite.plots::getFoldDistanceList(
    c(2, 4, 6),
    includeIdentity = TRUE
  )
)

# test_that("It issues warning when scale is linear", {
myPlotConfiguration <- DefaultPlotConfiguration$new()
myPlotConfiguration$xAxisScale <- tlf::Scaling$lin
myPlotConfiguration$yAxisScale <- tlf::Scaling$lin

plotObservedVsSimulated(
  myCombDat,
  myPlotConfiguration,
  foldDistance = c(2, 4, 6)
)
plotPredictedVsObserved(
  myCombDat,
  comparisonLineVector = ospsuite.plots::getFoldDistanceList(
    c(2, 4, 6),
    includeIdentity = TRUE
  ),
  xyScale = "linear"
)

# https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/1705
# test_that("It respects custom plot configuration", {
myPlotConfiguration <- DefaultPlotConfiguration$new()
myPlotConfiguration$yUnit <- ospUnits$Fraction$`%`
myPlotConfiguration$title <- "My Plot Title"
myPlotConfiguration$subtitle <- "My Plot Subtitle"
myPlotConfiguration$caption <- "My Sources"
myPlotConfiguration$legendTitle <- "My legendary title"
myPlotConfiguration$legendTitleColor <- "red"
myPlotConfiguration$legendTitleSize <- 16
myPlotConfiguration$legendKeysColor <- "brown"
myPlotConfiguration$pointsSize <- 2.5
myPlotConfiguration$legendPosition <- tlf::LegendPositions$outsideRight
myPlotConfiguration$pointsColor <- tlf::ColorMaps$default
myPlotConfiguration$linesLinetype <- names(tlf::Linetypes)
myPlotConfiguration$foldLinesLegend <- TRUE
myPlotConfiguration$foldLinesLegendDiagonal <- TRUE

plotObservedVsSimulated(
  myCombDat,
  myPlotConfiguration,
  foldDistance = c(1.5, 2)
)

# test_that("It produces expected plot for Aciclovir data", {
simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
sim <- loadSimulation(simFilePath)

simResults <- runSimulations(sim)[[1]]

obsData <- lapply(
  c(
    "ObsDataAciclovir_1.pkml",
    "ObsDataAciclovir_2.pkml",
    "ObsDataAciclovir_3.pkml"
  ),
  function(x) {
    loadDataSetFromPKML(system.file("extdata", x, package = "ospsuite"))
  }
)

names(obsData) <- lapply(obsData, function(x) x$name)

outputPaths <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
myDataCombined <- DataCombined$new()

# Add simulated results
myDataCombined$addSimulationResults(
  simulationResults = simResults,
  quantitiesOrPaths = outputPaths,
  groups = "Aciclovir PVB"
)

# Add observed data set
myDataCombined$addDataSets(obsData$`Vergin 1995.Iv`, groups = "Aciclovir PVB")

plotObservedVsSimulated(myDataCombined, foldDistance = 2)
plotPredictedVsObserved(
  myDataCombined,
  comparisonLineVector = ospsuite.plots::getFoldDistanceList(
    2,
    includeIdentity = TRUE
  )
)
# edge cases ------------------------

# test_that("It doesn't extrapolate past maximum simulated time point", {
simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
sim <- loadSimulation(simFilePath)

simData <- withr::with_tempdir({
  df <- dplyr::tibble(
    IndividualId = c(0, 0, 0),
    `Time [min]` = c(0, 2, 4),
    `Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood) [µmol/l]` = c(
      0,
      4,
      8
    )
  )
  readr::write_csv(df, "SimResults.csv")
  importResultsFromCSV(sim, "SimResults.csv")
})

obsData <- DataSet$new(name = "Observed")
obsData$setValues(
  xValues = c(1, 3, 3.5, 4, 5),
  yValues = c(1.9, 6.1, 7, 8.2, 1)
)
obsData$xUnit <- "min"

myDC <- DataCombined$new()
myDC$addSimulationResults(simData, groups = "myGroup")
myDC$addDataSets(obsData, groups = "myGroup")

plotObservedVsSimulated(myDC, foldDistance = 2)
plotPredictedVsObserved(
  myDC,
  comparisonLineVector = ospsuite.plots::getFoldDistanceList(
    2,
    includeIdentity = TRUE
  )
)

# test_that("It returns `NULL` when `DataCombined` doesn't have any pairable datasets", {
dataSet1 <- DataSet$new(name = "Dataset1")
dataSet1$setValues(1, 1)
dataSet1$yDimension <- ospDimensions$`Concentration (molar)`
dataSet1$molWeight <- 1

myCombDat <- DataCombined$new()
myCombDat$addDataSets(dataSet1)

# https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/1711
plotObservedVsSimulated(myCombDat)
plotPredictedVsObserved(myCombDat)


# test_that("Different symbols for data sets within one group", {
simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
sim <- loadSimulation(simFilePath)

simData <- withr::with_tempdir({
  df <- dplyr::tibble(
    IndividualId = c(0, 0, 0),
    `Time [min]` = c(0, 2, 4),
    `Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood) [µmol/l]` = c(
      0,
      4,
      8
    )
  )
  readr::write_csv(df, "SimResults.csv")
  importResultsFromCSV(sim, "SimResults.csv")
})

obsData <- DataSet$new(name = "Observed")
obsData$setValues(
  xValues = c(1, 3, 3.5, 4, 5),
  yValues = c(1.9, 6.1, 7, 8.2, 1)
)
obsData$xUnit <- "min"
obsData$yDimension <- ospDimensions$`Concentration (molar)`

myDC <- DataCombined$new()
myDC$addSimulationResults(simData, groups = "myGroup")
myDC$addDataSets(obsData, groups = "myGroup")

# Add second obs data
obsData2 <- DataSet$new(name = "Observed 2")
obsData2$setValues(
  xValues = c(0, 3, 4, 4.5, 5.5),
  yValues = c(2.9, 5.1, 3, 8.2, 1)
)
obsData2$xUnit <- "min"
obsData2$yDimension <- ospDimensions$`Concentration (molar)`
myDC$addDataSets(obsData2, groups = "myGroup")

# https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/1712
plotObservedVsSimulated(myDC, foldDistance = 2)
plotPredictedVsObserved(
  myDC,
  comparisonLineVector = ospsuite.plots::getFoldDistanceList(
    2,
    includeIdentity = TRUE
  )
)

# test_that("LLOQ is plotted", {
simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
sim <- loadSimulation(simFilePath)

simData <- withr::with_tempdir({
  df <- dplyr::tibble(
    IndividualId = c(0, 0, 0),
    `Time [min]` = c(0, 2, 4),
    `Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood) [µmol/l]` = c(
      0,
      4,
      8
    )
  )
  readr::write_csv(df, "SimResults.csv")
  importResultsFromCSV(sim, "SimResults.csv")
})

obsData <- DataSet$new(name = "Observed")
obsData$setValues(
  xValues = c(1, 3, 3.5, 4, 5),
  yValues = c(1.9, 6.1, 7, 8.2, 1)
)
obsData$xUnit <- "min"
obsData$yDimension <- ospDimensions$`Concentration (molar)`
obsData$LLOQ <- 3

myDC <- DataCombined$new()
myDC$addSimulationResults(simData, groups = "myGroup")
myDC$addDataSets(obsData, groups = "myGroup")

plotObservedVsSimulated(myDC, foldDistance = 2)
plotPredictedVsObserved(
  myDC,
  comparisonLineVector = ospsuite.plots::getFoldDistanceList(
    2,
    includeIdentity = TRUE
  )
)

DPC <- DefaultPlotConfiguration$new()
DPC$lloqDirection <- "both"
plotObservedVsSimulated(
  myDC,
  foldDistance = 2,
  defaultPlotConfiguration = DPC
)
# https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/1714
plotPredictedVsObserved(
  myDC,
  comparisonLineVector = ospsuite.plots::getFoldDistanceList(
    2,
    includeIdentity = TRUE
  )
)

# https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/1715
plotPredictedVsObserved(
  myDC,
  comparisonLineVector = ospsuite.plots::getFoldDistanceList(
    2,
    includeIdentity = TRUE
  ),
  predictedAxis = "x",
)


###########
# plotPopulationTimeProfile

# only simulated ------------------------

# test_that("It respects custom plot configuration", {
# Load simulation
simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
sim <- loadSimulation(simFilePath)

populationResults <- importResultsFromCSV(
  simulation = sim,
  filePaths = system.file(
    "extdata",
    "SimResults_pop.csv",
    package = "ospsuite"
  )
)

myDataComb <- DataCombined$new()
myDataComb$addSimulationResults(populationResults)

myPlotConfiguration <- DefaultPlotConfiguration$new()
myPlotConfiguration$title <- "My Plot Title"
myPlotConfiguration$subtitle <- "My Plot Subtitle"
myPlotConfiguration$caption <- "My Sources"

plotPopulationTimeProfile(myDataComb, myPlotConfiguration)
# How to easily define legend position?
plotTimeProfile(myDataComb)

# both observed and simulated ------------------------

# test_that("It produces expected plot for both observed and simulated datasets", {
simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
sim <- loadSimulation(simFilePath)

outputPaths <- c(
  "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
  "Organism|Muscle|Intracellular|Aciclovir|Concentration"
)

simResults <- importResultsFromCSV(
  simulation = sim,
  filePaths = system.file(
    "extdata",
    "SimResults_pop.csv",
    package = "ospsuite"
  )
)

obsData <- lapply(
  c(
    "ObsDataAciclovir_1.pkml",
    "ObsDataAciclovir_2.pkml",
    "ObsDataAciclovir_3.pkml"
  ),
  function(x) {
    loadDataSetFromPKML(system.file("extdata", x, package = "ospsuite"))
  }
)
names(obsData) <- lapply(obsData, function(x) x$name)

outputPaths <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
myDataCombined <- DataCombined$new()

# Add simulated results
myDataCombined$addSimulationResults(
  simulationResults = simResults,
  quantitiesOrPaths = outputPaths,
  groups = "Aciclovir PVB"
)

# Add observed data set
myDataCombined$addDataSets(obsData$`Vergin 1995.Iv`, groups = "Aciclovir PVB")

myDataCombined$setDataTransformations(
  forNames = obsData$`Laskin 1982.Group D`$name,
  xOffsets = 2
)

myPlotConfiguration <- DefaultPlotConfiguration$new()
myPlotConfiguration$yAxisScale <- "log"
myPlotConfiguration$yAxisLimits <- c(0.01, 1000)

plotPopulationTimeProfile(myDataCombined, myPlotConfiguration)
# https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/1708
plotTimeProfile(
  myDataCombined,
  yScale = "log",
  yScaleArgs = list(limits = c(0.01, 1000))
)


# multiple datasets per group ---------------------

# test_that("It produces expected plot for multple simulated datasets per group", {
simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
sim <- loadSimulation(simFilePath)

outputPaths <- c(
  "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
  "Organism|Muscle|Intracellular|Aciclovir|Concentration"
)

simResults <- importResultsFromCSV(
  simulation = sim,
  filePaths = system.file(
    "extdata",
    "SimResults_pop.csv",
    package = "ospsuite"
  )
)

obsData <- lapply(
  c("ObsDataAciclovir_1.pkml", "ObsDataAciclovir_3.pkml"),
  function(x) {
    loadDataSetFromPKML(system.file("extdata", x, package = "ospsuite"))
  }
)

names(obsData) <- lapply(obsData, function(x) x$name)

myDataCombined <- DataCombined$new()

myDataCombined$addSimulationResults(
  simulationResults = simResults,
  quantitiesOrPaths = outputPaths,
  groups = "Aciclovir PVB"
)

plotPopulationTimeProfile(myDataCombined)
# https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/1702
plotTimeProfile(myDataCombined)

plotPopulationTimeProfile(myDataCombined, showLegendPerDataset = TRUE)
# https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/1700
plotTimeProfile(myDataCombined, mapping = ggplot2::aes(shape = name))


# test_that("It produces expected plot for multple simulated and observed datasets per group", {
simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
sim <- loadSimulation(simFilePath)

outputPaths <- c(
  "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
  "Organism|Muscle|Intracellular|Aciclovir|Concentration"
)

simResults <- importResultsFromCSV(
  simulation = sim,
  filePaths = system.file(
    "extdata",
    "SimResults_pop.csv",
    package = "ospsuite"
  )
)

obsData <- lapply(
  c("ObsDataAciclovir_1.pkml", "ObsDataAciclovir_3.pkml"),
  function(x) {
    loadDataSetFromPKML(system.file("extdata", x, package = "ospsuite"))
  }
)

names(obsData) <- lapply(obsData, function(x) x$name)

myDataCombined <- DataCombined$new()

myDataCombined$addSimulationResults(
  simulationResults = simResults,
  quantitiesOrPaths = outputPaths,
  groups = "Aciclovir PVB"
)

myDataCombined$addDataSets(obsData, groups = "Aciclovir observed")

plotPopulationTimeProfile(myDataCombined)
# https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/1702
plotTimeProfile(myDataCombined)

# edge cases ------------------------

# test_that("It returns `NULL` when `DataCombined` is empty", {
myCombDat <- DataCombined$new()
plotPopulationTimeProfile(myCombDat)
# https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/1711
plotTimeProfile(myCombDat)

# Aggregations ------------------------

#test_that("Aggregations are computed and displayed correctly", {
# Load simulation
simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
sim <- loadSimulation(simFilePath)

populationResults <- importResultsFromCSV(
  simulation = sim,
  filePaths = system.file(
    "extdata",
    "SimResults_pop.csv",
    package = "ospsuite"
  )
)

myDataComb <- DataCombined$new()
myDataComb$addSimulationResults(populationResults)

plotPopulationTimeProfile(myDataComb)
plotTimeProfile(myDataComb)

plotPopulationTimeProfile(myDataComb, quantiles = c(0.1, 0.5, 0.9))
plotTimeProfile(myDataComb, quantiles = c(0.1, 0.5, 0.9))

plotPopulationTimeProfile(myDataComb, aggregation = "arithmetic")
plotTimeProfile(myDataComb, aggregation = "arithmetic")

plotPopulationTimeProfile(
  myDataComb,
  aggregation = "arithmetic",
  nsd = 2
)
plotTimeProfile(
  myDataComb,
  aggregation = "arithmetic",
  nsd = 2
)

plotPopulationTimeProfile(myDataComb, aggregation = "geometric")
plotTimeProfile(myDataComb, aggregation = "geometric")

plotPopulationTimeProfile(
  myDataComb,
  aggregation = "geometric",
  nsd = 2
)
plotTimeProfile(
  myDataComb,
  aggregation = "geometric",
  nsd = 2
)

##########
# plotResidualsVsSimulated")

# load the simulation
sim <- loadTestSimulation("MinimalModel")
simResults <- importResultsFromCSV(
  simulation = sim,
  filePaths = getTestDataFilePath("Stevens_2012_placebo_indiv_results.csv")
)

# import observed data (will return a list of DataSet objects)
dataSet <- loadDataSetsFromExcel(
  xlsFilePath = getTestDataFilePath("CompiledDataSetStevens2012.xlsx"),
  importerConfiguration = loadDataImporterConfiguration(getTestDataFilePath(
    "ImporterConfiguration.xml"
  ))
)

# create a new instance and add datasets
myCombDat <- DataCombined$new()
myCombDat$addDataSets(dataSet)
myCombDat$addSimulationResults(
  simResults,
  quantitiesOrPaths = c(
    "Organism|Lumen|Stomach|Metformin|Gastric retention",
    "Organism|Lumen|Stomach|Metformin|Gastric retention distal",
    "Organism|Lumen|Stomach|Metformin|Gastric retention proximal"
  )
)

myCombDat$setGroups(
  names = c(
    "Organism|Lumen|Stomach|Metformin|Gastric retention",
    "Organism|Lumen|Stomach|Metformin|Gastric retention distal",
    "Organism|Lumen|Stomach|Metformin|Gastric retention proximal",
    "Stevens_2012_placebo.Placebo_total",
    "Stevens_2012_placebo.Placebo_distal",
    "Stevens_2012_placebo.Placebo_proximal"
  ),
  groups = c(
    "Solid total",
    "Solid distal",
    "Solid proximal",
    "Solid total",
    "Solid distal",
    "Solid proximal"
  )
)

# test_that("It creates default plots as expected", {

plotResidualsVsSimulated(myCombDat)
plotResidualsVsCovariate(
  myCombDat,
  xAxis = "predicted",
  residualScale = "linear"
)
# https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/1710
# https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/1713
plotResidualsVsCovariate(
  convertUnits(myCombDat, yUnit = ""),
  xAxis = "predicted",
  residualScale = "linear"
)

#test_that("It doesn't work with log scale for Y-axis", {
myPlotConfiguration <- DefaultPlotConfiguration$new()
myPlotConfiguration$yAxisScale <- tlf::Scaling$log

plotResidualsVsSimulated(
  myCombDat,
  defaultPlotConfiguration = myPlotConfiguration
)
plotResidualsVsCovariate(
  myCombDat,
  xAxis = "predicted",
  residualScale = "log"
)

# test_that("It respects custom plot configuration", {
myPlotConfiguration <- DefaultPlotConfiguration$new()
myPlotConfiguration$yUnit <- ospUnits$Fraction$`%`
myPlotConfiguration$title <- "My Plot Title"
myPlotConfiguration$subtitle <- "My Plot Subtitle"
myPlotConfiguration$caption <- "My Sources"
myPlotConfiguration$legendTitle <- "My legendary title"
myPlotConfiguration$legendTitleColor <- "red"
myPlotConfiguration$legendTitleSize <- 16
myPlotConfiguration$legendKeysColor <- "brown"
myPlotConfiguration$pointsSize <- 2.5
myPlotConfiguration$legendPosition <- tlf::LegendPositions$outsideRight
myPlotConfiguration$pointsColor <- tlf::ColorMaps$default
myPlotConfiguration$linesLinetype <- names(tlf::Linetypes)

plotResidualsVsSimulated(
  myCombDat,
  defaultPlotConfiguration = myPlotConfiguration
)
plotResidualsVsCovariate(
  myCombDat,
  xAxis = "predicted",
  residualScale = "linear"
)

# edge cases ------------------------

# test_that("It doesn't extrapolate past maximum simulated time point", {
simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
sim <- loadSimulation(simFilePath)

simData <- withr::with_tempdir({
  df <- dplyr::tibble(
    IndividualId = c(0, 0, 0),
    `Time [min]` = c(0, 2, 4),
    `Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood) [µmol/l]` = c(
      0,
      4,
      8
    )
  )
  readr::write_csv(df, "SimResults.csv")
  importResultsFromCSV(sim, "SimResults.csv")
})

obsData <- DataSet$new(name = "Observed")
obsData$setValues(
  xValues = c(1, 3, 3.5, 4, 5),
  yValues = c(1.9, 6.1, 7, 8.2, 1)
)
obsData$xUnit <- "min"

myDC <- DataCombined$new()
myDC$addSimulationResults(simData, groups = "myGroup")
myDC$addDataSets(obsData, groups = "myGroup")

plotResidualsVsSimulated(myDC)
plotResidualsVsCovariate(
  myDC,
  xAxis = "predicted",
  residualScale = "linear"
)

# test_that("It returns `NULL` when `DataCombined` is empty", {
myCombDat <- DataCombined$new()
plotResidualsVsSimulated(myCombDat)
plotResidualsVsCovariate(myCombDat)


# test_that("It returns `NULL` when `DataCombined` doesn't have any pairable datasets", {
dataSet1 <- DataSet$new(name = "Dataset1")
dataSet1$setValues(1, 1)
dataSet1$yDimension <- ospDimensions$`Concentration (molar)`
dataSet1$molWeight <- 1

myCombDat <- DataCombined$new()
myCombDat$addDataSets(dataSet1)

plotResidualsVsSimulated(myCombDat)
plotResidualsVsCovariate(myCombDat)

# test_that("Different symbols for data sets within one group", {
simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
sim <- loadSimulation(simFilePath)

simData <- withr::with_tempdir({
  df <- dplyr::tibble(
    IndividualId = c(0, 0, 0),
    `Time [min]` = c(0, 2, 4),
    `Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood) [µmol/l]` = c(
      0,
      4,
      8
    )
  )
  readr::write_csv(df, "SimResults.csv")
  importResultsFromCSV(sim, "SimResults.csv")
})

obsData <- DataSet$new(name = "Observed")
obsData$setValues(
  xValues = c(1, 3, 3.5, 4, 5),
  yValues = c(1.9, 6.1, 7, 8.2, 1)
)
obsData$xUnit <- "min"
obsData$yDimension <- ospDimensions$`Concentration (molar)`

myDC <- DataCombined$new()
myDC$addSimulationResults(simData, groups = "myGroup")
myDC$addDataSets(obsData, groups = "myGroup")

# Add second obs data
obsData2 <- DataSet$new(name = "Observed 2")
obsData2$setValues(
  xValues = c(0, 3, 4, 4.5, 5.5),
  yValues = c(2.9, 5.1, 3, 8.2, 1)
)
obsData2$xUnit <- "min"
obsData2$yDimension <- ospDimensions$`Concentration (molar)`
myDC$addDataSets(obsData2, groups = "myGroup")

plotResidualsVsSimulated(myDC)
# https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/1712
plotResidualsVsCovariate(
  myDC,
  xAxis = "predicted",
  residualScale = "linear"
)
