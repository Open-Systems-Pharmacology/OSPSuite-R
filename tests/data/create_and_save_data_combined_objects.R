# path relative to the project directory where data should be saved
path <- testthat::test_path("../data")

# simulated results ------------------------

simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
sim <- loadSimulation(simFilePath, loadFromCache = FALSE, addToCache = FALSE)
outputPathSingle <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
outputPaths <- c(
  "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
  "Organism|Muscle|Intracellular|Aciclovir|Concentration",
  "Organism|Kidney|Urine|Aciclovir|Fraction excreted to urine"
)
setOutputs(outputPaths, sim)
simResults <- runSimulations(sim)[[1]]

# observed data ------------------------

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

# `DataCombined` (DC) objects ----------------------------

## only one observed dataset ------------------------

oneObsDC <- DataCombined$new()
oneObsDC$addDataSets(obsData$`Vergin 1995.Iv`)
saveRDS(oneObsDC, file = file.path(path, "oneObsDC.rds"))

## only one simulated dataset ------------------------

oneSimDC <- DataCombined$new()
oneSimDC$addSimulationResults(
  simulationResults = simResults,
  quantitiesOrPaths = outputPathSingle,
  groups = "Aciclovir PVB"
)
saveRDS(oneSimDC, file = file.path(path, "oneSimDC.rds"))

## many observed datasets ------------------------

manyObsDC <- DataCombined$new()
manyObsDC$addDataSets(
  c(obsData$`Vergin 1995.Iv`, obsData$`Laskin 1982.Group C`),
  groups = "Aciclovir observed"
)
saveRDS(manyObsDC, file = file.path(path, "manyObsDC.rds"))

## many simulated datasets ------------------------

manySimDC <- DataCombined$new()
manySimDC$addSimulationResults(
  simulationResults = simResults,
  # Excluding the fraction excreted output
  quantitiesOrPaths = outputPaths[1:2],
  groups = "Aciclovir PVB"
)
saveRDS(manySimDC, file = file.path(path, "manySimDC.rds"))

## one observed and one simulated dataset ------------------------

oneObsSimDC <- DataCombined$new()
oneObsSimDC$addDataSets(obsData$`Vergin 1995.Iv`, groups = "Aciclovir PVB")
oneObsSimDC$addSimulationResults(
  simulationResults = simResults,
  quantitiesOrPaths = outputPathSingle,
  groups = "Aciclovir PVB"
)
saveRDS(oneObsSimDC, file = file.path(path, "oneObsSimDC.rds"))

## multiple observed and multiple simulated datasets ------------------------

manyObsSimDC <- DataCombined$new()
manyObsSimDC$addDataSets(
  c(obsData$`Vergin 1995.Iv`, obsData$`Laskin 1982.Group C`),
  groups = "Aciclovir observed"
)
manyObsSimDC$addSimulationResults(
  simulationResults = simResults,
  # Excluding the fraction excreted output
  quantitiesOrPaths = outputPaths[1:2],
  groups = "Aciclovir PVB"
)
saveRDS(manyObsSimDC, file = file.path(path, "manyObsSimDC.rds"))

## dataset with geometric error ------------------------

oneObsGeometricDC <- DataCombined$new()
oneObsGeometricDC$addDataSets(
  obsData$`Laskin 1982.Group C`,
  groups = "Aciclovir PVB"
)
saveRDS(oneObsGeometricDC, file = file.path(path, "oneObsGeometricDC.rds"))

# custom default plot configuration (DPC) -------------------------------------

customDPC <- DefaultPlotConfiguration$new()
customDPC$title <- "My Plot Title"
customDPC$subtitle <- "My Plot Subtitle"
customDPC$caption <- "My Sources"
customDPC$legendPosition <- tlf::LegendPositions$outsideRight
customDPC$yAxisScale <- "log"
customDPC$yAxisLimits <- c(0.01, 1000)
saveRDS(customDPC, file = file.path(path, "customDPC.rds"))

# dataset with mixed dimensions and y-Units ---------------------------

manyObsSimDCWithFraction <- DataCombined$new()

manyObsSimDCWithFraction$addSimulationResults(
  simulationResults = simResults,
  quantitiesOrPaths = outputPaths[[1]],
  names = "Aciclovir Plasma",
  groups = "Aciclovir PVB"
)
manyObsSimDCWithFraction$addSimulationResults(
  simulationResults = simResults,
  quantitiesOrPaths = outputPaths[[3]],
  names = "Aciclovir Fraction excreted",
  groups = "Aciclovir Urine"
)

manyObsSimDCWithFraction$addDataSets(obsData[[1]], groups = "Aciclovir PVB")

saveRDS(
  manyObsSimDCWithFraction,
  file = file.path(path, "manyObsSimDCWithFraction.rds")
)
