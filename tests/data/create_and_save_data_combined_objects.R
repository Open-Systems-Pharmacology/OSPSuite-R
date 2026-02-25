devtools::load_all()

# path relative to the project directory where data should be saved
path <- testthat::test_path("../data")

# simulated results ------------------------

simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
sim <- loadSimulation(simFilePath)
simResultsOne <- runSimulations(sim)[[1]]


simResultsMany <- runSimulations(sim)[[1]][[1]]

outputPath <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
outputPaths <- c(
  "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
  "Organism|Muscle|Intracellular|Aciclovir|Concentration"
)
addOutputs(outputPaths, sim)

# observed data ------------------------

obsDataOne <- lapply(
  c(
    "ObsDataAciclovir_1.pkml",
    "ObsDataAciclovir_2.pkml",
    "ObsDataAciclovir_3.pkml"
  ),
  function(x) {
    loadDataSetFromPKML(system.file("extdata", x, package = "ospsuite"))
  }
)
names(obsDataOne) <- lapply(obsDataOne, function(x) x$name)

obsDataMany <- lapply(
  c("ObsDataAciclovir_1.pkml", "ObsDataAciclovir_3.pkml"),
  function(x) {
    loadDataSetFromPKML(system.file("extdata", x, package = "ospsuite"))
  }
)
names(obsDataMany) <- lapply(obsDataMany, function(x) x$name)

# `DataCombined` (DC) objects ----------------------------

## only one observed dataset ------------------------

oneObsDC <- DataCombined$new()
oneObsDC$addDataSets(obsDataOne$`Vergin 1995.Iv`)
saveRDS(oneObsDC, file = file.path(path, "oneObsDC"))

## only one simulated dataset ------------------------

oneSimDC <- DataCombined$new()
oneSimDC$addSimulationResults(
  simulationResults = simResultsOne,
  quantitiesOrPaths = outputPath,
  groups = "Aciclovir PVB"
)
saveRDS(oneSimDC, file = file.path(path, "oneSimDC"))

## many observed datasets ------------------------

manyObsDC <- DataCombined$new()
manyObsDC$addDataSets(obsDataMany, groups = "Aciclovir observed")
saveRDS(manyObsDC, file = file.path(path, "manyObsDC"))

## many simulated datasets ------------------------

manySimDC <- DataCombined$new()
manySimDC$addSimulationResults(
  simulationResults = simResultsMany,
  quantitiesOrPaths = outputPaths,
  groups = "Aciclovir PVB"
)
saveRDS(manySimDC, file = file.path(path, "manySimDC"))

## one observed and one simulated dataset ------------------------

oneObsSimDC <- DataCombined$new()
oneObsSimDC$addDataSets(obsDataOne$`Vergin 1995.Iv`, groups = "Aciclovir PVB")
oneObsSimDC$addSimulationResults(
  simulationResults = simResultsOne,
  quantitiesOrPaths = outputPath,
  groups = "Aciclovir PVB"
)
saveRDS(oneObsSimDC, file = file.path(path, "oneObsSimDC"))

## multiple observed and multiple simulated datasets ------------------------

manyObsSimDC <- DataCombined$new()
manyObsSimDC$addDataSets(obsDataMany, groups = "Aciclovir observed")
manyObsSimDC$addSimulationResults(
  simulationResults = simResultsMany,
  quantitiesOrPaths = outputPaths,
  groups = "Aciclovir PVB"
)
saveRDS(manyObsSimDC, file = file.path(path, "manyObsSimDC"))

## dataset with geometric error ------------------------

ObsDataAciclovir_3 <- loadDataSetFromPKML(system.file(
  "extdata",
  "ObsDataAciclovir_3.pkml",
  package = "ospsuite"
))

oneObsGeometricDC <- DataCombined$new()
oneObsGeometricDC$addDataSets(ObsDataAciclovir_3, groups = "Aciclovir PVB")
saveRDS(oneObsGeometricDC, file = file.path(path, "oneObsGeometricDC"))

# custom default plot configuration (DPC) -------------------------------------

customDPC <- DefaultPlotConfiguration$new()
customDPC$title <- "My Plot Title"
customDPC$subtitle <- "My Plot Subtitle"
customDPC$caption <- "My Sources"
customDPC$legendPosition <- tlf::LegendPositions$outsideRight
customDPC$yAxisScale <- "log"
customDPC$yAxisLimits <- c(0.01, 1000)
saveRDS(customDPC, file = file.path(path, "customDPC"))

# dataset with mixed dimensions and y-Units ---------------------------

simFilePath <- system.file("extdata", "Aciclovir.pkml", package = "ospsuite")
sim <- loadSimulation(simFilePath)
outputPath <-  c("Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)",
                 "Organism|Kidney|Urine|Aciclovir|Fraction excreted to urine")
setOutputs(quantitiesOrPaths =  outputPath,
           simulation = sim)
simResults <- runSimulations(sim)[[1]]

manyObsSimDCWithFraction <- DataCombined$new()

manyObsSimDCWithFraction$addSimulationResults(
  simulationResults = simResults,
  quantitiesOrPaths = outputPath[1],
  names = "Aciclovir Plasma",
  groups = "Aciclovir PVB"
)
manyObsSimDCWithFraction$addSimulationResults(
  simulationResults = simResults,
  quantitiesOrPaths = outputPath[2],
  names = "Aciclovir Fraction excreted",
  groups = "Aciclovir Urine"
)

# observed data
obsData <-  loadDataSetFromPKML(system.file("extdata", "ObsDataAciclovir_1.pkml", package = "ospsuite"))

manyObsSimDCWithFraction$addDataSets(obsData, groups = "Aciclovir PVB")

saveRDS(manyObsSimDCWithFraction, file = file.path(path, "manyObsSimDCWithFraction"))
