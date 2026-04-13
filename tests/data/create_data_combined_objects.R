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

oneObsDCGlobal <- DataCombined$new()
oneObsDCGlobal$addDataSets(obsData$`Vergin 1995.Iv`)

## only one simulated dataset ------------------------

oneSimDCGlobal <- DataCombined$new()
oneSimDCGlobal$addSimulationResults(
  simulationResults = simResults,
  quantitiesOrPaths = outputPathSingle,
  groups = "Aciclovir PVB"
)

## many observed datasets ------------------------

manyObsDCGlobal <- DataCombined$new()
manyObsDCGlobal$addDataSets(
  c(obsData$`Vergin 1995.Iv`, obsData$`Laskin 1982.Group C`),
  groups = "Aciclovir observed"
)

## many simulated datasets ------------------------

manySimDCGlobal <- DataCombined$new()
manySimDCGlobal$addSimulationResults(
  simulationResults = simResults,
  # Excluding the fraction excreted output
  quantitiesOrPaths = outputPaths[1:2],
  groups = "Aciclovir PVB"
)

## one observed and one simulated dataset ------------------------

oneObsSimDCGlobal <- DataCombined$new()
oneObsSimDCGlobal$addDataSets(
  obsData$`Vergin 1995.Iv`,
  groups = "Aciclovir PVB"
)
oneObsSimDCGlobal$addSimulationResults(
  simulationResults = simResults,
  quantitiesOrPaths = outputPathSingle,
  groups = "Aciclovir PVB"
)

## multiple observed and multiple simulated datasets ------------------------

manyObsSimDCGlobal <- DataCombined$new()
manyObsSimDCGlobal$addDataSets(
  c(obsData$`Vergin 1995.Iv`, obsData$`Laskin 1982.Group C`),
  groups = "Aciclovir observed"
)
manyObsSimDCGlobal$addSimulationResults(
  simulationResults = simResults,
  # Excluding the fraction excreted output
  quantitiesOrPaths = outputPaths[1:2],
  groups = "Aciclovir PVB"
)

## dataset with geometric error ------------------------

oneObsGeometricDCGlobal <- DataCombined$new()
oneObsGeometricDCGlobal$addDataSets(
  obsData$`Laskin 1982.Group C`,
  groups = "Aciclovir PVB"
)

# custom default plot configuration (DPC) -------------------------------------

customDPCGlobal <- DefaultPlotConfiguration$new()
customDPCGlobal$title <- "My Plot Title"
customDPCGlobal$subtitle <- "My Plot Subtitle"
customDPCGlobal$caption <- "My Sources"
customDPCGlobal$legendPosition <- tlf::LegendPositions$outsideRight
customDPCGlobal$yAxisScale <- "log"
customDPCGlobal$yAxisLimits <- c(0.01, 1000)

# dataset with mixed dimensions and y-Units ---------------------------

manyObsSimDCWithFractionGlobal <- DataCombined$new()

manyObsSimDCWithFractionGlobal$addSimulationResults(
  simulationResults = simResults,
  quantitiesOrPaths = outputPaths[[1]],
  names = "Aciclovir Plasma",
  groups = "Aciclovir PVB"
)
manyObsSimDCWithFractionGlobal$addSimulationResults(
  simulationResults = simResults,
  quantitiesOrPaths = outputPaths[[3]],
  names = "Aciclovir Fraction excreted",
  groups = "Aciclovir Urine"
)

manyObsSimDCWithFractionGlobal$addDataSets(
  obsData[[1]],
  groups = "Aciclovir PVB"
)
