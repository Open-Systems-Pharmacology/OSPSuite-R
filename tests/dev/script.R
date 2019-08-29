library(ospsuite)

sim <- loadSimulation("C:/projects/OSPSuite-R/tests/data/S1.pkml")


parameter <- getParameter("Organism|Liver|Volume", sim)
print(parameter)

results <- runSimulation(sim)

individualResults <- results[[1]]

time <- rClr::clrGet(individualResults, "Time")

allValues <- rClr::clrCall(individualResults, "ValuesAsArray")

firstOutput <- allValues[[1]]
path <- rClr::clrGet(firstOutput, "QuantityPath")
values <- rClr::clrGet(firstOutput, "Values")


saveSimulation(sim, "c:/temp/toto.xml")

