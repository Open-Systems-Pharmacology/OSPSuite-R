library(ospsuite)



sim <- loadSimulation("C:/projects/OSPSuite-R/tests/data/S1.pkml")

# This is only required until SimModel fixes problem with CVODES
setwd("C:/projects/OSPSuite-R/inst/lib")

parameter <- getParameter("Organism|Liver|Volume", sim)
print(parameter)

results <- runSimulation(sim)

individualResults <- results[[1]]

time <- rClr::clrGet(individualResults, "Time")

allValues <- rClr::clrCall(individualResults, "ValuesAsArray")

firstOutput <- allValues[[1]]
path <- rClr::clrGet(firstOutput, "QuantityPath")
values <- rClr::clrGet(firstOutput, "Values")

