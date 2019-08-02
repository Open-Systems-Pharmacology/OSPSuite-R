library(ospsuite)



sim <- loadSimulation("C:/projects/rDotNet_feasibility/lib/S1.pkml")

parameter <- getParameter(c("Organism", "Liver", "Volume"), sim)
print(parameter)

results <- runSimulation(sim)

individualResults <- results[[1]]

time <- rClr::clrGet(individualResults, "Time")

allValues <- rClr::clrCall(individualResults, "ValuesAsArray")

firstOutput <- allValues[[1]]
path <- rClr::clrGet(firstOutput, "QuantityPath")
values <- rClr::clrGet(firstOutput, "Values")
