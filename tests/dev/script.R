library(ospsuite)

sim <- loadSimulation("C:/projects/rDotNet_feasibility/lib/S1.pkml")

parameter <- getParameter(c("Organism", "Liver", "Volume"), sim)

print(parameter$id)


