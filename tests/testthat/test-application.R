#  Application
quantityPath <- "Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)"
sim_mutable <- loadSimulation(
  aciclovirSimulationPath,
  loadFromCache = FALSE,
  addToCache = FALSE
)
applications <- sim_mutable$allApplicationsFor(quantityPath)
application <- applications[[1]]

test_that("It can set the value of the start time for the application", {
  application$startTime$value <- 10
  expect_equal(application$startTime$value, 10)
})

# Application$print
test_that("It can print an application", {
  expect_snapshot(application$print())
})
