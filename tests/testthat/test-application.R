#  Application
quantityPath <- "Organism|PeripheralVenousBlood|Caffeine|Plasma (Peripheral Venous Blood)"
sim_mutable <- loadTestSimulation(
  "S1",
  loadFromCache = FALSE,
  addToCache = FALSE
)
applications <- sim_mutable$allApplicationsFor(quantityPath)
application <- applications[[1]]

test_that("It can retrieve the applications defined for the simulation", {
  expect_false(is.null(application))
})

test_that("It can set the value of the start time for the application", {
  application$startTime$value <- 10
  expect_equal(application$startTime$value, 10)
})

# Application$print
test_that("It can print an application", {
  expect_snapshot(application$print())
})
