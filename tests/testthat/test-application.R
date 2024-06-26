#  Application
quantityPath <- "Organism|PeripheralVenousBlood|Caffeine|Plasma (Peripheral Venous Blood)"
sim <- loadTestSimulation("S1")
applications <- sim$allApplicationsFor(quantityPath)
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
  expect_error(capture.output(application$print()), NA)
})
