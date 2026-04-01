# Molecule in concentration mode

sim_conc_based <- loadTestSimulation(
  "concentration_based",
  loadFromCache = FALSE,
  addToCache = FALSE
)
molecule <- getMolecule("Organism|M", sim_conc_based)

test_that("Setting the value of a molecule in concentration mode should not override the formula but set the value in the Start value parameter", {
  molecule$value <- 50
  expect_false(molecule$isFixedValue)

  startValueParameter <- getParameter("Start value", molecule)
  expect_true(startValueParameter$isFixedValue)
})

test_that("It can print molecule", {
  expect_snapshot(molecule$print())
})

test_that("It can set the scale divisor", {
  scaleDivisor <- molecule$scaleDivisor
  molecule$scaleDivisor <- 0.025
  expect_equal(molecule$scaleDivisor, 0.025)
})
