# Molecule in concentration mode

sim_conc_based <- loadTestSimulation("concentration_based")

test_that("Setting the value of a molecule in concentration mode should not override the formula but set the value in the Start value parameter", {
  molecule <- getMolecule("Organism|M", sim_conc_based)
  molecule$value <- 50
  expect_false(molecule$isFixedValue)

  startValueParameter <- getParameter("Start value", molecule)
  expect_true(startValueParameter$isFixedValue)
})

test_that("It can print molecule", {
  molecule <- getMolecule("Organism|M", sim_conc_based)
  expectSnapshotPrint(molecule)
})

test_that("It can set the scale divisor", {
  molecule <- getMolecule("Organism|M", sim_conc_based)
  scaleDivisor <- molecule$scaleDivisor
  molecule$scaleDivisor <- 0.025
  expect_equal(molecule$scaleDivisor, 0.025)
})
