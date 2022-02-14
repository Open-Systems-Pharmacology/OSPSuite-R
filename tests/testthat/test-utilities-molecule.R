

sim <- loadTestSimulation("S1")

context("getAllMoleculesMatching")

test_that("It can retrieve molecule with absolute path", {
  molecules <- getAllMoleculesMatching("Organism|VenousBlood|Plasma|Caffeine", sim)
  expect_equal(length(molecules), 1)
})

test_that("It can retrieve molecule with wildcard path", {
  molecules <- getAllMoleculesMatching("Organism|VenousBlood|*|Caffeine", sim)
  expect_equal(length(molecules), 2)
})

test_that("It returns an empty array if the molecule with the given path is not found", {
  molecules <- getAllMoleculesMatching("Organism|test|*|Caffeine", sim)
  expect_length(molecules, 0)
})

context("getMolecule")

test_that("It can retrieve a single molecule with absolute path", {
  molecule <- getMolecule("Organism|VenousBlood|Plasma|Caffeine", sim)
  expect_false(is.null(molecule))
})


context("getAllMoleculePathsIn")

test_that("It can retrieve all molecule paths defined in the simulation", {
  paths <- getAllMoleculePathsIn(sim)
  expect_gt(length(paths), 0)
})

test_that("It can retrieve all molecule paths defined in a container", {
  paths <- getAllMoleculePathsIn(sim$root)
  expect_gt(length(paths), 0)
})

context("setMoleculeInitialValues")

test_that("It throws an error when no valid molecule objects are provided", {
  expect_that(setMoleculeInitialValues("quantity", 1), throws_error())
})

test_that("It throws an error when no valid values are provided", {
  molecules <- getAllMoleculesMatching("Organism|VenousBlood|Plasma|Caffeine", sim)
  expect_that(setMoleculeInitialValues(molecules, "s"), throws_error())
})

test_that("It throws an error when the number of quantity differs from the number of values", {
  molecule <- getMolecule("Organism|VenousBlood|Plasma|Caffeine", sim)
  molecules <- getAllMoleculesMatching("Organism|VenousBlood|*|Caffeine", sim)
  expect_that(setMoleculeInitialValues(molecule, c(1, 2)), throws_error())
  expect_that(setMoleculeInitialValues(molecules, c(1:5)), throws_error())
})

test_that("It can set the value of a single quantity", {
  molecule <- getMolecule("Organism|VenousBlood|Plasma|Caffeine", sim)
  setMoleculeInitialValues(molecule, 1)
  expect_equal(molecule$value, 1)
})

test_that("It can set the values of multiple molecules", {
  molecules <- getAllMoleculesMatching("Organism|VenousBlood|*|Caffeine", sim)
  setMoleculeInitialValues(molecules, c(1:2))
  newVals <- sapply(molecules, function(x) {
    x$value
  })
  expect_equal(newVals, c(1:2))
})

context("setMoleculeScaleDivisors")

test_that("It throws an error when no valid molecule objects are provided", {
  expect_that(setMoleculeScaleDivisors("quantity", 1), throws_error())
})

test_that("It throws an error when no valid values are provided", {
  molecules <- getAllMoleculesMatching("Organism|VenousBlood|Plasma|Caffeine", sim)
  expect_that(setMoleculeScaleDivisors(molecules, "s"), throws_error())
})

test_that("It throws an error when the number of molecules differs from the number of values", {
  molecule <- getMolecule("Organism|VenousBlood|Plasma|Caffeine", sim)
  molecules <- getAllMoleculesMatching("Organism|VenousBlood|*|Caffeine", sim)
  expect_that(setMoleculeScaleDivisors(molecule, c(1, 2)), throws_error())
  expect_that(setMoleculeScaleDivisors(molecules, c(1:5)), throws_error())
})

test_that("It can set the scale divisor of a single quantity", {
  molecule <- getMolecule("Organism|VenousBlood|Plasma|Caffeine", sim)
  setMoleculeScaleDivisors(molecule, 0.5)
  expect_equal(molecule$scaleDivisor, 0.5)
})

test_that("It can set the  scale divisors of multiple molecules", {
  molecules <- getAllMoleculesMatching("Organism|VenousBlood|*|Caffeine", sim)
  setMoleculeScaleDivisors(molecules, c(1:2))
  newVals <- sapply(molecules, function(x) {
    x$scaleDivisor
  })
  expect_equal(newVals, c(1:2))
})
