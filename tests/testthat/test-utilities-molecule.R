

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
