test_that("It can print population charachteristics", {
  popCharacteristics <- PopulationCharacteristics$new()
  popCharacteristics$addMoleculeOntogeny(MoleculeOntogeny$new(
    molecule = "CYP3A4",
    ontogeny = "CYP3A4"
  ))
  expect_snapshot(print(popCharacteristics))
})
