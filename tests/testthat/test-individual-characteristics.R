test_that("It can print individual charachteristics", {
  myProteinOntogeny <- MoleculeOntogeny$new(
    molecule = "MyProtein",
    ontogeny = StandardOntogeny$CYP3A4
  )
  # Add this ontogeny to the individual characteristics used to create the individual parameters set
  individualCharacterstics <- createIndividualCharacteristics(
    species = Species$Human,
    population = HumanPopulation$Japanese_Population,
    gender = Gender$Female,
    weight = 75,
    height = 1.75,
    heightUnit = "m",
    age = 43,
    moleculeOntogenies = myProteinOntogeny
  )
  expect_snapshot(print(individualCharacterstics))
})
