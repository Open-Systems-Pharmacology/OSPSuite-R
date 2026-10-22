# MoleculeOntogeny

test_that("It can create a molecule ontogeny for every standard ontogeny", {
  for (ontogeny in StandardOntogeny) {
    moleculeOntogeny <- MoleculeOntogeny$new(
      molecule = "MyMolecule",
      ontogeny = ontogeny
    )
    expect_equal(moleculeOntogeny$ontogeny, ontogeny)
  }
})

test_that("It throws an error when creating a molecule ontogeny for an unknown ontogeny", {
  expect_error(MoleculeOntogeny$new(
    molecule = "MyMolecule",
    ontogeny = "Unknown"
  ))
})
