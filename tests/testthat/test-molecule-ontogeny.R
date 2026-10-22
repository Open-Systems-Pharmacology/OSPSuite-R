# MoleculeOntogeny

test_that("It can create a molecule ontogeny for a predefined ontogeny", {
  moleculeOntogeny <- MoleculeOntogeny$new(
    molecule = "MyMolecule",
    ontogeny = StandardOntogeny$CYP2C18
  )
  expect_false(is.null(moleculeOntogeny))
  expect_snapshot(moleculeOntogeny$print())
})

test_that("It can create a molecule ontogeny for CES1 and ABCB1", {
  for (ontogeny in c(StandardOntogeny$CES1, StandardOntogeny$ABCB1)) {
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
