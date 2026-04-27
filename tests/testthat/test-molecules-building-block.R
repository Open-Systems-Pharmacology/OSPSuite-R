.getMoleculesBBFixture <- function() {
  module <- globalTestMoBiProject$getModules("TestModule")[[1]]
  .getBBFromModule(module, bbType = "Molecules")
}

test_that("MoleculesBuildingBlock is returned by .getBBFromModule for Molecules type", {
  mBB <- .getMoleculesBBFixture()
  expect_true(isOfType(mBB, "MoleculesBuildingBlock"))
  expect_equal(mBB$type, BuildingBlockTypes$Molecules)
})

test_that("allMoleculeNames returns all molecule names of the building block", {
  mBB <- .getMoleculesBBFixture()
  names <- mBB$allMoleculeNames()
  expect_setequal(
    names,
    c("A", "B", "UGT2B7", "CYP3A4", "FloatingMolecule")
  )
})

test_that("allFloatingMoleculeNames returns only floating molecules", {
  mBB <- .getMoleculesBBFixture()
  expect_equal(mBB$allFloatingMoleculeNames(), "FloatingMolecule")
})

test_that("allStationaryMoleculeNames returns only stationary molecules", {
  mBB <- .getMoleculesBBFixture()
  expect_setequal(
    mBB$allStationaryMoleculeNames(),
    c("A", "B", "UGT2B7", "CYP3A4")
  )
})

test_that("allMoleculeNamesOfType(Protein) returns all proteins", {
  mBB <- .getMoleculesBBFixture()
  expect_setequal(
    mBB$allMoleculeNamesOfType(MoleculeType$Protein),
    c("UGT2B7", "CYP3A4")
  )
})

test_that("allMoleculeNamesOfType(Drug) returns only drugs", {
  mBB <- .getMoleculesBBFixture()
  expect_setequal(
    mBB$allMoleculeNamesOfType(MoleculeType$Drug),
    c("A", "B", "FloatingMolecule")
  )
})

test_that("allMoleculeNamesOfType(Enzyme) returns only enzymes", {
  mBB <- .getMoleculesBBFixture()
  expect_setequal(
    mBB$allMoleculeNamesOfType(MoleculeType$Enzyme),
    c("UGT2B7", "CYP3A4")
  )
})

test_that("allMoleculeNamesOfType(Transporter) returns only transporters", {
  mBB <- .getMoleculesBBFixture()
  expect_equal(
    mBB$allMoleculeNamesOfType(MoleculeType$Transporter),
    character(0)
  )
})

test_that("allMoleculeNamesOfType validates the moleculeType argument", {
  mBB <- .getMoleculesBBFixture()
  expect_error(mBB$allMoleculeNamesOfType("NotAType"))
})

test_that("allXenobioticFloatingMoleculeNames returns xenobiotic floating molecules", {
  mBB <- .getMoleculesBBFixture()
  expect_equal(mBB$allXenobioticFloatingMoleculeNames(), "FloatingMolecule")
})

test_that("allEndogenousStationaryMoleculeNames returns endogenous stationary molecules", {
  mBB <- .getMoleculesBBFixture()
  expect_setequal(
    mBB$allEndogenousStationaryMoleculeNames(),
    c("UGT2B7", "CYP3A4")
  )
})

test_that("moleculeTypeFor returns the type of an existing molecule", {
  mBB <- .getMoleculesBBFixture()
  expect_equal(mBB$moleculeTypeFor("UGT2B7"), "Enzyme")
  expect_equal(mBB$moleculeTypeFor("CYP3A4"), "Enzyme")
  expect_equal(mBB$moleculeTypeFor("A"), "Drug")
})

test_that("moleculeTypeFor errors for an unknown molecule", {
  mBB <- .getMoleculesBBFixture()
  expect_error(mBB$moleculeTypeFor("NotThere"))
})

test_that("moleculeTypeFor validates the moleculeName argument", {
  mBB <- .getMoleculesBBFixture()
  expect_error(mBB$moleculeTypeFor(123))
})
