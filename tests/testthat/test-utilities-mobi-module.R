# Test for .createModuleConfiguration

# Create a module configuration with no IC and PV BBs
test_that(".createModuleConfiguration creates a module configuration with no IC and PV BBs", {
  module <- globalTestMoBiProject$getModules("ExtModule_noIC_noPV")[[1]]
  netModuleConfiguration <- .createModuleConfiguration(module)

  # Expect that no PV and IC BBs are selected
  expect_null(netModuleConfiguration$get("SelectedParameterValue"))
  expect_null(netModuleConfiguration$get("SelectedInitialCondition"))

  # With manual passing NULL as IC and PV BBs
  netModuleConfiguration <- .createModuleConfiguration(module, NULL, NULL)
  netModuleConfiguration$get("SelectedParameterValue") %>%
    expect_null()
  netModuleConfiguration$get("SelectedInitialCondition") %>%
    expect_null()
})

# Create a module configuration with specified IC and PV BBs
test_that(".createModuleConfiguration creates a module configuration with IC and PV BBs", {
  module <- globalTestMoBiProject$getModules("ExtModule_3IC_3PV")[[1]]

  netModuleConfiguration <- .createModuleConfiguration(
    module,
    selectedParameterValueName = "PV2",
    selectedInitialConditionName = "IC3"
  )

  # Expect that the correct PV and IC BBs are selected
  selectedPV <- netModuleConfiguration$get("SelectedParameterValue")
  selectedIC <- netModuleConfiguration$get("SelectedInitialCondition")

  expect_equal(selectedPV$get("Name"), "PV2")
  expect_equal(selectedIC$get("Name"), "IC3")
})

# Loading a module from pkml
test_that("loadModuleFromPKML loads a module correctly", {
  filePath <- system.file("extdata", "Thyroid.pkml", package = "ospsuite")
  module <- loadModuleFromPKML(filePath)

  expect_true(isOfType(module, "MoBiModule"))
  expect_equal(module$name, "Thyroid_Generic")
  expect_false(module$isPKSimModule)
})

test_that("loadModuleFromPKML throws an error when the passed PKML contains more than one module", {
  filePath <- getTestDataFilePath("MoBiProject/TestSim_2Modules.pkml")
  expect_error(
    loadModuleFromPKML(filePath),
    regexp = "The PKML you are trying to load the module from contains more than one module, but the 
    function expects only one module.
    Most probably you are trying to load a simulation export."
  )
})

test_that("loadModuleFromPKML throws an error when the passed PKML does not contain any module", {
  filePath <- getTestDataFilePath("S1.pkml")
  expect_error(loadModuleFromPKML(filePath))
})

# --- createMoBiModule ---------------------------------------------------------

# Construct a fresh, detached building block for tests using the engine-level
# constructor. Lets us instantiate single-type BBs (Molecules, Reactions, ...)
# without round-tripping through pkml.
.freshTestBB <- function(typeName, type, name = "Test") {
  netBB <- rSharp::newObjectFromName(
    paste0("OSPSuite.Core.Domain.Builder.", typeName)
  )
  netBB$set("Name", name)
  if (type == BuildingBlockTypes$Molecules) {
    return(MoleculesBuildingBlock$new(netBB))
  }
  BuildingBlock$new(netBB, type = type)
}

test_that("createMoBiModule creates an empty module with the given name", {
  module <- createMoBiModule("FromR")

  expect_true(isOfType(module, "MoBiModule"))
  expect_equal(module$name, "FromR")
  expect_equal(length(module$initialConditionsBBnames), 0)
  expect_equal(length(module$parameterValuesBBnames), 0)
  expect_null(module$getMoleculesBB())
})

test_that("createMoBiModule attaches the provided building blocks", {
  ic1 <- .freshTestBB(
    "InitialConditionsBuildingBlock",
    BuildingBlockTypes$`Initial Conditions`,
    "IC1"
  )
  ic2 <- .freshTestBB(
    "InitialConditionsBuildingBlock",
    BuildingBlockTypes$`Initial Conditions`,
    "IC2"
  )
  pv1 <- .freshTestBB(
    "ParameterValuesBuildingBlock",
    BuildingBlockTypes$`Parameter Values`,
    "PV1"
  )
  mol <- .freshTestBB(
    "MoleculeBuildingBlock",
    BuildingBlockTypes$Molecules,
    "M"
  )

  module <- createMoBiModule(
    "WithBBs",
    buildingBlocks = list(ic1, ic2, pv1, mol)
  )

  expect_setequal(module$initialConditionsBBnames, c("IC1", "IC2"))
  expect_equal(module$parameterValuesBBnames, "PV1")
  expect_true(isOfType(module$getMoleculesBB(), "MoleculesBuildingBlock"))
  expect_equal(module$getMoleculesBB()$name, "M")
})

test_that("createMoBiModule errors when name is empty or not a string", {
  expect_error(createMoBiModule(""))
  expect_error(createMoBiModule(123))
  expect_error(createMoBiModule(NULL))
})

test_that("createMoBiModule accepts a single BuildingBlock as `buildingBlocks`", {
  ic <- .freshTestBB(
    "InitialConditionsBuildingBlock",
    BuildingBlockTypes$`Initial Conditions`,
    "ICOnly"
  )

  module <- createMoBiModule("WithSingleBB", buildingBlocks = ic)

  expect_equal(module$initialConditionsBBnames, "ICOnly")
})

test_that("createMoBiModule errors when buildingBlocks has the wrong type", {
  expect_error(
    createMoBiModule("Foo", buildingBlocks = "not a BB"),
    regexp = "BuildingBlock"
  )
  expect_error(
    createMoBiModule("Foo", buildingBlocks = list("not a BB")),
    regexp = "BuildingBlock"
  )
})

test_that("createMoBiModule errors when a single-type BB is passed twice", {
  m1 <- .freshTestBB(
    "MoleculeBuildingBlock",
    BuildingBlockTypes$Molecules,
    "M1"
  )
  m2 <- .freshTestBB(
    "MoleculeBuildingBlock",
    BuildingBlockTypes$Molecules,
    "M2"
  )
  expect_error(createMoBiModule("DupMol", buildingBlocks = list(m1, m2)))
})
