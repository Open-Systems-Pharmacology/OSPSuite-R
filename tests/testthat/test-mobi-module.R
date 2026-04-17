testModule_noIC_noPV <- globalTestMoBiProject$getModules(
  "ExtModule_noIC_noPV"
)[[1]]
testModule_3IC_3_PV <- globalTestMoBiProject$getModules("ExtModule_3IC_3PV")[[
  1
]]

# Getting module name
test_that("MoBiModule name is correct", {
  expect_equal(testModule_noIC_noPV$name, "ExtModule_noIC_noPV")
  # test for read only
  expect_error(
    testModule_noIC_noPV$name <- "NewName",
    "Property 'name' is read-only"
  )
})

# Test for isPkSimModule
test_that("MoBiModule isPkSimModule returns correct value", {
  expect_false(testModule_noIC_noPV$isPKSimModule)
  # test for read only
  expect_error(
    testModule_noIC_noPV$isPKSimModule <- TRUE,
    "Property 'isPKSimModule' is read-only"
  )
})

# Snapshot test for printing a Module
test_that("MoBiModule prints correctly", {
  expect_snapshot_output(print(testModule_3IC_3_PV))
})

# Test for mergeBehavior
test_that("MoBiModule mergeBehavior works correctly", {
  expect_equal(testModule_noIC_noPV$mergeBehavior, "Extend")

  # Test for read only
  expect_error(
    testModule_noIC_noPV$mergeBehavior <- "Overwrite",
    "Property 'mergeBehavior' is read-only"
  )

  # TODO: https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/1591
  # # Test setting merge behavior
  # testModuleGlobal$mergeBehavior <- "Overwrite"
  # expect_equal(testModuleGlobal$mergeBehavior, "Overwrite")
  #
  # # Test setting invalid merge behavior
  # expect_error(testModuleGlobal$mergeBehavior <- "InvalidBehavior", "Invalid value for enum 'MergeBehavior'")
})

#  Test for getParameterValuesBBs
test_that("getParameterValuesBBs returns an empty list for a module with now PV BBs", {
  pvBBs <- testModule_noIC_noPV$getParameterValuesBBs()
  expect_equal(length(pvBBs), 0)
})

test_that("It throws an error when trying to get PV BBs with names for a module with no PV BBs", {
  expect_error(
    testModule_noIC_noPV$getParameterValuesBBs(names = c("PV1", "PV2")),
    "No Parameter Values Building Blocks found with names: PV1, PV2"
  )
})

test_that("It throws an error when trying to get PV BBs with names that do not exist and stopIfNotFound is TRUE", {
  expect_error(
    testModule_3IC_3_PV$getParameterValuesBBs(names = c("NonExistentPV")),
    "No Parameter Values Building Blocks found with names: NonExistentPV"
  )
})

test_that("It returns only the present PV BBs when trying to get PV BBs with names that do not exist and stopIfNotFound is FALSE", {
  expect_named(
    testModule_3IC_3_PV$getParameterValuesBBs(
      names = c("NonExistentPV", "PV1"),
      stopIfNotFound = FALSE
    ),
    "PV1"
  )
})

test_that("It returns the specified PV BBs when names are provided", {
  pvBBs <- testModule_3IC_3_PV$getParameterValuesBBs(names = c("PV3", "PV1"))
  expect_equal(length(pvBBs), 2)
  expect_true(isOfType(pvBBs, "BuildingBlock"))
  expect_equal(pvBBs$PV1$name, "PV1")
  expect_equal(pvBBs$PV3$name, "PV3")
  expect_equal(pvBBs[[1]]$type, "Parameter Values")
})

test_that("getParameterValuesBBs returns all PV BBs for module with multiple PV BBs", {
  pvBBs <- testModule_3IC_3_PV$getParameterValuesBBs()
  expect_true(isOfType(pvBBs, "BuildingBlock"))
  expect_equal(pvBBs[[1]]$name, "PV1")
  expect_equal(pvBBs[[2]]$name, "PV2")
  expect_equal(pvBBs[[3]]$name, "PV3")
  expect_equal(pvBBs[[1]]$type, "Parameter Values")
})

test_that("It returns the names of all PV BBs", {
  pvBBs <- testModule_3IC_3_PV$parameterValuesBBnames
  expect_equal(pvBBs, c("PV1", "PV2", "PV3"))

  # Module with no PV BBs should return an empty character vector
  expect_equal(testModule_noIC_noPV$parameterValuesBBnames, character(0))

  # Test for read only
  expect_error(
    testModule_3IC_3_PV$parameterValuesBBnames <- c("NewPV1", "NewPV2"),
    "Property 'parameterValuesBBnames' is read-only"
  )
})

# Test for getInitialConditionsBBs

test_that("getInitialConditionsBBs returns an empty list for a module with no IC BBs", {
  icBBs <- testModule_noIC_noPV$getInitialConditionsBBs()
  expect_equal(length(icBBs), 0)
})

test_that("It throws an error when trying to get IC BBs with names for a module with no IC BBs", {
  expect_error(
    testModule_noIC_noPV$getInitialConditionsBBs(names = c("IC1", "IC2")),
    "No Initial Conditions Building Blocks found with names: IC1, IC2"
  )
})

test_that("It throws an error when trying to get IC BBs with names that do not exist and stopIfNotFound is TRUE", {
  expect_error(
    testModule_3IC_3_PV$getInitialConditionsBBs(names = c("NonExistentIC")),
    "No Initial Conditions Building Blocks found with names: NonExistentIC"
  )
})

test_that("It returns only the present IC BBs when trying to get IC BBs with names that do not exist and stopIfNotFound is FALSE", {
  expect_named(
    testModule_3IC_3_PV$getInitialConditionsBBs(
      names = c("NonExistentIC", "IC1"),
      stopIfNotFound = FALSE
    ),
    "IC1"
  )
})

test_that("It returns the specified IC BBs when names are provided", {
  icBBs <- testModule_3IC_3_PV$getInitialConditionsBBs(names = c("IC3", "IC1"))
  expect_equal(length(icBBs), 2)
  expect_true(isOfType(icBBs, "BuildingBlock"))
  expect_equal(icBBs$IC1$name, "IC1")
  expect_equal(icBBs$IC3$name, "IC3")
  expect_equal(icBBs[[1]]$type, "Initial Conditions")
})

test_that("getInitialConditionsBBs returns all IC BBs for module with multiple IC BBs", {
  icBBs <- testModule_3IC_3_PV$getInitialConditionsBBs()
  expect_true(isOfType(icBBs, "BuildingBlock"))
  expect_equal(icBBs[[1]]$name, "IC1")
  expect_equal(icBBs[[2]]$name, "IC2")
  expect_equal(icBBs[[3]]$name, "IC3")
  expect_equal(icBBs[[1]]$type, "Initial Conditions")
})

test_that("It returns the names of all IC BBs", {
  icBBs <- testModule_3IC_3_PV$initialConditionsBBnames
  expect_equal(icBBs, c("IC1", "IC2", "IC3"))

  # Module with no IC BBs should return an empty character vector
  expect_equal(testModule_noIC_noPV$initialConditionsBBnames, character(0))

  # Test for read only
  expect_error(
    testModule_3IC_3_PV$initialConditionsBBnames <- c("NewIC1", "NewIC2"),
    "Property 'initialConditionsBBnames' is read-only"
  )
})
