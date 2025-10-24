skip_on_os("mac")

testMoBiProject <- loadMoBiProject(
  filePath = getTestDataFilePath("Test_Project.mbp3")
)
testModule <- testMoBiProject$getModules("ExtModule_noIC_noPV")[[1]]

# Getting module name
test_that("MoBiModule name is correct", {
  expect_equal(testModule$name, "ExtModule_noIC_noPV")
  # test for read only
  expect_error(testModule$name <- "NewName", "Property 'name' is read-only")
})

# Test for isPkSimModule
test_that("MoBiModule isPkSimModule returns correct value", {
  expect_false(testModule$isPKSimModule)
  # test for read only
  expect_error(
    testModule$isPKSimModule <- TRUE,
    "Property 'isPkSimModule' is read-only"
  )
})

# Snapshot test for printing a Module
test_that("MoBiModule prints correctly", {
  testModule <- testMoBiProject$getModules("ExtModule_3IC_3PV")[[1]]
  expect_snapshot_output(print(testModule))
})

# Test for mergeBehavior
test_that("MoBiModule mergeBehavior works correctly", {
  expect_equal(testModule$mergeBehavior, "Extend")

  # Test for read only
  expect_error(
    testModule$mergeBehavior <- "Overwrite",
    "Property 'mergeBehavior' is read-only"
  )

  # TODO
  # # Test setting merge behavior
  # testModule$mergeBehavior <- "Overwrite"
  # expect_equal(testModule$mergeBehavior, "Overwrite")
  #
  # # Test setting invalid merge behavior
  # expect_error(testModule$mergeBehavior <- "InvalidBehavior", "Invalid value for enum 'MergeBehavior'")
})

#  Test for getParameterValuesBBs
test_that("getParameterValuesBBs returns an empty list for a module with now PV BBs", {
  testModule <- testMoBiProject$getModules("ExtModule_noIC_noPV")[[1]]

  pvBBs <- testModule$getParameterValuesBBs()
  expect_equal(length(pvBBs), 0)
})

test_that("It throws an error when trying to get PV BBs with names for a module with no PV BBs", {
  testModule <- testMoBiProject$getModules("ExtModule_noIC_noPV")[[1]]

  expect_error(
    testModule$getParameterValuesBBs(names = c("PV1", "PV2")),
    "No Parameter Values Building Blocks found with names: PV1, PV2"
  )
})

test_that("It throws an error when trying to get PV BBs with names that do not exist", {
  testModule <- testMoBiProject$getModules("ExtModule_3IC_3PV")[[1]]

  expect_error(
    testModule$getParameterValuesBBs(names = c("NonExistentPV")),
    "No Parameter Values Building Blocks found with names: NonExistentPV"
  )
})

test_that("It returns the specified PV BBs when names are provided", {
  testModule <- testMoBiProject$getModules("ExtModule_3IC_3PV")[[1]]

  pvBBs <- testModule$getParameterValuesBBs(names = c("PV3", "PV1"))
  expect_equal(length(pvBBs), 2)
  expect_true(isOfType(pvBBs, "BuildingBlock"))
  expect_equal(pvBBs$PV1$name, "PV1")
  expect_equal(pvBBs$PV3$name, "PV3")
  expect_equal(pvBBs[[1]]$type, "Parameter Values")
})

test_that("getParameterValuesBBs returns all PV BBs for module with multiple PV BBs", {
  testModule <- testMoBiProject$getModules("ExtModule_3IC_3PV")[[1]]

  pvBBs <- testModule$getParameterValuesBBs()
  expect_true(isOfType(pvBBs, "BuildingBlock"))
  expect_equal(pvBBs[[1]]$name, "PV1")
  expect_equal(pvBBs[[2]]$name, "PV2")
  expect_equal(pvBBs[[3]]$name, "PV3")
  expect_equal(pvBBs[[1]]$type, "Parameter Values")
})

# Test for getInitialConditionsBBs

test_that("getInitialConditionsBBs returns an empty list for a module with no IC BBs", {
  testModule <- testMoBiProject$getModules("ExtModule_noIC_noPV")[[1]]

  icBBs <- testModule$getInitialConditionsBBs()
  expect_equal(length(icBBs), 0)
})

test_that("It throws an error when trying to get IC BBs with names for a module with no IC BBs", {
  testModule <- testMoBiProject$getModules("ExtModule_noIC_noPV")[[1]]

  expect_error(
    testModule$getInitialConditionsBBs(names = c("IC1", "IC2")),
    "No Initial Conditions Building Blocks found with names: IC1, IC2"
  )
})

test_that("It throws an error when trying to get IC BBs with names that do not exist", {
  testModule <- testMoBiProject$getModules("ExtModule_3IC_3PV")[[1]]

  expect_error(
    testModule$getInitialConditionsBBs(names = c("NonExistentIC")),
    "No Initial Conditions Building Blocks found with names: NonExistentIC"
  )
})

test_that("It returns the specified IC BBs when names are provided", {
  testModule <- testMoBiProject$getModules("ExtModule_3IC_3PV")[[1]]

  icBBs <- testModule$getInitialConditionsBBs(names = c("IC3", "IC1"))
  expect_equal(length(icBBs), 2)
  expect_true(isOfType(icBBs, "BuildingBlock"))
  expect_equal(icBBs$IC1$name, "IC1")
  expect_equal(icBBs$IC3$name, "IC3")
  expect_equal(icBBs[[1]]$type, "Initial Conditions")
})

test_that("getInitialConditionsBBs returns all IC BBs for module with multiple IC BBs", {
  testModule <- testMoBiProject$getModules("ExtModule_3IC_3PV")[[1]]

  icBBs <- testModule$getInitialConditionsBBs()
  expect_true(isOfType(icBBs, "BuildingBlock"))
  expect_equal(icBBs[[1]]$name, "IC1")
  expect_equal(icBBs[[2]]$name, "IC2")
  expect_equal(icBBs[[3]]$name, "IC3")
  expect_equal(icBBs[[1]]$type, "Initial Conditions")
})

# test_that("It returns the names of all IC BBs", {
#   testModule <- testMoBiProject$getModules("ExtModule_3IC_3PV")[[1]]
#
#   icBBs <- testModule$initialConditionsBBnames
#   expect_equal(icBBs, c("IC1", "IC2", "IC3"))
#
#   # Test for read only
#   expect_error(testModule$initialConditionsBBnames <- c("NewIC1", "NewIC2"), "Property 'initialConditionsBBnames' is read-only")
# })
