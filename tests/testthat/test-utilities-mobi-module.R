testMoBiProject <- loadMoBiProject(
  filePath = getTestDataFilePath("Test_Project.mbp3")
)

# Test for .createModuleConfiguration

# Create a module configuration with no IC and PV BBs
test_that(".createModuleConfiguration creates a module configuration with no IC and PV BBs", {
  module <- testMoBiProject$getModules("ExtModule_noIC_noPV")[[1]]
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
  module <- testMoBiProject$getModules("ExtModule_3IC_3PV")[[1]]

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
  filePath <- getTestDataFilePath("TestSim_2Modules.pkml")
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
