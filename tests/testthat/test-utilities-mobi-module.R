testMoBiProject <- loadMoBiProject(
  filePath = getTestDataFilePath("Test_Project.mbp3")
)

# Test for .createModuleConfiguration

# Create a module configuration with no IC and PV BBs
test_that(".createModuleConfiguration creates a module configuration with no IC and PV BBs", {
  module <- testMoBiProject$getModules("ExtModule_noIC_noPV")[[1]]
  netModuleConfiguration <- .createModuleConfiguration(module)

  netModuleConfiguration$get("SelectedParameterValue") %>%
    expect_null()
  netModuleConfiguration$get("SelectedInitialCondition") %>%
    expect_null()

  # WIth manual passing NULL as IC and PV BBs
  netModuleConfiguration <- .createModuleConfiguration(module, NULL, NULL)
  netModuleConfiguration$get("SelectedParameterValue") %>%
    expect_null()
  netModuleConfiguration$get("SelectedInitialCondition") %>%
    expect_null()
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
  module <- loadModuleFromPKML(filePath)
})

test_that("loadModuleFromPKML throws an error when the passed PKML doest not contain any module", {})

# passing name of IC BB that is not present in the module. requires https://github.com/Open-Systems-Pharmacology/MoBi/issues/2027
# Passing name of PV BB that is not present in the module. requires https://github.com/Open-Systems-Pharmacology/MoBi/issues/2027
