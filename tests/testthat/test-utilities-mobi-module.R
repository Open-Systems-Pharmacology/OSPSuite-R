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

# passing name of IC BB that is not present in the module. requires https://github.com/Open-Systems-Pharmacology/MoBi/issues/2027
# Passing name of PV BB that is not present in the module. requires https://github.com/Open-Systems-Pharmacology/MoBi/issues/2027
