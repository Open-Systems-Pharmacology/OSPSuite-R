test_that("It can load a valid MoBi project", {
  moBiProject <- loadMoBiProject(
    filePath = getTestDataFilePath("Empty_Project.mbp3")
  )
  expect_true(isOfType(moBiProject, "MoBiProject"))
  expect_equal(
    moBiProject$sourceFile,
    getTestDataFilePath("Empty_Project.mbp3")
  )
})
