test_that("It can load a valid MoBi project", {
  moBiProject <- loadMoBiProject(filePath = getTestDataFilePath("TH_QST_Platform.mbp3"))
  expect_true(isOfType(moBiProject, "MoBiProject"))
})
