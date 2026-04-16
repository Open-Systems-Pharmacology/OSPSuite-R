.normalizePKMLForComparison <- function(filePath) {
  if (!file.exists(filePath)) {
    stop(messages$errorFileDoesNotExist(filePath))
  }
  doc <- xml2::read_xml(filePath)
  nodesWithId <- xml2::xml_find_all(doc, ".//*[@id]")
  xml2::xml_attr(nodesWithId, "id") <- ""
  xml2::as_list(doc)
}

.expectBuildingBlockRoundTrip <- function(loadFunction, saveFunction, filePath) {
  originalBuildingBlock <- loadFunction(filePath)
  exportedFilePath <- tempfile(fileext = ".pkml")
  reExportedFilePath <- tempfile(fileext = ".pkml")
  on.exit(unlink(c(exportedFilePath, reExportedFilePath), force = TRUE), add = TRUE)

  saveFunction(originalBuildingBlock, exportedFilePath)
  exportedBuildingBlock <- loadFunction(exportedFilePath)
  saveFunction(exportedBuildingBlock, reExportedFilePath)

  expect_equal(originalBuildingBlock$name, exportedBuildingBlock$name)
  expect_equal(
    .normalizePKMLForComparison(exportedFilePath),
    .normalizePKMLForComparison(reExportedFilePath)
  )
}

test_that("it can save expression profile building blocks as pkml", {
  .expectBuildingBlockRoundTrip(
    loadFunction = loadExpressionProfileFromPKML,
    saveFunction = saveExpressionProfileToPKML,
    filePath = aciclovirSimulationPath
  )
})

test_that("it can save individual building blocks as pkml", {
  .expectBuildingBlockRoundTrip(
    loadFunction = loadIndividualFromPKML,
    saveFunction = saveIndividualToPKML,
    filePath = aciclovirSimulationPath
  )
})

test_that("it can save initial conditions building blocks as pkml", {
  .expectBuildingBlockRoundTrip(
    loadFunction = loadInitialConditionsFromPKML,
    saveFunction = saveInitialConditionsToPKML,
    filePath = aciclovirSimulationPath
  )
})

test_that("it can save parameter values building blocks as pkml", {
  .expectBuildingBlockRoundTrip(
    loadFunction = loadParameterValuesFromPKML,
    saveFunction = saveParameterValuesToPKML,
    filePath = aciclovirSimulationPath
  )
})
