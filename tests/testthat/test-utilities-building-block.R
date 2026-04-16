.normalizePKMLForComparison <- function(filePath) {
  if (!file.exists(filePath)) {
    stop(messages$errorFileDoesNotExist(filePath))
  }
  doc <- xml2::read_xml(filePath)
  nodesWithId <- xml2::xml_find_all(doc, ".//*[@id]")
  # IDs are generated identifiers and can change after import/export round-trips.
  xml2::xml_attr(nodesWithId, "id") <- "NORMALIZED_ID"
  xml2::as_list(doc)
}

.expectBuildingBlockRoundTrip <- function(loadFunction, saveFunction, filePath) {
  originalBuildingBlock <- loadFunction(filePath)
  firstExportPath <- tempfile(fileext = ".pkml")
  secondExportPath <- tempfile(fileext = ".pkml")
  on.exit(unlink(c(firstExportPath, secondExportPath), force = TRUE), add = TRUE)

  saveFunction(originalBuildingBlock, firstExportPath)
  exportedBuildingBlock <- loadFunction(firstExportPath)
  saveFunction(exportedBuildingBlock, secondExportPath)

  expect_equal(originalBuildingBlock$name, exportedBuildingBlock$name)
  expect_equal(
    .normalizePKMLForComparison(firstExportPath),
    .normalizePKMLForComparison(secondExportPath)
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
