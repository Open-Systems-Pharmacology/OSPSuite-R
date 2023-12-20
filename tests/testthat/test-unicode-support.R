# Unicode support

test_that("It can load a simulation whose name contains unicode characters", {
  # The unicoße file is created on the fly for this file impact other aspects of the package like installation from github on non utf8 systems.

  file.copy(
    from = getTestDataFilePath("unicode.pkml"),
    to = getTestDataFilePath("unicoße.pkml")
  )

  sim <- loadTestSimulation("unicoße")
  expect_false(is.null(sim))

  param <- getParameter("Organism|Liver|paré", sim)
  expect_false(is.null(param))

  file.remove(getTestDataFilePath("unicoße.pkml"))
})
