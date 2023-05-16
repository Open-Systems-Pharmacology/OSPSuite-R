context("Unicode support")

test_that("It can load a simulation whose name contains unicode characters", {
  skip_on_os("linux")
  # in principle, this test runs under Linux. The problem is more in transfering files containing unicode chars in the file name
  # between Windows and Linux. Thus e.g. when installing the ospsuite-r package under Linux, the file name of "unicoße.pkml"
  # is already corrupted - so no chance for ospsuite-r to load it properly
  # The same happens if e.g. the `{ospsuite}` sources are just zipped under Windows and unzipped under Linux - the file name becomes corrupted

  # The unicoße file is created on the fly for this file impact other aspects of the package like installation from github on non utf8 systems.

  file.copy(from = getTestDataFilePath("unicode.pkml"),
            to = getTestDataFilePath("unicoße.pkml"))

  sim <- loadTestSimulation("unicoße")
  expect_false(is.null(sim))

  param <- getParameter("Organism|Liver|paré", sim)
  expect_false(is.null(param))

  file.remove(getTestDataFilePath("unicoße.pkml"))
})
