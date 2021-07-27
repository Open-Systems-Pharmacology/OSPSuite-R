context("netEnumName")

test_that("It returns the correct name of an enum entry", {
  enumType <- "OSPSuite.Core.Domain.Data.AuxiliaryType"
  expect_equal(netEnumName(enumType = enumType, 1L), "ArithmeticStdDev")
})

test_that("It returns NULL if the entry does not exist", {
  enumType <- "OSPSuite.Core.Domain.Data.AuxiliaryType"
  expect_equal(netEnumName(enumType = enumType, 5L), NULL)
})
