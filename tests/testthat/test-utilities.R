context("netEnumName")
enumType <- "OSPSuite.Core.Domain.Data.AuxiliaryType"

test_that("It returns the correct name of an enum entry", {
  expect_equal(netEnumName(enumType = enumType, 1L), "ArithmeticStdDev")
})

test_that("It returns NULL if the entry does not exist", {
  expect_equal(netEnumName(enumType = enumType, 5L), NULL)
})
