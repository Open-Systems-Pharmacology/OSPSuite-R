context("quantity-type")

test_that("It returns the correct string names of the simple quantity types", {
  expect_equal(getEnumKey(QuantityType, bitwShiftL(2, 0)),  getQuantityTypeAsString(bitwShiftL(2, 0)))
  expect_equal(getEnumKey(QuantityType, bitwShiftL(2, 1)),  getQuantityTypeAsString(bitwShiftL(2, 1)))
  expect_equal(getEnumKey(QuantityType, bitwShiftL(2, 2)),  getQuantityTypeAsString(bitwShiftL(2, 2)))
  expect_equal(getEnumKey(QuantityType, bitwShiftL(2, 3)),  getQuantityTypeAsString(bitwShiftL(2, 3)))
  expect_equal(getEnumKey(QuantityType, bitwShiftL(2, 4)),  getQuantityTypeAsString(bitwShiftL(2, 4)))
  expect_equal(getEnumKey(QuantityType, bitwShiftL(2, 5)),  getQuantityTypeAsString(bitwShiftL(2, 5)))
  expect_equal(getEnumKey(QuantityType, bitwShiftL(2, 6)),  getQuantityTypeAsString(bitwShiftL(2, 6)))
  expect_equal(getEnumKey(QuantityType, bitwShiftL(2, 7)),  getQuantityTypeAsString(bitwShiftL(2, 7)))
  expect_equal(getEnumKey(QuantityType, bitwShiftL(2, 8)),  getQuantityTypeAsString(bitwShiftL(2, 8)))
  expect_equal(getEnumKey(QuantityType, bitwShiftL(2, 9)),  getQuantityTypeAsString(bitwShiftL(2, 9)))
})

test_that("It returns the correct string names of the copmlex quantity types", {
  #Drug, observer
  expect_equal(getQuantityTypeAsString(QuantityType$Drug + QuantityType$Observer), "Drug, Observer")
  #Undefined, Enzyme, Parameter
  expect_equal(getQuantityTypeAsString(QuantityType$Undefined + QuantityType$Enzyme + QuantityType$Parameter), "Undefined, Enzyme, Parameter")
})
