context("quantity-type")

test_that("It returns the correct string names of the simple quantity types", {
  expect_equal(getEnumKey(QuantityType,QuantityType$Drug), getQuantityTypeAsString(QuantityType$Drug))
  expect_equal(getEnumKey(QuantityType,QuantityType$Metabolite), getQuantityTypeAsString(QuantityType$Metabolite))
  expect_equal(getEnumKey(QuantityType,QuantityType$Complex), getQuantityTypeAsString(QuantityType$Complex))
  expect_equal(getEnumKey(QuantityType,QuantityType$Observer), getQuantityTypeAsString(QuantityType$Observer))
})

test_that("It returns the correct string names of the copmlex quantity types", {
  # Drug, observer
  expect_equal(getQuantityTypeAsString(QuantityType$Drug + QuantityType$Observer), "Drug, Observer")
  # Undefined, Enzyme, Parameter
  expect_equal(getQuantityTypeAsString(QuantityType$Undefined + QuantityType$Enzyme + QuantityType$Parameter), "Undefined, Enzyme, Parameter")
})
