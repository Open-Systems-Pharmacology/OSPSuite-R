test_that("It can print individual charachteristics", {
  indivCharacteristics <- IndividualCharacteristics$new()
  expect_snapshot(print(indivCharacteristics))
})
