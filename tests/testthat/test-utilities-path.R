# toPathArray

test_that("It should convert a valid path to array", {
  expect_identical(toPathArray("Organism|Liver"), c("Organism", "Liver"))
})

test_that("It should throw an error if the argument is not a string ", {
  expect_error(toPathArray(function() {
  }))
})


# toPathString

test_that("It should convert a valid path array to string", {
  expect_identical(toPathString("Organism", "Liver"), "Organism|Liver")
  expect_identical(toPathString(c("Organism", "Liver")), "Organism|Liver")
})

test_that("It should throw an error if the argument is not a string ", {
  expect_error(toPathString(function() {
  }))
})
