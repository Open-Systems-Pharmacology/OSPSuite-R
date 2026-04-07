# toPathArray

test_that("It should convert a valid path to array", {
  expect_identical(toPathArray("Organism|Liver"), c("Organism", "Liver"))
})

test_that("It should throw an error if the argument is not a string ", {
  expect_error(toPathArray(function() {}))
})


# toPathString

test_that("It should convert a valid path array to string", {
  expect_identical(toPathString("Organism", "Liver"), "Organism|Liver")
  expect_identical(toPathString(c("Organism", "Liver")), "Organism|Liver")
})

test_that("It should throw an error if the argument is not a string ", {
  expect_error(toPathString(function() {}))
})

# .getParentContainerPath

test_that("It returns the parent path for a path with multiple entries", {
  expect_identical(
    .getParentContainerPath("Organism|Organ|Liver"),
    "Organism|Organ"
  )
  expect_identical(.getParentContainerPath("Organism|Liver"), "Organism")
})

test_that("It returns NULL if the path has no parent (single entry)", {
  expect_null(.getParentContainerPath("Organism"))
})

test_that("It throws an error if the path is not a string", {
  expect_error(.getParentContainerPath(123))
  expect_error(.getParentContainerPath(NULL))
})
