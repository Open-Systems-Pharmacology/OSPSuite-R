# uniqueEntities

sim <- loadTestSimulation("S1")

test_that("It throws an error when no valid entities are provided", {
  expect_error(
    uniqueEntities("String"),
    messages$errorWrongType("entities", "list", "Entity"),
    fixed = TRUE
  )
})

test_that("It returns the entity for uniqueEntities with one passed entity", {
  parameter <- getParameter(
    toPathString(c("Organism", "Liver", "Pericentral", "Volume")),
    sim
  )
  expect_equal(uniqueEntities(parameter)[[1]], parameter)
})

test_that("It throws an error when no valid 'compareBy' is provided", {
  parameter <- getParameter(
    toPathString(c("Organism", "Liver", "Pericentral", "Volume")),
    sim
  )

  expect_error(
    suppressWarnings(uniqueEntities(parameter, compareBy = 2)),
    messages$errorValueNotInEnum(value = 2, CompareBy),
    fixed = TRUE
  )
  expect_error(
    suppressWarnings(uniqueEntities(parameter, compareBy = "2")),
    messages$errorValueNotInEnum(value = "2", CompareBy),
    fixed = TRUE
  )
})

test_that("It can filter by id", {
  parameters <- c(
    getParameter(
      toPathString(c("Organism", "Liver", "Pericentral", "Volume")),
      sim
    ),
    getParameter(
      toPathString(c("Organism", "Liver", "Pericentral", "Volume")),
      sim
    ),
    getParameter(
      toPathString(c("Organism", "Liver", "Pericentral", "Weight (tissue)")),
      sim
    )
  )
  expect_equal(length(uniqueEntities(parameters)), 2)
})

test_that("It can filter by name", {
  parameters <- c(
    getParameter(
      toPathString(c("Organism", "Liver", "Pericentral", "Volume")),
      sim
    ),
    getParameter(
      toPathString(c("Organism", "Liver", "Pericentral", "Volume")),
      sim
    ),
    getParameter(
      toPathString(c("Organism", "Liver", "Pericentral", "Weight (tissue)")),
      sim
    )
  )
  expect_equal(length(uniqueEntities(parameters, CompareBy$name)), 2)
})

test_that("It can filter by path", {
  parameters <- c(
    getParameter(
      toPathString(c("Organism", "Liver", "Pericentral", "Volume")),
      sim
    ),
    getParameter(
      toPathString(c("Organism", "Liver", "Pericentral", "Volume")),
      sim
    ),
    getParameter(
      toPathString(c("Organism", "Liver", "Pericentral", "Weight (tissue)")),
      sim
    )
  )
  expect_equal(length(uniqueEntities(parameters, CompareBy$path)), 2)
})

test_that("It throws an exception if comparing by a value that is not defined", {
  parameters <- c(
    getParameter(
      toPathString(c("Organism", "Liver", "Pericentral", "Volume")),
      sim
    ),
    getParameter(
      toPathString(c("Organism", "Liver", "Pericentral", "Volume")),
      sim
    ),
    getParameter(
      toPathString(c("Organism", "Liver", "Pericentral", "Weight (tissue)")),
      sim
    )
  )
  expect_error(
    uniqueEntities(parameters, "toto"),
    messages$errorValueNotInEnum(value = "toto", CompareBy),
    fixed = TRUE
  )
})
