context("DotNetWrapper")

sim <- loadTestSimulation("S1")
sim2 <- loadTestSimulation("S1", loadFromCache = FALSE)

test_that("It can retrieve the reference for an object loaded", {
  expect_false(is.null(sim$ref))
})

test_that("It throws an error when trying to overwrite the ref value", {
  expect_error(sim$ref <- sim2$ref)
})
