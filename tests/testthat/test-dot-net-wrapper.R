#  DotNetWrapper

sim <- loadTestSimulation("S1")
sim2 <- loadTestSimulation("S1", loadFromCache = FALSE)

test_that("It can retrieve the pointer for an object loaded", {
  expect_false(is.null(sim$pointer))
})

test_that("It throws an error when trying to overwrite the pointer", {
  expectPropertyReadOnly(sim, "ref", sim2$pointer)
})
