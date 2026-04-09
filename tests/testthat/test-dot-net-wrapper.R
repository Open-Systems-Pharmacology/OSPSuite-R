#  DotNetWrapper

sim <- loadTestSimulation(
  "simple",
  loadFromCache = TRUE,
  addToCache = TRUE
)

test_that("It can retrieve the pointer for an object loaded", {
  expect_false(is.null(sim$pointer))
})

test_that("It throws an error when trying to overwrite the pointer", {
  expect_error(
    sim$pointer <- sim$pointer,
    regexp = "Property 'pointer' is read-only"
  )
})
