# Quantity

sim <- loadTestSimulation("simple", loadFromCache = FALSE, addToCache = FALSE)
quantity <- getQuantity(
  toPathString(c("Organism", "Liver", "Volume")),
  sim
)

test_that("It can print Quantity", {
  expect_snapshot(quantity$print())
})

test_that("It can retrieve the parent of an entity", {
  parent <- quantity$parentContainer
  expect_false(is.null(parent))
  expect_equal(parent$name, "Liver")
  expect_equal(parent$parentContainer$name, "Organism")
})


test_that("It prints the Scientific value of the Quantity", {
  quantity$value <- 0.001
  expect_snapshot(print(quantity))
  quantity$value <- 2
  expect_snapshot(print(quantity))
  quantity$value <- 10000
  expect_snapshot(print(quantity))
  quantity$value <- 10001.1
  expect_snapshot(print(quantity))
})

test_that("It can access valueOrigin property", {
  valueOrigin <- quantity$valueOrigin
  expect_true(is.character(valueOrigin))
})

test_that("valueOrigin property is read-only", {
  expect_error(quantity$valueOrigin <- "new value", regexp = "read.?only")
})

test_that("It prints the NaN value of the Quantity", {
  quantity <- getQuantity("A|Molecular weight", sim)
  expect_snapshot(quantity$print())
})
