test_that("It can print parameter range", {
  paramRange <- ParameterRange$new()
  expect_snapshot(paramRange$print())
})
