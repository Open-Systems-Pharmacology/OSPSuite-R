test_that("It can create a new ParameterRange object without a netObject", {
  paramRange <- ParameterRange$new()
})

test_that("It can print parameter range"; {
  paramRange <- ParameterRange$new()
  expect_snapshot(paramRange$print()
})
