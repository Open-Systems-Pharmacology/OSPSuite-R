test_that("It can print parameter range", {
  paramRange <- ParameterRange$new()
  expect_snapshot(paramRange$print())
})

test_that("It sets min, max and unit on creation", {
  paramRange <- ParameterRange$new(min = 10, max = 20, unit = "kg")
  expect_equal(paramRange$min, 10)
  expect_equal(paramRange$max, 20)
  expect_equal(paramRange$unit, "kg")
})

test_that("It rejects a non-numeric range or non-string unit", {
  expect_error(ParameterRange$new(min = "10"))
  expect_error(ParameterRange$new(max = "20"))
  expect_error(ParameterRange$new(unit = 5))
})

test_that("It prints a filled parameter range", {
  paramRange <- ParameterRange$new(min = 10, max = 20, unit = "kg")
  expect_snapshot(paramRange$print())
})

test_that("It returns a one-line print value", {
  expect_snapshot({
    ParameterRange$new(min = 10, max = 20, unit = "kg")$getPrintValue()
    ParameterRange$new(max = 20, unit = "kg")$getPrintValue()
    ParameterRange$new(min = 10, unit = "kg")$getPrintValue()
  })
})
