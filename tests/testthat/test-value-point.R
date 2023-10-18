# ValuePoint$print

simple <- loadTestSimulation("simple")
tableParameter <- getParameter("Organism|TableParameter", simple)
tableFormula <- tableParameter$formula
point <- tableFormula$allPoints[[1]]

test_that("It can print the point", {
  expect_error(capture.output(point$print()), NA)
})
