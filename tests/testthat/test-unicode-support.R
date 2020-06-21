
context("Unciode support")

test_that("It can load a simulation whose name contains unicode characters", {
  sim <- loadTestSimulation("unicoße")
  expect_false(is.null(sim))

  param <- getParameter("Organism|Liver|paré", sim)
  expect_false(is.null(param))
})
