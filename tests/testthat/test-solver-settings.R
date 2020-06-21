context("SolverSettings")

sim <- loadTestSimulation("S1")
solver <- sim$solver
test_that("It can retrieve the basic solver parameters", {
  expect_true(solver$useJacobian)
  expect_equal(solver$h0, 1e-10)
  expect_equal(solver$hMin, 0)
  expect_equal(solver$hMax, 60)
  expect_equal(solver$mxStep, 100000)
  expect_equal(solver$relTol, 1e-4)
  expect_equal(solver$absTol, 1e-8)
})

test_that("It can set the basic solver parameters", {
  solver$useJacobian <- FALSE
  expect_false(solver$useJacobian)
  solver$h0 <- 1e-8
  expect_equal(solver$h0, 1e-8)

  solver$hMin <- 10
  expect_equal(solver$hMin, 10)

  solver$hMax <- 100
  expect_equal(solver$hMax, 100)

  solver$mxStep <- 5000
  expect_equal(solver$mxStep, 5000)

  solver$relTol <- 1e-5
  expect_equal(solver$relTol, 1e-5)

  solver$absTol <- 1e-10
  expect_equal(solver$absTol, 1e-10)
})

test_that("It can set the  solver parameters from the simulation", {
  sim$solver$h0 <- 1e-6
  expect_equal(solver$h0, 1e-6)
  expect_equal(sim$solver$h0, 1e-6)
})

context("SolverSettings$print")

test_that("It can print solver settings", {
  expect_error(capture.output(solver$print()), NA)
})
