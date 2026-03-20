simulation <- loadSimulation(system.file(
  "extdata",
  "Aciclovir.pkml",
  package = "ospsuite"
))

icBB <- simulation$configuration$modules[[1]]$getInitialConditionsBBs()[[1]]

# Test initialConditionsToDataFrame with no paths
# Test initialConditionsToDataFrame with one path
# Test initialConditionsToDataFrame with multiple paths
test_that("initialConditionsToDataFrame returns a data frame with the expected columns", {
  df <- initialConditionsToDataFrame(icBB)
  expect_snapshot(df)
})

test_that("initialConditionsToDataFrame throws an error if the building block is not of type Initial Conditions", {
  # Get a PV BB to test validation
  pvBB <- simulation$configuration$modules[[1]]$getParameterValuesBBs()[[1]]
  expect_error(
    initialConditionsToDataFrame(pvBB),
    regexp = "Initial Conditions"
  )
})


# test for wrong type of building block
test_that("setInitialConditions throws an error if the building block is not of type Initial Conditions", {
  # Get a PV BB to test validation
  pvBB <- simulation$configuration$modules[[1]]$getParameterValuesBBs()[[1]]
  expect_error(
    setInitialConditions(pvBB, "Path", 1),
    regexp = "Initial Conditions"
  )
})

# test for differnt lengths of quantityPaths and quantityValues
test_that("setInitialConditions throws an error if quantityPaths and quantityValues have different lengths", {
  expect_error(
    setInitialConditions(
      icBB,
      quantityPaths = c("Path1", "Path2"),
      quantityValues = c(1)
    ),
    regexp = "The length of quantityPaths should be equal to the length "
  )
})


test_that("setInitialConditions updates one entry correctly", {
  # Create a copy or use the existing icBB from the simulation
  # Note: In these tests we assume the BB is modified in place or returned

  quantityPaths <- "Organism|Brain|Intracellular|Aciclovir"
  quantityValues <- 10

  # Set initial conditions
  setInitialConditions(
    icBB,
    quantityPaths = quantityPaths,
    quantityValues = quantityValues,
    scaleDivisors = 1,
    isPresent = TRUE,
    negativeValuesAllowed = FALSE
  )

  df <- initialConditionsToDataFrame(icBB)

  # Check if the paths exist in the data frame
  # The data frame columns are "Container Path" and "Molecule Name"
  brainEntry <- df[
    df$`Container Path` == "Organism|Brain|Intracellular" &
      df$`Molecule Name` == "Aciclovir",
  ]

  expect_equal(nrow(brainEntry), 1)
  expect_equal(brainEntry$Value, 10)
})

# It can change two entries of the same molecule in different compartments
test_that("setInitialConditions updates multiple entries correctly", {
  quantityPaths <- c(
    "Organism|Brain|Intracellular|Aciclovir",
    "Organism|Kidney|Intracellular|Aciclovir"
  )
  quantityValues <- c(10, 20)

  # Set initial conditions
  setInitialConditions(
    icBB,
    quantityPaths = quantityPaths,
    quantityValues = quantityValues,
    scaleDivisors = 2,
    isPresent = TRUE,
    negativeValuesAllowed = TRUE
  )

  df <- initialConditionsToDataFrame(icBB)

  brainEntry <- df[
    df$`Container Path` == "Organism|Brain|Intracellular" &
      df$`Molecule Name` == "Aciclovir",
  ]
  kidneyEntry <- df[
    df$`Container Path` == "Organism|Kidney|Intracellular" &
      df$`Molecule Name` == "Aciclovir",
  ]

  expect_equal(nrow(brainEntry), 1)
  expect_equal(nrow(kidneyEntry), 1)
  expect_equal(brainEntry$Value, 10)
  expect_equal(brainEntry$`Scale Divisor`, 2)
  expect_equal(brainEntry$`Is Present`, TRUE)
  expect_equal(brainEntry$`Neg. Values Allowed`, TRUE)
  expect_equal(kidneyEntry$Value, 20)
  expect_equal(kidneyEntry$`Scale Divisor`, 2)
  expect_equal(kidneyEntry$`Is Present`, TRUE)
  expect_equal(kidneyEntry$`Neg. Values Allowed`, TRUE)
})
