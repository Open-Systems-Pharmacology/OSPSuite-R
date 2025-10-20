test_that("macOS SQLite fix converts VIEWs to TABLEs correctly", {
  skip_on_os(c("windows", "linux"))
  
  # Get path to the database
  libDir <- system.file("lib", package = "ospsuite")
  dbPath <- file.path(libDir, "PKSimDB.sqlite")
  markerPath <- paste0(dbPath, ".macos-fixed")
  
  # Check that marker exists (fix was applied)
  expect_true(file.exists(markerPath))
  
  # Connect to database
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbPath)
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)
  
  # Verify ContainerParameters_Species is a TABLE
  result <- RSQLite::dbGetQuery(
    con,
    "SELECT type FROM sqlite_master WHERE name = 'ContainerParameters_Species'"
  )
  expect_equal(result$type[1], "table")
  
  # Verify VIEW_INDIVIDUAL_PARAMETER_SAME_FORMULA_OR_VALUE_FOR_ALL_SPECIES is a TABLE
  result <- RSQLite::dbGetQuery(
    con,
    "SELECT type FROM sqlite_master WHERE name = 'VIEW_INDIVIDUAL_PARAMETER_SAME_FORMULA_OR_VALUE_FOR_ALL_SPECIES'"
  )
  expect_equal(result$type[1], "table")
  
  # Verify ContainerParameters_Species has data and correct structure
  containerParams <- RSQLite::dbGetQuery(
    con,
    "SELECT * FROM ContainerParameters_Species LIMIT 1"
  )
  expect_true(nrow(containerParams) > 0)
  expect_true(all(c("container_id", "container_type", "container_name", "parameter_name", "species") %in% names(containerParams)))
  
  # Verify VIEW_INDIVIDUAL_PARAMETER_SAME_FORMULA_OR_VALUE_FOR_ALL_SPECIES has data and correct structure
  viewParams <- RSQLite::dbGetQuery(
    con,
    "SELECT * FROM VIEW_INDIVIDUAL_PARAMETER_SAME_FORMULA_OR_VALUE_FOR_ALL_SPECIES LIMIT 1"
  )
  expect_true(nrow(viewParams) > 0)
  expect_true(all(c("ContainerId", "ContainerType", "ContainerName", "ParameterName", "IsSameFormula") %in% names(viewParams)))
  
  # Verify indexes were created
  indexes <- RSQLite::dbGetQuery(
    con,
    "SELECT name FROM sqlite_master WHERE type = 'index' AND (name = 'idx_container_params_species' OR name = 'idx_same_formula_species')"
  )
  expect_equal(nrow(indexes), 2)
})

test_that("macOS SQLite fix produces consistent row counts", {
  skip_on_os(c("windows", "linux"))
  
  # Get path to the database
  libDir <- system.file("lib", package = "ospsuite")
  dbPath <- file.path(libDir, "PKSimDB.sqlite")
  
  # Connect to database
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbPath)
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)
  
  # Count rows in ContainerParameters_Species
  count1 <- RSQLite::dbGetQuery(
    con,
    "SELECT COUNT(*) as n FROM ContainerParameters_Species"
  )
  expect_true(count1$n > 0)
  
  # Verify no duplicates in ContainerParameters_Species
  distinctCount1 <- RSQLite::dbGetQuery(
    con,
    "SELECT COUNT(*) as n FROM (
      SELECT DISTINCT container_id, container_type, container_name, parameter_name, species
      FROM ContainerParameters_Species
    )"
  )
  expect_equal(count1$n, distinctCount1$n, 
               label = "ContainerParameters_Species should have no duplicates")
  
  # Count rows in VIEW_INDIVIDUAL_PARAMETER_SAME_FORMULA_OR_VALUE_FOR_ALL_SPECIES
  count2 <- RSQLite::dbGetQuery(
    con,
    "SELECT COUNT(*) as n FROM VIEW_INDIVIDUAL_PARAMETER_SAME_FORMULA_OR_VALUE_FOR_ALL_SPECIES"
  )
  expect_true(count2$n > 0)
  
  # Verify no duplicates in VIEW_INDIVIDUAL_PARAMETER_SAME_FORMULA_OR_VALUE_FOR_ALL_SPECIES
  distinctCount2 <- RSQLite::dbGetQuery(
    con,
    "SELECT COUNT(*) as n FROM (
      SELECT DISTINCT ContainerId, ContainerType, ContainerName, ParameterName, IsSameFormula
      FROM VIEW_INDIVIDUAL_PARAMETER_SAME_FORMULA_OR_VALUE_FOR_ALL_SPECIES
    )"
  )
  expect_equal(count2$n, distinctCount2$n,
               label = "VIEW_INDIVIDUAL_PARAMETER_SAME_FORMULA_OR_VALUE_FOR_ALL_SPECIES should have no duplicates")
})

test_that("macOS SQLite fix marker is valid", {
  skip_on_os(c("windows", "linux"))
  
  # Get path to the database and marker
  libDir <- system.file("lib", package = "ospsuite")
  dbPath <- file.path(libDir, "PKSimDB.sqlite")
  markerPath <- paste0(dbPath, ".macos-fixed")
  
  # Check marker exists
  expect_true(file.exists(markerPath))
  
  # Check marker content
  markerContent <- readLines(markerPath)
  expect_true(any(grepl("macOS SQLite fix has been applied", markerContent)))
  expect_true(any(grepl("Applied on:|Marker created on:", markerContent)))
  
  # Verify marker is not older than database
  markerTime <- file.info(markerPath)$mtime
  dbTime <- file.info(dbPath)$mtime
  expect_true(markerTime >= dbTime, 
              label = "Marker should be created after or at the same time as database modification")
})

test_that("macOS SQLite TABLE conversion produces same results as original VIEWs", {
  skip_on_os(c("windows", "linux"))
  
  # Get path to databases
  libDir <- system.file("lib", package = "ospsuite")
  fixedPath <- file.path(libDir, "PKSimDB.sqlite")
  originalPath <- paste0(fixedPath, ".original")
  
  # Skip if backup doesn't exist
  skip_if_not(file.exists(originalPath), "Original database backup not found")
  
  # Connect to both databases
  conFixed <- RSQLite::dbConnect(RSQLite::SQLite(), fixedPath)
  conOriginal <- RSQLite::dbConnect(RSQLite::SQLite(), originalPath)
  on.exit({
    RSQLite::dbDisconnect(conFixed)
    RSQLite::dbDisconnect(conOriginal)
  }, add = TRUE)
  
  # Test 1: Compare ContainerParameters_Species (TABLE vs VIEW)
  fixedData1 <- RSQLite::dbGetQuery(
    conFixed,
    "SELECT * FROM ContainerParameters_Species ORDER BY container_id, parameter_name, species"
  )
  
  originalData1 <- RSQLite::dbGetQuery(
    conOriginal,
    "SELECT * FROM ContainerParameters_Species ORDER BY container_id, parameter_name, species"
  )
  
  expect_equal(nrow(fixedData1), nrow(originalData1),
               label = "ContainerParameters_Species: Row counts should match")
  expect_equal(fixedData1, originalData1,
               label = "ContainerParameters_Species: TABLE should produce same results as VIEW")
  
  # Test 2: Compare VIEW_INDIVIDUAL_PARAMETER_SAME_FORMULA_OR_VALUE_FOR_ALL_SPECIES (TABLE vs VIEW)
  fixedData2 <- RSQLite::dbGetQuery(
    conFixed,
    "SELECT * FROM VIEW_INDIVIDUAL_PARAMETER_SAME_FORMULA_OR_VALUE_FOR_ALL_SPECIES 
     ORDER BY ContainerId, ParameterName, IsSameFormula"
  )
  
  originalData2 <- RSQLite::dbGetQuery(
    conOriginal,
    "SELECT * FROM VIEW_INDIVIDUAL_PARAMETER_SAME_FORMULA_OR_VALUE_FOR_ALL_SPECIES 
     ORDER BY ContainerId, ParameterName, IsSameFormula"
  )
  
  expect_equal(nrow(fixedData2), nrow(originalData2),
               label = "VIEW_INDIVIDUAL_PARAMETER_SAME_FORMULA_OR_VALUE_FOR_ALL_SPECIES: Row counts should match")
  expect_equal(fixedData2, originalData2,
               label = "VIEW_INDIVIDUAL_PARAMETER_SAME_FORMULA_OR_VALUE_FOR_ALL_SPECIES: TABLE should produce same results as VIEW")
})

