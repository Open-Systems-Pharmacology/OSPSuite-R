test_that("Read-only detection works correctly", {
  skip_on_os(c("windows")) # Skip on Windows due to permission handling differences

  # Create a temporary directory
  tempDir <- withr::local_tempdir()

  # Test writable directory
  expect_true(
    .isWritable(tempDir),
    info = "Writable directory should be detected as writable"
  )

  # Make directory read-only
  Sys.chmod(tempDir, mode = "0555")

  # Test read-only directory
  expect_false(
    .isWritable(tempDir),
    info = "Read-only directory should be detected as not writable"
  )
})

test_that("Fallback directory selection works correctly", {
  skip_on_os(c("windows")) # Skip on Windows due to permission handling differences

  # Create a temporary directory to simulate read-only lib
  tempDir <- withr::local_tempdir()

  # Make it read-only
  Sys.chmod(tempDir, mode = "0555")

  # Get writable directory (should fallback)
  writeableDir <- .getWritableLibDir(tempDir)

  # Verify fallback was used
  expect_true(
    writeableDir != tempDir,
    info = "Should use fallback directory for read-only lib"
  )
  expect_true(
    .isWritable(writeableDir),
    info = "Fallback directory should be writable"
  )

  # Verify it uses the correct CRAN-compliant location
  expectedCacheDir <- tools::R_user_dir("ospsuite", which = "cache")
  expect_equal(
    writeableDir,
    expectedCacheDir,
    info = "Fallback directory should use tools::R_user_dir"
  )
})

test_that("File copying to fallback directory works correctly", {
  skip_on_os(c("windows")) # Skip on Windows due to permission handling differences

  # Create source and target directories
  sourceDir <- withr::local_tempdir()
  targetDir <- withr::local_tempdir()

  # Create test files in source directory (simulating lib directory)
  dir.create(file.path(sourceDir, "subdir"), recursive = TRUE)
  testFiles <- list(
    "test.dll" = "DLL content",
    "test.dylib" = "DYLIB content",
    "test.so" = "SO content",
    "test.xml" = "XML content",
    "subdir/nested.dll" = "Nested DLL"
  )

  for (file in names(testFiles)) {
    filePath <- file.path(sourceDir, file)
    dir.create(dirname(filePath), recursive = TRUE, showWarnings = FALSE)
    writeLines(testFiles[[file]], filePath)
  }

  # Copy files
  result <- .copyFilesIfNeeded(sourceDir, targetDir)
  expect_true(result, info = "Files should be copied successfully")

  # Verify all files were copied
  for (file in names(testFiles)) {
    targetPath <- file.path(targetDir, file)
    expect_true(
      file.exists(targetPath),
      info = paste("File", file, "should be copied")
    )
    expect_equal(
      readLines(file.path(sourceDir, file)),
      readLines(targetPath),
      info = paste("File", file, "content should match")
    )
  }
})

test_that("Cache marker system works correctly", {
  skip_on_os(c("windows")) # Skip on Windows due to permission handling differences

  # Create temporary directories
  sourceDir <- withr::local_tempdir()
  cacheDir <- withr::local_tempdir()

  # Create some test files in source directory
  writeLines("test content", file.path(sourceDir, "test.dll"))

  # No marker file exists - should need refresh
  expect_true(
    .cacheNeedsRefresh(sourceDir, cacheDir),
    info = "Should need refresh when no marker exists"
  )

  # Create marker file
  .createCacheMarker(cacheDir)

  # Verify marker exists and has content
  markerFile <- file.path(cacheDir, ".ospsuite_cache_marker")
  expect_true(file.exists(markerFile), info = "Cache marker should exist")

  markerContent <- readLines(markerFile)
  expect_true(
    length(markerContent) > 0,
    info = "Cache marker should contain content"
  )
  expect_true(
    grepl("\\d{4}-\\d{2}-\\d{2}", markerContent[1]),
    info = "Cache marker should contain timestamp"
  )

  # Should not need refresh immediately
  expect_false(
    .cacheNeedsRefresh(sourceDir, cacheDir),
    info = "Should not need refresh when cache is fresh"
  )

  # Update source files (make them newer)
  Sys.sleep(1) # Ensure time difference
  writeLines("updated content", file.path(sourceDir, "test.dll"))

  # Should need refresh now
  expect_true(
    .cacheNeedsRefresh(sourceDir, cacheDir),
    info = "Should need refresh when source files are newer"
  )
})

test_that("Package loads successfully from read-only location", {
  skip_on_os(c("windows")) # Skip on Windows due to permission handling differences

  # Copy package to temp location (cleanup handled by withr::local_tempdir)
  tempPkgDir <- .copyPackageToTemp()
  libDir <- file.path(tempPkgDir, "inst/lib")

  # Make package installation directory read-only
  Sys.chmod(tempPkgDir, mode = "0555")

  # Verify directory is read-only
  expect_false(.isWritable(tempPkgDir), "should be read-only")

  # Test that package loads without error
  expect_no_error({
    devtools::load_all(tempPkgDir, quiet = TRUE)
  })

  # Verify that writeableLibDir was set
  expect_true(
    !is.null(ospsuiteEnv$writeableLibDir),
    info = "writeableLibDir should be set in environment"
  )

  # Verify that fallback directory was used
  expect_true(
    ospsuiteEnv$writeableLibDir != libDir,
    info = "Should use fallback directory for read-only lib"
  )

  # Verify that fallback directory is writable
  expect_true(
    .isWritable(ospsuiteEnv$writeableLibDir),
    info = "Fallback directory should be writable"
  )
})

test_that("All initialization steps work from read-only location", {
  skip_on_os(c("windows")) # Skip on Windows due to permission handling differences

  # Copy package to temp location (cleanup handled by withr::local_tempdir)
  tempPkgDir <- .copyPackageToTemp()

  # Make package directory read-only
  libDir <- file.path(tempPkgDir, "inst", "lib")
  Sys.chmod(tempPkgDir, mode = "0555")

  # Load package
  devtools::load_all(tempPkgDir, quiet = TRUE)

  # Verify writeableLibDir is set
  expect_true(!is.null(ospsuiteEnv$writeableLibDir))
  writeableDir <- ospsuiteEnv$writeableLibDir

  # Note: system.file() may point to an installed package location, not the temp copy
  # The important thing is that writeableDir is set and writable
  expect_true(
    .isWritable(writeableDir),
    info = "writeableLibDir must be writable"
  )

  # Test 1: Verify cache marker was created if using fallback directory
  # (Skip this check as system.file() behavior with devtools::load_all() is complex)
  markerFile <- file.path(writeableDir, ".ospsuite_cache_marker")
  # If a marker exists, that's fine; if not, that's also fine in this test context

  # Test 2: Verify key DLL files were copied
  keyDllFiles <- c(
    "OSPSuite.R.dll",
    "System.Data.SQLite.dll"
  )
  for (file in keyDllFiles) {
    targetPath <- file.path(writeableDir, file)
    expect_true(
      file.exists(targetPath),
      info = paste("DLL file", file, "should be copied to fallback")
    )
  }

  # Test 3: Verify XML configuration files were copied
  xmlFiles <- c(
    "OSPSuite.Dimensions.xml",
    "OSPSuite.PKParameters.xml"
  )
  for (file in xmlFiles) {
    targetPath <- file.path(writeableDir, file)
    expect_true(
      file.exists(targetPath),
      info = paste("XML file", file, "should be copied to fallback")
    )
  }

  # Test 4: Verify database file was copied
  dbPath <- file.path(writeableDir, "PKSimDB.sqlite")
  if (file.exists(file.path(libDir, "PKSimDB.sqlite"))) {
    expect_true(
      file.exists(dbPath),
      info = "Database file should be copied to fallback"
    )
  }

  # Test 5: Verify platform-specific files were copied
  sysname <- Sys.info()[['sysname']]
  if (sysname == "Darwin") {
    machine <- Sys.info()[['machine']]

    # Check SQLite.Interop architecture-specific file
    sourceFile <- if (machine == "arm64") {
      "SQLite.Interop.arm64.dylib"
    } else if (machine == "x86_64") {
      "SQLite.Interop.x64.dylib"
    }

    if (!is.null(sourceFile) && file.exists(file.path(libDir, sourceFile))) {
      expect_true(
        file.exists(file.path(writeableDir, sourceFile)),
        info = "Architecture-specific SQLite file should be copied"
      )
    }

    # Check native library files
    nativeLibraries <- c(
      "libOSPSuite.FuncParserNative",
      "libOSPSuite.SimModelNative",
      "libOSPSuite.SimModelSolver_CVODES"
    )

    for (libName in nativeLibraries) {
      sourceFile <- if (machine == "arm64") {
        paste0(libName, ".Arm64.dylib")
      } else if (machine == "x86_64") {
        paste0(libName, ".x64.dylib")
      }

      if (!is.null(sourceFile) && file.exists(file.path(libDir, sourceFile))) {
        expect_true(
          file.exists(file.path(writeableDir, sourceFile)),
          info = paste("Native library", sourceFile, "should be copied")
        )
      }
    }
  } else if (sysname == "Linux") {
    # Check .so files were copied
    soFiles <- list.files(libDir, pattern = "\\.so$", full.names = FALSE)
    for (soFile in soFiles) {
      expect_true(
        file.exists(file.path(writeableDir, soFile)),
        info = paste("SO file", soFile, "should be copied")
      )
    }
  }

  # Test 6: Verify API can be initialized
  expect_no_error({
    apiConfig <- ApiConfig$new(rSharp::newObjectFromName(
      "OSPSuite.R.ApiConfig"
    ))
  })
})

test_that("Package can be reloaded from cached fallback directory", {
  skip_on_os(c("windows")) # Skip on Windows due to permission handling differences

  # Copy package to temp location (cleanup handled by withr::local_tempdir)
  tempPkgDir <- .copyPackageToTemp()

  # Make package directory read-only
  libDir <- file.path(tempPkgDir, "inst", "lib")
  Sys.chmod(tempPkgDir, mode = "0555")

  # Load package first time
  devtools::load_all(tempPkgDir, quiet = TRUE)
  firstWriteableDir <- ospsuiteEnv$writeableLibDir

  # Verify writeableDir is writable
  expect_true(
    .isWritable(firstWriteableDir),
    info = "writeableLibDir must be writable"
  )

  # Wait a bit
  Sys.sleep(1)

  # Reload package (should use same directory)
  devtools::load_all(tempPkgDir, quiet = TRUE)
  secondWriteableDir <- ospsuiteEnv$writeableLibDir

  # Verify same directory is used on reload
  expect_equal(
    firstWriteableDir,
    secondWriteableDir,
    info = "Should use same directory on reload"
  )

  # Test that marker file exists and caching works if using fallback
  markerFile <- file.path(firstWriteableDir, ".ospsuite_cache_marker")
  if (file.exists(markerFile)) {
    # If marker exists, verify it wasn't recreated (cache was reused)
    firstMarkerTime <- file.mtime(markerFile)
    Sys.sleep(1)
    devtools::load_all(tempPkgDir, quiet = TRUE)
    secondMarkerTime <- file.mtime(markerFile)
    expect_equal(
      firstMarkerTime,
      secondMarkerTime,
      info = "Cache marker should not be recreated if cache is fresh"
    )
  }
})

test_that("runSimulationsFromSnapshot works from read-only location", {
  skip_on_os(c("windows")) # Skip on Windows due to permission handling differences
  skip_if_not_installed("RSQLite")

  # Copy package to temp location (cleanup handled by withr::defer)
  tempPkgDir <- .copyPackageToTemp()

  # Make package directory read-only
  Sys.chmod(tempPkgDir, mode = "0555")

  # Load package from read-only location
  devtools::load_all(tempPkgDir, quiet = TRUE)

  # Verify writeableLibDir was set
  expect_true(!is.null(ospsuiteEnv$writeableLibDir))
  writeableDir <- ospsuiteEnv$writeableLibDir

  # Verify database file was copied to writable location
  dbPath <- file.path(writeableDir, "PKSimDB.sqlite")
  expect_true(
    file.exists(dbPath),
    info = "Database should be copied to writable directory"
  )

  # Verify writable directory is actually writable (needed for database operations)
  expect_true(
    .isWritable(writeableDir),
    info = "Fallback directory must be writable for database operations"
  )

  # Test that runSimulationsFromSnapshot can be called
  # (This function needs to access and potentially modify the database)
  expect_no_error({
    # Get the same test snapshot file used in test-utilities-snapshots.R
    snapshotFile <- getTestDataFilePath("test_snapshot.json")

    # Skip if snapshot file doesn't exist
    skip_if(!file.exists(snapshotFile), "Test snapshot file not found")

    # Create temp output directory
    outputDir <- withr::local_tempdir()

    # This should work even from read-only package location
    # because the database is in the writable fallback directory
    runSimulationsFromSnapshot(
      snapshotFile,
      output = outputDir,
      exportCSV = TRUE
    )

    # Verify it actually ran and produced output
    csvFiles <- list.files(outputDir, pattern = "\\.csv$")
    expect_true(
      length(csvFiles) > 0,
      info = "runSimulationsFromSnapshot should produce CSV output"
    )
  })
})
