aciclovirSimulationPath <- system.file(
  "extdata",
  "Aciclovir.pkml",
  package = "ospsuite"
)

getTestDataFilePath <- function(fileName) {
  dataPath <- testthat::test_path("../data")
  file.path(dataPath, fileName, fsep = .Platform$file.sep)
}

globalTestMoBiProject <- loadMoBiProject(
  filePath = getTestDataFilePath("MoBiProject/Test_Project.mbp3")
)

getSimulationFilePath <- function(simulationName) {
  getTestDataFilePath(paste0(simulationName, ".pkml"))
}

# No PK-Sim project file is stored in the repository. Tests that need one get it
# from the snapshot in `inst/extdata`, which is the maintained form of that
# project (see `data-raw/README.md`). The conversion is slow and the tests only
# read the file, so it happens on first use and the path is reused afterwards.
# The directory lives in the test process and R removes it on exit.
testProjectPath <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      outputDir <- file.path(tempdir(), "test-project")
      dir.create(outputDir, showWarnings = FALSE)
      snapshotToProject(
        system.file("extdata", "test_snapshot.json", package = "ospsuite"),
        output = outputDir
      )
      cached <<- list.files(
        outputDir,
        pattern = "\\.pksim5$",
        full.names = TRUE
      )[[1]]
    }
    cached
  }
})

# Helper function to load a model easily. In the test environment, we do not
# want to load from cache by default. Instead new instances should be created
# unless specifically specified otherwise
loadTestSimulation <- function(
  simulationName,
  loadFromCache = FALSE,
  addToCache = TRUE
) {
  simFile <- getSimulationFilePath(simulationName)
  loadSimulation(
    simFile,
    loadFromCache = loadFromCache,
    addToCache = addToCache
  )
}

executeWithTestFile <- function(actionWithFile) {
  newFile <- tempfile()
  actionWithFile(newFile)
  file.remove(newFile)
}

extractPlotTextLabels <- function(plotObject) {
  grobTable <- ggplot2::ggplotGrob(plotObject)
  labelRows <- list()

  collectLabels <- function(node) {
    if (!is.null(node$label) && is.character(node$label)) {
      labelColor <- NA_character_
      if (!is.null(node$gp) && !is.null(node$gp$col)) {
        labelColor <- as.character(node$gp$col)
      }

      labelRows[[length(labelRows) + 1]] <<- data.frame(
        label = node$label,
        color = labelColor,
        stringsAsFactors = FALSE
      )
    }

    if (!is.null(node$children)) {
      for (child in node$children) {
        collectLabels(child)
      }
    }

    if (!is.null(node$grobs)) {
      for (child in node$grobs) {
        collectLabels(child)
      }
    }
  }

  collectLabels(grobTable)

  if (length(labelRows) == 0) {
    return(data.frame(
      label = character(),
      color = character(),
      stringsAsFactors = FALSE
    ))
  }

  unique(do.call(rbind, labelRows))
}

# Checks that a plotting wrapper passes `watermark` and `watermarkFormat` on to
# `{ospsuite.plots}`. `plotFunction` must forward `...` to the wrapper.
expectWatermarkPassthrough <- function(plotFunction) {
  withr::local_options(list(
    ospsuite.plots.watermarkEnabled = TRUE,
    ospsuite.plots.watermarkLabel = "GLOBAL WATERMARK"
  ))

  customWatermarkLabels <- extractPlotTextLabels(
    plotFunction(watermark = "DRAFT")
  )
  expect_true(any(customWatermarkLabels$label == "DRAFT"))
  expect_false(any(customWatermarkLabels$label == "GLOBAL WATERMARK"))

  # `watermark = FALSE` must give the same text as a plot with no watermark
  noWatermarkLabels <- extractPlotTextLabels(plotFunction(watermark = FALSE))
  controlLabels <- withr::with_options(
    list(ospsuite.plots.watermarkEnabled = FALSE),
    extractPlotTextLabels(plotFunction())
  )
  expect_setequal(noWatermarkLabels$label, controlLabels$label)
  expect_false(any(noWatermarkLabels$label == "GLOBAL WATERMARK"))

  defaultWatermarkLabels <- extractPlotTextLabels(plotFunction())
  expect_true(any(defaultWatermarkLabels$label == "GLOBAL WATERMARK"))

  redWatermarkLabels <- extractPlotTextLabels(
    plotFunction(watermark = "DRAFT", watermarkFormat = list(color = "red"))
  )
  draftColors <- tolower(redWatermarkLabels$color[
    redWatermarkLabels$label == "DRAFT"
  ])
  expect_true(any(draftColors %in% c("red", "#ff0000", "#ff0000ff")))
}
