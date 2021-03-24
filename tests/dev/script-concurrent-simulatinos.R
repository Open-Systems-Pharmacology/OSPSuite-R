library(ospsuite)

sourceAll <- function(folderPath, recursive = FALSE) {
  filesPaths <- list.files(folderPath, recursive = recursive)

  sourceFile <- function(filePath) {
    if (toupper(tools::file_ext(filePath)) == "R") {
      source(filePath, encoding = "UTF-8")
    }
    invisible()
  }

  invisible(lapply(file.path(folderPath, filesPaths), sourceFile))
}


source('./tests/testthat/helper-for-tests.R', echo=FALSE)
sourceAll(file.path("R"))
setwd("./tests/testthat")


resetSimulationCache()
sim <- loadTestSimulation("S1", loadFromCache = FALSE)
sim2 <- loadTestSimulation("S1", loadFromCache = FALSE)
results <- runSimulationsConcurrently(c(sim, sim2))
