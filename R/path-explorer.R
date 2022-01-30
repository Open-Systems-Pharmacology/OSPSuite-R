
addBranch <- function(originalPathString, arrayToGo) {
  # Function to create a multilayered list called endList with a branched structure corresponding to the structure of arrayToGo that terminates with a string called 'path' that is equal to the string originalString
  if (length(arrayToGo) == 0) {
    # If arrayToGo is empty, create a terminal list with a string called 'path' and value equal to originalString
    endList <- list()
    endList$path <- originalPathString
    return(endList)
  } else {
    # If arrayToGo is still not empty, remove its leading element and create a sub-branch list corresponding to the structure of the remaining elements of arrayToGo
    newBranch <- list()
    newBranch[[arrayToGo[1]]] <- addBranch(originalPathString, tail(arrayToGo, -1))
    return(newBranch)
  }
}

nextStep <- function(listSoFar, originalString, arrayToGo) {
  # Recursive function that adds a multilayer list to listSoFar that has a branched structure representing the vector of strings arrayToGo.
  if (length(arrayToGo) == 0) { # If end of string vector arrayToGo has been reached, create a vector called 'path' and give it the value 'originalString'.
    listSoFar$path <- originalString
  } else { # End of branch has not been reached.
    # If this portion of the string vector arrayToGo has not been added to listToGo yet, add it using the function addBranch
    if (is.null(listSoFar[[arrayToGo[1]]])) {
      listSoFar[[arrayToGo[1]]] <- addBranch(originalString, tail(arrayToGo, -1))
    }
    # If this portion of the string vector arrayToGo has already been added to listSoFar, remove the leading element of arrayToGo and recursively apply this function using the remaining elements of arrayToGo.
    else {
      listSoFar[[arrayToGo[1]]] <- nextStep(listSoFar[[arrayToGo[1]]], originalString, tail(arrayToGo, -1))
    }
  }
  return(listSoFar)
}

#'  Given a simulation file path or an instance of a simulation, traverses the simulation structure and returns a tree like structure
#'  allowing for intuitive navigation in the simulation tree
#
#' @param simulationOrFilePath Full path of the simulation to load or instance of a simulation
#' @param parameterPathsOnly when set to `TRUE`, the function returns a tree of parameter paths only.  If `FALSE` (as default), the tree includes all available quantities.
#' @return A list with a branched structure representing the path tree of Quantities in the simulation file.
#' At the end of each branch is a string called 'path' that is the path of the quantity represented by the branch.
#'
#' @importFrom utils tail
#' @examples
#' simPath <- system.file("extdata", "simple.pkml", package = "ospsuite")
#' sim <- loadSimulation(simPath)
#'
#' tree <- getSimulationTree(sim)
#'
#' liver_volume_path <- tree$Organism$Liver$Volume$path
#' @export
getSimulationTree <- function(simulationOrFilePath, parameterPathsOnly = FALSE) {
  validateIsOfType(simulationOrFilePath, c(Simulation, "character"))

  simulation <- simulationOrFilePath
  if (isOfType(simulationOrFilePath, "character")) {
    simulation <- loadSimulation(simulationOrFilePath)
  }


  pathGetterFunction <- getAllQuantityPathsIn
  if(parameterPathsOnly){
    pathGetterFunction <- getAllParameterPathsIn
  }

  allQuantityPaths <- pathGetterFunction(simulation)

  # Initiate list to be returned as a null list.
  pathEnumList <- list()
  for (path in allQuantityPaths) {
    # Convert the path string to a vector of strings, each representing a branch portion.
    pathArray <- toPathArray(path)

    # Begin recursive loop to generate branched list.
    pathEnumList <- nextStep(pathEnumList, path, pathArray)
  }

  return(pathEnumList)
}
