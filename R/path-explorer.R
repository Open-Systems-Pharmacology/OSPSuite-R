
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
#' @param quantityType A vector of strings that specify the types of the entities to be included in the tree.  The types can be any combination of "Quantity", "Molecule", "Parameter" and "Observer"
#' @return A list with a branched structure representing the path tree of entities in the simulation file that fall under the types specified in `quantityType`.
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
getSimulationTree <- function(simulationOrFilePath, quantityType = "Quantity") {
  validateIsOfType(simulationOrFilePath, c(Simulation, "character"))

  quantityTypeList <- list("Quantity" = getAllQuantityPathsIn,
                           "Molecule" = getAllMoleculePathsIn,
                           "Parameter" = getAllParameterPathsIn,
                           "Observer" = getAllObserverPathsIn)

  validateIsIncluded(values = quantityType,parentValues = names(quantityTypeList))

  simulation <- simulationOrFilePath
  if (isOfType(simulationOrFilePath, "character")) {
    simulation <- loadSimulation(simulationOrFilePath)
  }

  # Build a vector, with no duplicated entries, of all paths corresponding to entities in `simulation` that fall under the types specified in quantityType
  allPaths <- unique(unlist(unname(sapply(quantityType,function(type){quantityTypeList[[type]](simulation)}))))

  # Initiate list to be returned as a null list.
  pathEnumList <- list()
  for (path in allPaths) {
    # Convert the path string to a vector of strings, each representing a branch portion.
    pathArray <- toPathArray(path)

    # Begin recursive loop to generate branched list.
    pathEnumList <- nextStep(pathEnumList, path, pathArray)
  }

  return(pathEnumList)
}
