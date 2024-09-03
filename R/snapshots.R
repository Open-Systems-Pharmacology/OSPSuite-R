#' Run Simulations From Snapshot Files
#'
#' @param ... character strings, path to snapshot files or a directory containing snapshot files 
#' @param output character string, path to the output directory where to write simulation results
#' @param csv logical, whether to export the results as csv (default = TRUE)
#' @param pkml logical, whether to export the results as pkml (default = FALSE)
#' @param xml logical, whether to export the results as xml (default = FALSE)
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' runSimulationsFromSnapshot("path/to/my_snapshot.json", csv = TRUE, pkml = TRUE)
#' }
runSimulationsFromSnapshot <- function(..., output = ".", csv = TRUE, pkml = FALSE, xml = FALSE){
  
  initPKSim()

  temp_dir <- .gatherFiles(c(...))
  
  JsonRunOptions <- rSharp::newObjectFromName("PKSim.CLI.Core.RunOptions.JsonRunOptions")
  JsonRunOptions$set("InputFolder", temp_dir)
  JsonRunOptions$set("OutputFolder", normalizePath(output))

  if (isTRUE(csv)){ csv <- 2L } else { csv <- 0L }
  if (isTRUE(xml)){ xml <- 4L } else { xml <- 0L }
  if (isTRUE(pkml)){ pkml <- 8L } else { pkml <- 0L }
  
  ExportMode <- csv + xml + pkml
  
  JsonRunOptions$set("ExportMode", ExportMode)
  #0: nothing
  #1: json
  #2: csv
  #3: json + csv
  #4: xml
  #5: xml + json
  #6: xml + csv
  #7: json + csv + xml
  #8: pkml
  #9: pkml + json
  #10: pkml + csv
  #11: pkml + json + csv
  #12: pkml + xml
  #13: pkml + xml + json
  #14: pkml + xml + csv
  #15: all
  #16: nothing
  
  cli::cli_process_start(msg = "Running simulations from {length(list.files(temp_dir))} snapshots")
  rSharp::callStatic("PKSim.R.Api", "RunJson", JsonRunOptions)
}

#' Convert between snapshot and project formats
#'
#' @param ... character strings, path to files or a directory containing files to convert
#' @param format, character string, target format either "snapshot" or "project".
#' @param output character string, path to the output directory where to write the converted files
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' convertSnapshot("path/to/snapshot.json", format = "project")
#' convertSnapshot("path/to/project.pksim5", format = "snapshot")
#' }
convertSnapshot <- function(..., format, output = "."){
  
  rlang::arg_match(arg = format, values = c("snapshot", "project"))
  
  initPKSim()
  
  temp_dir <- .gatherFiles(c(...))
  
  SnapshotRunOptions <- rSharp::newObjectFromName("PKSim.CLI.Core.RunOptions.SnapshotRunOptions")
  SnapshotRunOptions$set(name = "InputFolder", value = temp_dir)
  SnapshotRunOptions$set(name = "OutputFolder", value = normalizePath(output))
  
  if (format == "project"){
    SnapshotRunOptions$set("ExportMode", 0L)
  } else  if (format == "snapshot") {
    SnapshotRunOptions$set("ExportMode", 1L)
  }

  cli::cli_process_start(msg = "Converting to {format} format")
  rSharp::callStatic("PKSim.R.Api", "RunSnapshot", SnapshotRunOptions)
}


#' Gather files and files from folders to one location
#'
#' @param ... character strings of file paths or folder paths
#'
#' @return A temporary directory with all files copied to it
.gatherFiles <- function(...){
  temp_dir <- tempfile()
  dir.create(temp_dir)
  for (element in c(...)){
    # if the element is a folder, list all files in it and copy them to the temp directory
    if (dir.exists(element)){
      files <- list.files(element, full.names = TRUE, recursive = TRUE)
      for (file in files){
        file.copy(from = file, to = temp_dir)
      }
      next
    }
    # if the element is a file, copy it to the temp directory
    else if (file.exists(element)){
      file.copy(from = element, to = temp_dir)
      next
    }
  }
  return(temp_dir)
}
