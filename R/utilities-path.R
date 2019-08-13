toPathArray <- function(path){
  strsplit(path, "\\|")
}

toPathString <- function(pathArray){
  paste(pathArray, collapse = "|")
}
