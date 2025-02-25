#' Load the OSPSuite R to .NET Binding
#'
#' This will be called once when the package is loaded
#'
#'
#' @import rSharp
#' @keywords internal
.initPackage <- function() {
  filePathFor <- function(name) {
    system.file("lib", name, package = ospsuiteEnv$packageName)
  }


  # Make .dll binaries available on windows by extending PATH
  if (.Platform$OS.type == "windows") {
    Sys.setenv(PATH = paste(system.file("lib", package = ospsuiteEnv$packageName),
                            Sys.getenv("PATH"),
                            sep = ";"
    ))
  }

  if (.Platform$OS.type == "unix") {

    if (Sys.info()[['sysname']] == "Linux"){
      # Load the .so binary files on unix/linux
      for (soFile in list.files(system.file("lib", package = ospsuiteEnv$packageName), pattern = "\\.so$", full.names = TRUE)) {
        dyn.load(soFile)
      }
    } else if(Sys.info()[['sysname']] == "Darwin"){

      # if ARM64 architecture, rename *Arm64.dylib files into *.dylib
      if (Sys.info()[['machine']] == "arm64") {
        for (arm64File in list.files(system.file("lib", package = ospsuiteEnv$packageName), pattern = "Arm64.dylib$", full.names = TRUE)) {
          file.copy(arm64File, gsub(".Arm64.dylib", ".dylib", arm64File),overwrite = TRUE)
        }
      }
      # if x64 architecture, rename *x64.dylib files to *.dylib
      else if (Sys.info()[['machine']] == "x86_64") {
        for (x64File in list.files(system.file("lib", package = ospsuiteEnv$packageName), pattern = "x64.dylib$", full.names = TRUE)) {
          file.copy(x64File, gsub(".x64.dylib", ".dylib",  x64File), overwrite = TRUE)
        }
      }

      # Load the .dylib (not x64.dylib nor Arm64.dylib) binary files on mac
      dylibFile <- list.files(system.file("lib", package = ospsuiteEnv$packageName), pattern = ".dylib$", full.names = TRUE)
      filtered_dylibFile <- dylibFile[!grepl("\\.(x64|Arm64)\\.dylib$", dylibFile)]
      for (dylibFile in filtered_dylibFile) {
        dyn.load(dylibFile)
      }
    }
  }

  rSharp::loadAssembly(filePathFor("OSPSuite.R.dll"))
  # Initialize once
  netObject <- rSharp::newObjectFromName("OSPSuite.R.ApiConfig")
  apiConfig <- ApiConfig$new(netObject)
  apiConfig$dimensionFilePath <- filePathFor("OSPSuite.Dimensions.xml")
  apiConfig$pkParametersFilePath <- filePathFor("OSPSuite.PKParameters.xml")

  rSharp::callStatic("OSPSuite.R.Api", "InitializeOnce", apiConfig)

  .initializeDimensionAndUnitLists()
}
