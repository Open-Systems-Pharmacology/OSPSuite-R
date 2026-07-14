# nocov start
.onLoad <- function(libname, pkgname) {
  # Only for x64 bits packages.
  # This is required to avoid error when package is being checked on CI for x86
  is64 <- (.Machine$sizeof.pointer == 8)
  if (!is64) {
    return()
  }

  # Check that the package is support by the user architecture
  supportedArchitecture <- R.version$arch %in% c("x86_64", "aarch64")
  if (!supportedArchitecture) {
    stop(
      "The processor architecture you are using is not supported by this
         package. (only x86_64 and aarch64 are supported)"
    )
  }

  .initPackage()
}

.onAttach <- function(libname, pkgname) {
  # Surface a failed runtime initialisation only when the user explicitly
  # attaches the package (`library(ospsuite)`), using packageStartupMessage() so
  # it can be suppressed. `.onLoad()` stays silent, as required for a bare
  # namespace load.
  if (!is.null(ospsuiteEnv$loadError)) {
    packageStartupMessage(
      "The OSPSuite .NET runtime could not be initialised.\n",
      "ospsuite is installed, but calls into the .NET API will fail until a working runtime is available.\n",
      "Details: ",
      ospsuiteEnv$loadError
    )
  }
}
# nocov end
