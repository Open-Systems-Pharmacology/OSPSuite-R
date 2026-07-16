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

  # macOS only ships arm64 (Apple Silicon) native libraries. An unsupported
  # macOS architecture (for example x86_64) is a hard failure and must abort
  # loading with a precise diagnostic, so it runs here, before `.initPackage()`
  # wraps native-library loading in graceful degradation. Otherwise the abort
  # would be swallowed and misreported as a generic "runtime not initialised".
  sysInfo <- Sys.info()
  if (sysInfo[["sysname"]] == "Darwin" && sysInfo[["machine"]] != "arm64") {
    stop(messages$errorUnsupportedMacArchitecture(sysInfo[["machine"]]))
  }

  .initPackage()
}

.onAttach <- function(libname, pkgname) {
  # Surface a failed runtime initialisation only when the user explicitly
  # attaches the package (`library(ospsuite)`), using packageStartupMessage() so
  # it can be suppressed. `.onLoad()` stays silent, as required for a bare
  # namespace load.
  if (!is.null(ospsuiteEnv$loadError)) {
    packageStartupMessage(messages$runtimeNotInitialised(ospsuiteEnv$loadError))
  }
}
# nocov end
