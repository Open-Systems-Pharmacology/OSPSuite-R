# This file is run after all tests (inside each worker when parallel)
options(ospsuite.plots.watermarkEnabled = NULL)

# Capture the set of .NET assemblies actually loaded during the test run.
# Enabled by setting OSPSUITE_AUDIT_ASSEMBLIES=1 before running tests.
# See issue #1587.
if (nzchar(Sys.getenv("OSPSUITE_AUDIT_ASSEMBLIES"))) {
  libDir <- system.file("lib", package = "ospsuite")
  rSharp::loadAssembly(file.path(libDir, "DependencyManager.dll"))
  loaded <- rSharp::callStatic("DependencyManager.Auditor", "GetLoadedAssemblies")

  platform <- tolower(Sys.info()[["sysname"]])
  outDir <- file.path(rprojroot::find_package_root_file(), "tools", "audit", "logs")
  dir.create(outDir, recursive = TRUE, showWarnings = FALSE)
  # Per-PID filename so each parallel testthat worker writes its own log;
  # union them in post-processing.
  outFile <- file.path(
    outDir,
    sprintf("loaded-assemblies-%s-pid%d.log", platform, Sys.getpid())
  )
  writeLines(loaded, outFile)
  message("Wrote ", length(loaded), " loaded assemblies to ", outFile)
}
