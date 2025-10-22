# tools/configure_reticulate.R
# prepare platform-specific files for installation
#
# This script is called by both configure and configure.win
# It handles platform detection and file operations in R

# Get system information
sysname <- Sys.info()[["sysname"]]
machine <- Sys.info()[["machine"]]
lib_dir <- "inst/lib"

cat("Configuring ospsuite for", sysname, "(", machine, ")...\n")


src_file <- switch(
  sysname,
  Darwin = if (machine == "arm64") "System.Data.SQLite.mac.dll" else NULL,
  Linux = "System.Data.SQLite.windows_linux.dll",
  Windows = "System.Data.SQLite.windows_linux.dll",
  NULL
)

if (is.null(src_file)) {
  cat(sprintf(
    "  Not supported on this platform (%s/%s) — skipping configuration.\n",
    sysname,
    machine
  ))
} else {
  src_path <- file.path(lib_dir, src_file)
  target_path <- file.path(lib_dir, "System.Data.SQLite.dll")

  if (file.exists(src_path)) {
    if (file.copy(src_path, target_path, overwrite = TRUE)) {
      cat(sprintf("  Configured %s for %s\n", basename(target_path), sysname))
    } else {
      cat(sprintf("  Error: failed to copy %s -> %s\n", src_path, target_path))
    }
  } else {
    cat(sprintf("  Warning: %s not found\n", basename(src_path)))
  }
}
cat("Configuration complete.\n")
