
# OSPSuite R package

<!-- badges: start -->

[![Build
Badge](https://img.shields.io/github/actions/workflow/status/Open-Systems-Pharmacology/OSPSuite-R/main-workflow.yaml?branch=main&label=Build)](https://github.com/Open-Systems-Pharmacology/OSPSuite-R/actions/workflows/main-workflow.yaml)
[![Codecov test coverage
Badge](https://codecov.io/gh/Open-Systems-Pharmacology/OSPSuite-R/branch/main/graph/badge.svg)](https://app.codecov.io/gh/Open-Systems-Pharmacology/OSPSuite-R?branch=main)

<!-- badges: end -->

# Overview

The `{ospsuite}`R package provides the functionality of loading,
manipulating, and running the simulations created in the Open Systems
Pharmacology Software tools PK-Sim and MoBi.

- [Documentation](#documentation)
- [Installation](#installation)
- [Known issues](#known-issues)
- [Development](#development)
- [Code of conduct](#code-of-conduct)
- [Contribution](#contribution)
- [Licence](#licence)

# Documentation

For complete documentation, see the [online
documentation](https://www.open-systems-pharmacology.org/OSPSuite-R/).

## Essential Guides

**Essential guides:**

- [Get
  Started](https://www.open-systems-pharmacology.org/OSPSuite-R/articles/ospsuite.html) -
  Complete beginner tutorial with working examples
- [Loading a simulation and accessing
  entities](https://www.open-systems-pharmacology.org/OSPSuite-R/articles/load-get.html)
- [Changing parameter and molecule start
  values](https://www.open-systems-pharmacology.org/OSPSuite-R/articles/set-values.html)
- [Running a
  simulation](https://www.open-systems-pharmacology.org/OSPSuite-R/articles/run-simulation.html)

**For advanced workflows:**

- [Efficient
  calculations](https://www.open-systems-pharmacology.org/OSPSuite-R/articles/efficient-calculations.html)
- [Creating
  individuals](https://www.open-systems-pharmacology.org/OSPSuite-R/articles/create-individual.html)
- [Population
  simulations](https://www.open-systems-pharmacology.org/OSPSuite-R/articles/create-run-population.html)
- [PK
  Analysis](https://www.open-systems-pharmacology.org/OSPSuite-R/articles/pk-analysis.html)
- [Sensitivity
  analysis](https://www.open-systems-pharmacology.org/OSPSuite-R/articles/sensitivity-analysis.html)

**Data handling and visualization:**

- [Working with data sets and import from
  excel](https://www.open-systems-pharmacology.org/OSPSuite-R/articles/observed-data.html)
- [Working with `DataCombined`
  class](https://www.open-systems-pharmacology.org/OSPSuite-R/articles/data-combined.html)
- [Visualizations with
  `DataCombined`](https://www.open-systems-pharmacology.org/OSPSuite-R/articles/data-combined-plotting.html)

**Reference:**

- [Dimensions and
  Units](https://www.open-systems-pharmacology.org/OSPSuite-R/articles/unit-conversion.html)
- [Table
  parameters](https://www.open-systems-pharmacology.org/OSPSuite-R/articles/table-parameters.html)
- [PK-Sim
  Installation](https://www.open-systems-pharmacology.org/OSPSuite-R/articles/PKSim-installation.html)

# Installation

The **ospsuite** package requires R version 4.x.x and supports
[Windows](#on-windows) and [Linux (Ubuntu)](#on-linux) operating
systems.

`ospsuite` is not available on CRAN and depends on packages from the OSP
ecosystem that are not available on CRAN. Follow the instructions below
to install the package and all required dependencies.

## Pre-requisites

The `{ospsuite}` package requires `{rSharp}` and its external
dependencies (Visual C++ Redistributable (Windows) and .NET 8). Install these
dependencies using the following instructions:

- [For Windows](https://github.com/Open-Systems-Pharmacology/rSharp?tab=readme-ov-file#windows)
- [For Linux (Ubuntu)](https://github.com/Open-Systems-Pharmacology/rSharp?tab=readme-ov-file#ubuntu)
- [For MacOS](https://github.com/Open-Systems-Pharmacology/rSharp?tab=readme-ov-file#macos)

## From GitHub (recommended)

Install the latest released version from GitHub using the `{remotes}`
package. This installation method downloads and installs all required
dependencies.

``` r
install.packages("remotes")
remotes::install_github("Open-Systems-Pharmacology/OSPSuite-R@*release")
```

To install the latest development version:

``` r
install.packages("remotes")
remotes::install_github("Open-Systems-Pharmacology/OSPSuite-R")
```

## From package archive files (Deprecated)

Install manually from pre-built archive files available in the
[releases](https://github.com/Open-Systems-Pharmacology/OSPSuite-R/releases).

#### Install CRAN dependencies

Installing from archive files requires manual installation of CRAN
dependencies first.

``` r
# Install dependencies (e.g. R6) which are on CRAN
install.packages("cli")
install.packages("crayon")
install.packages("data.table")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggtext")
install.packages("glue")
install.packages("jsonlite")
install.packages("lifecycle")
install.packages("logger")
install.packages("openxlsx")
install.packages("patchwork")
install.packages("purrr")
install.packages("R6")
install.packages("readr")
install.packages("rlang")
install.packages("showtext")
install.packages("stringi")
install.packages("stringr")
install.packages("tidyr")
install.packages("xml2")
```

#### Install non-CRAN dependencies

Pre-built packages are available as binary files (*.zip for Windows,
*.tar.gz for Linux) Download the OSPSuite-R binary archive from the
[releases
page](https://github.com/Open-Systems-Pharmacology/OSPSuite-R/releases).
Download and install these additional non-CRAN dependencies:

- [`{rSharp}`](https://github.com/Open-Systems-Pharmacology/rSharp/releases/latest)
- [`{ospuite.utils}`](https://github.com/Open-Systems-Pharmacology/OSPSuite.RUtils/releases/latest)
- [`{tlf}`](https://github.com/Open-Systems-Pharmacology/TLF-Library/releases/latest)

In [RStudio IDE](https://www.rstudio.com/), use the *Install* option in
the *Packages* pane and select *Install from -\> Package Archive File*
to install packages from binary files.

**Note**: CRAN dependencies for `{rSharp}`, `{ospuite.utils}`, and
`{tlf}` were installed in the previous step.

``` r
# Install rSharp from local file 
# Replace pathTo_rSharp.zip with the actual path to the .zip file (or to the .tar.gz file on Linux)
install.packages(pathTo_rSharp.zip, repos = NULL)

# Install ospsuite.utils from local file
# Replace pathTo_ospsuite.utils.zip with the actual path to the .zip file (or to the .tar.gz file on Linux)
install.packages(pathTo_ospsuite.utils.zip, repos = NULL)

# Install tlf from local file
# Replace pathTo_tlf.zip with the actual path to the .zip file (or to the .tar.gz file on Linux)
install.packages(pathTo_tlf.zip, repos = NULL)

# Install ospsuite from local file
# Replace pathToOSPSuite.zip with the actual path to the .zip file (or to the .tar.gz file on Linux)
install.packages(pathTo_OSPSuite.zip, repos = NULL)
```

# Known issues

## Loading `ospsuite` might fail if the system locale is not set to English

- On Windows, set
  `Settings > Language > Administrative language settings > Current language for non-Unicode programs`
  to `English (United States)` and reboot.

- On Linux, set the environment variable `LC_ALL` to `~/.bashrc` before
  starting R:

  `export LC_ALL=en_US.UTF-8`

- add the path to the lib subfolder of installed ospsuite package to
  LD_LIBRARY_PATH
  `export LD_LIBRARY_PATH=/usr/local/lib/R/site-library/ospsuite/lib/:$LD_LIBRARY_PATH`

## Saving and loading the workspace in RStudio does not restore objects

The ospsuite package uses the features implemented in PK-Sim and MoBi by
creating `.NET` objects (e.g. a simulation) and using them from R. These
objects cannot be saved as part of the workspace and reloaded on next
start. Upon restoring the workspace, the objects will be `NULL` and
cannot be re-used.

# Development

## Development Setup

When developing the `{ospsuite}` package, you need to prepare
platform-specific library files before using `devtools::load_all()`. The
package uses configure scripts during installation to rename
platform-specific DLLs, but `devtools::load_all()` bypasses this
process.

**Run the development setup script:**

``` r
source("tools/setup_dev.R")
setup_dev()
```

**When to run `setup_dev()`:** - After cloning the repository for the
first time - After switching to a different branch - After pulling
changes that might affect library files

The script automatically detects your operating system and renames the
appropriate DLL: - **Windows**: Renames
`System.Data.SQLite.windows_linux.dll` → `System.Data.SQLite.dll` -
**Linux**: Renames `System.Data.SQLite.windows_linux.dll` →
`System.Data.SQLite.dll` - **macOS**: Renames
`System.Data.SQLite.mac.dll` → `System.Data.SQLite.dll`

## Embedded binaries

The `{ospsuite}` package requires shared libraries to access PK-Sim
functionality. To obtain the latest libraries (**.dll** on Windows or
**.so** on Linux), run the `update_core_files.R` script included with
this package.

Because `{ospsuite}` contains binary files, it is classified as a binary
package and cannot be submitted to CRAN.

## Versioning

The package follows the versioning process described in the [OSP R
collaboration
guide](https://dev.open-systems-pharmacology.org/r-development-resources/collaboration_guide#releasing-versions).

For development versions, [this GitHub
action](https://github.com/Open-Systems-Pharmacology/OSPSuite-R/blob/main/.github/workflows/main-workflow.yaml#L11-L17)
automatically increments the `.9000` version suffix when pull requests
are merged.

# Code of Conduct

Everyone interacting in the Open Systems Pharmacology community
(codebases, issue trackers, chat rooms, mailing lists etc.) is expected
to follow the Open Systems Pharmacology [code of
conduct](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CODE_OF_CONDUCT.md).

# Contribution

Contributions to the Open Systems Pharmacology community are welcome.
Before contributing, read the [contribution
guidelines](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CONTRIBUTING.md).
Code contributors should follow the [R coding
standards](https://dev.open-systems-pharmacology.org/r-development-resources/coding_standards_r)
and [collaboration
guide](https://dev.open-systems-pharmacology.org/r-development-resources/collaboration_guide).

# License

OSPSuite-R is released under the [GPLv2 License](LICENSE).

All trademarks within this document belong to their legitimate owners.
