
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

If you are reading this on GitHub README, please refer to the [online
documentation](https://www.open-systems-pharmacology.org/OSPSuite-R/)
for more details on the package.

In particular, we would recommend that you read the articles in the
following order:

- [Get
  Started](ttps://www.open-systems-pharmacology.org/OSPSuite-R/articles/ospsuite.html)
- [Loading a simulation and accessing
  entities](https://www.open-systems-pharmacology.org/OSPSuite-R/articles/load-get.html)
- [Changing parameter and molecule start
  values](https://www.open-systems-pharmacology.org/OSPSuite-R/articles/set-values.html)
- [Running a
  simulation](https://www.open-systems-pharmacology.org/OSPSuite-R/articles/run-simulation.html)
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
- [Table
  parameters](https://www.open-systems-pharmacology.org/OSPSuite-R/articles/table-parameters.html)
- [Dimensions and
  Units](https://www.open-systems-pharmacology.org/OSPSuite-R/articles/unit-conversion.html)
- [Working with data sets and import from
  excel](https://www.open-systems-pharmacology.org/OSPSuite-R/articles/observed-data.html)
- [Working with `DataCombined`
  class](https://www.open-systems-pharmacology.org/OSPSuite-R/articles/data-combined.html)
- [Visualizations with
  `DataCombined`](https://www.open-systems-pharmacology.org/OSPSuite-R/articles/data-combined-plotting.html)
- [PK-Sim
  Installation](https://www.open-systems-pharmacology.org/OSPSuite-R/articles/PKSim-installation.html)

# Installation

The **ospsuite** package is compatible with R version 4.x.x and can be
used on [Windows](#on-windows) and [Linux (Ubuntu)](#on-linux) operating
systems.

`ospsuite` is not available on CRAN and also depends on packages from
the OSP ecosystem that are not available on CRAN. Please follow the
instructions below to install the packages and all required
dependencies.

## Pre-requisites

As `{ospsuite}` relies on `{rSharp}`, install its external dependencies
(Visual C++ Redistributable and .NET 8) by following these instructions:

- [For
  Windows](https://github.com/Open-Systems-Pharmacology/rSharp?tab=readme-ov-file#prerequisites)
- [For
  Linux](https://github.com/Open-Systems-Pharmacology/rSharp?tab=readme-ov-file#ubuntu)

## From GitHub (recommended)

The latest released version of the package can be installed from GitHub
using the `{remotes}` package. The code below will download and install
all the required dependencies.

``` r
install.packages("remotes")
remotes::install_github("Open-Systems-Pharmacology/OSPSuite-R@*release")
```

Get the latest development version with:

``` r
install.packages("remotes")
remotes::install_github("Open-Systems-Pharmacology/OSPSuite-R")
```

## From package archive files (Deprecated)

It is also possible to install manually from archive pre-built archive
files provided with the
[release](https://github.com/Open-Systems-Pharmacology/OSPSuite-R/releases).

#### Install CRAN dependencies

When installing from such files, the CRAN dependencies need to be
installed manually first.

``` r
# Install dependencies (e.g. R6) which are on CRAN
install.packages("cli")
install.packages("data.table")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggtext")
install.packages("jsonlite")
install.packages("lifecycle")
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

Each of the pre-built released packages are available as a a binary
`*.zip`. OSPSuite-R binary archive can be downloaded from
[here](https://github.com/Open-Systems-Pharmacology/OSPSuite-R/releases).
The other non-CRAN dependencies needed for OSPSuite-R also have to be
downloaded and manually installed:

- [`{rSharp}`](https://github.com/Open-Systems-Pharmacology/rSharp/releases/latest)
- [`{ospuite.utils}`](https://github.com/Open-Systems-Pharmacology/OSPSuite.RUtils/releases/latest)
- [`{tlf}`](https://github.com/Open-Systems-Pharmacology/TLF-Library/releases/latest)

If you use [RStudio IDE](https://www.rstudio.com/), you can use the
*Install* option in the *Packages* pane and select the option *Install
from -\> Package Archive File* to install a package from binary `*.zip`
files.

**NB**: The CRAN dependencies of `{rSharp}`, `{ospuite.utils}` and
`{tlf}` were already installed during the previous step.

``` r
# Install `{rSharp}` from local file 
# (`pathTo_rSharp.zip` here should be replaced with the actual path to the `.zip` file)
install.packages(pathTo_rSharp.zip, repos = NULL)

# Install `{ospsuite.utils}` from local file
# (`pathTo_ospsuite.utils.zip` here should be replaced with the actual path to the `.zip` file)
install.packages(pathTo_ospsuite.utils.zip, repos = NULL)

# Install `{tlf}` from local file
# (`pathTo_tlf.zip` here should be replaced with the actual path to the `.zip` file)
install.packages(pathTo_tlf.zip, repos = NULL)

# Install `{ospsuite}` from local file
# (`pathToOSPSuite.zip` here should be replaced with the actual path to the `.zip` file)
install.packages(pathTo_OSPSuite.zip, repos = NULL)
```

# Known issues

## Loading `ospsuite` might fail if your systems locale is not set to English

- On Windows, set
  `Settings > Language > Administrative language settings > Current language for non-Unicode programs`
  to `English (United States)` and reboot.

- On Linux, set the environment variable `LC_ALL` before starting R:

  `export LC_ALL=en_US.UTF-8`

## Saving and loading the workspace in RStudio does not restore objects

The ospsuite package uses the features implemented in PK-Sim and MoBi by
creating `.NET` objects (e.g. a simulation) and using them from R. These
objects cannot be saved as part of the workspace and reloaded on next
start. Upon restoring the workspace, the objects will be `NULL` and
cannot be re-used.

# Development

## Embeded binaries

The `{ospsuite}` package requires some shared libraries to get access to
PK-Sim functionality. To get the latest libraries(**.dll** on Windows or
**.so** on Linux), run the script file `update_core_files.R` provided
with this package.

Since `{ospsuite}` contains this binary files, it is considered as a
binary package and thus cannot be submitted to CRAN in this state.

## Versionning

The package follows the versioning process described in the [OSP R
collaboration
guide](https://dev.open-systems-pharmacology.org/r-development-resources/collaboration_guide#releasing-versions).

For development version, each time a pull request is merged, [this
github
action](https://github.com/Open-Systems-Pharmacology/OSPSuite-R/blob/main/.github/workflows/main-workflow.yaml#L11-L17)
automatically bumps the `.9000` version suffix by one.

# Code of Conduct

Everyone interacting in the Open Systems Pharmacology community
(codebases, issue trackers, chat rooms, mailing lists etc.) is expected
to follow the Open Systems Pharmacology [code of
conduct](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CODE_OF_CONDUCT.md).

# Contribution

We encourage contribution to the Open Systems Pharmacology community.
Before getting started please read the [contribution
guidelines](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CONTRIBUTING.md).
If you are contributing to the codebase, please be familiar with the [R
coding
standards](https://dev.open-systems-pharmacology.org/r-development-resources/coding_standards_r)
as well as the [collaboration
guide](https://dev.open-systems-pharmacology.org/r-development-resources/collaboration_guide).

# License

OSPSuite-R is released under the [GPLv2 License](LICENSE).

All trademarks within this document belong to their legitimate owners.
