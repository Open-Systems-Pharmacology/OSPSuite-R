
<!-- README.md is generated from README.Rmd. Please edit that file -->

# OSPSuite-R

<!-- badges: start -->

<a href="https://github.com/Open-Systems-Pharmacology/OSPSuite-R/releases/latest" class="pkgdown-devel">
<img src="https://img.shields.io/github/downloads/Open-Systems-Pharmacology/OSPSuite-R/latest/total?label=%E2%AD%B3%20Downloads%20latest%20release"/></a>
<a href="https://github.com/Open-Systems-Pharmacology/OSPSuite-R/releases" class="pkgdown-devel">
<img src="https://img.shields.io/github/downloads/Open-Systems-Pharmacology/OSPSuite-R/total?label=%E2%AD%B3%20Downloads%20total"/></a>
<br>
<a href="https://ci.appveyor.com/project/open-systems-pharmacology-ci/OSPSuite-R/branch/develop" class="pkgdown-devel">
<img src="https://ci.appveyor.com/api/projects/status/github/Open-Systems-Pharmacology/OSPSuite-R?branch=develop&amp;svg=true" alt="AppVeyor build status"/></a>
<a href="https://app.codecov.io/gh/Open-Systems-Pharmacology/OSPSuite-R" class="pkgdown-devel">
<img src="https://codecov.io/gh/Open-Systems-Pharmacology/OSPSuite-R/branch/develop/graph/badge.svg" alt="codecov"/></a>

<!-- badges: end -->

# Overview

The **ospsuite-R** package provides the functionality of loading,
manipulating, and simulating the simulations created in the Open Systems
Pharmacology Software tools PK-Sim and MoBi.

- [Documentation](#documentation)
- [Installation](#installation)
- [Known issues](#known-issues)
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
  Started](https://www.open-systems-pharmacology.org/OSPSuite-R/articles/ospsuite.html)
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

# Installation

The **ospsuite** package is compatible with R version 3.6.x **AND**
4.x.x and can be used on [Windows](#on-windows) and [Linux](#on-linux)
operating systems.

`ospsuite` is not available on CRAN and also depends on three packages
that are not available on CRAN. Thus the installation process is a bit
unconventional, please follow carefully the instructions below.

## On Windows

### Pre-requisites

The package requires additional software installations:

- Visual C++ Runtime available
  [here](https://aka.ms/vs/16/release/vc_redist.x64.exe)
- .NET Framework 4.7.2 available
  [here](https://go.microsoft.com/fwlink/?LinkID=863265)

**NB**: These pre-requisites are already installed if the OSP Suite was
installed before.

### From GitHub (recommended)

The latest released version of the package can be installed from GitHub
using the `{remotes}` package. The code below will download and install
all the required dependencies.

``` r
install.packages("remotes")
rClrURL <- if (R.Version()$major >= 4) {
  "https://github.com/Open-Systems-Pharmacology/rClr/releases/download/v0.9.2/rClr_0.9.2.zip"
} else {
  "https://github.com/Open-Systems-Pharmacology/rClr/releases/download/v0.9.1-R3/rClr_0.9.1.zip"
}

install.packages(rClrURL,
  repos = NULL,
  type = "binary"
)

remotes::install_github("Open-Systems-Pharmacology/OSPSuite.RUtils@*release")
remotes::install_github("Open-Systems-Pharmacology/TLF-Library@*release")
remotes::install_github("Open-Systems-Pharmacology/OSPSuite-R@*release")
```

### From package archive files

It is also possible to install manually from archive pre-built archive
files.

#### Install CRAN dependencies

When installing from such files, the CRAN dependencies need to be
installed manually first.

``` r
    # Install dependencies (e.g. R6) which are on CRAN
    install.packages("data.table")
    install.packages("dplyr")
    install.packages("ggplot2")
    install.packages("ggtext")
    install.packages("jsonlite")
    install.packages("lifecycle")
    install.packages("patchwork")
    install.packages("purrr")
    install.packages("R6")
    install.packages("readr")
    install.packages("rlang")
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

- `rClr`
  - [For R
    4.x.x](https://github.com/Open-Systems-Pharmacology/rClr/releases/download/v0.9.2/rClr_0.9.2.zip)
  - [For R
    3.6.x](https://github.com/Open-Systems-Pharmacology/rClr/releases/download/v0.9.1-R3/rClr_0.9.1.zip)
- [`ospsuite.utils`](https://github.com/Open-Systems-Pharmacology/OSPSuite.RUtils/releases/latest)
- [`tlf`](https://github.com/Open-Systems-Pharmacology/TLF-Library/releases/latest)

If you use [RStudio IDE](https://www.rstudio.com/), you can use the
*Install* option in the *Packages* pane and select the option *Install
from -\> Package Archive File* to install a package from binary `*.zip`
files.

**NB**: The CRAN dependencies of rClr, ospuite.utils and tlf were
already installed during the previous step.

``` r
# Install `{rClr}` from local file 
# (`pathTo_rCLR.zip` here should be replaced with the actual path to the `.zip` file)
install.packages(pathTo_rCLR.zip, repos = NULL)

# Install `{ospsuite.utils}` from local file 
# (`pathTo_ospsuite.utils.zip` here should be replaced with the actual path to the `.zip` file)
install.packages(pathTo_ospsuite.utils.zip, repos = NULL)

# Install `{tlf}` from local file 
# (`pathTo_tlf.zip` here should be replaced with the actual path to the `.zip` file)
install.packages(pathTo_tlf.zip, repos = NULL)

# Install `{ospsuite}` from local file
# (`pathToOSPSuite.zip` here should be replaced with the actual path to the `.zip` file)
install.packages(pathToOSPSuite.zip, repos = NULL)
```

## On Linux

The **ospsuite** package has been tested under Linux distributions
**CentOS 7** and **Ubuntu 18**. Some functionality, such as creating
individuals or populations, is not available under Linux. Installation
under Linux requires several prerequisites, the detailed instructions
can be found in the Wiki:

- [Setup OSPSuite-R on
  CentOS](https://github.com/Open-Systems-Pharmacology/OSPSuite-R/wiki/Setup-ospsuite-R-on-CentOS7)
- [Setup OSPSuite-R on
  Ubuntu](https://github.com/Open-Systems-Pharmacology/OSPSuite-R/wiki/Setup-ospsuite-R-on-Ubuntu)

For other Linux distributions Docker containers can be used (Dockerfiles
based on CentOS 7 and Ubuntu 18 are available under
<https://github.com/Open-Systems-Pharmacology/OSPSuite-R/releases>)

## Build from source

You can clone the GIT repository and build the package from source.

### How to update dependencies from nuget?

- `git submodule update --init --recursive` to install all submodules
- Make sure you have [ruby](https://www.ruby-lang.org/de/downloads/)
  install and that it is available in your path
- Run `rake postclean` or simply double click on `postclean.bat`. This
  will update all nuget packages and copy the dependencies in the
  package `inst/lib` folder.

# Known issues

## Loading `ospsuite` might fail if your systems locale is not set to English

– On Windows, set
`Settings > Language > Administrative language settings > Current language for non-Unicode programs`
to `English (United States)` and reboot. – On Linux, set the environment
variable `LC_ALL` before starting R:

    export LC_ALL=en_US.UTF-8

## RStudio crashes when trying to load a workspace.

The ospsuite package uses the features implemented in PK-Sim and MoBi by
creating `.NET` objects (e.g. a simulation) and using them from R. These
objects cannot be saved as part of the workspace and reloaded on next
start. When trying to do so, RStudio simply crashes. There is no
possibility to overcome this limitation. To prevent RStudio from
crashing, make sure to disable the check-box “Restore `.RData` into
workspace at startup” in the options of RStudio. Keep in mind that you
can also change this setting for specific projects.

# Code of Conduct

Everyone interacting in the Open Systems Pharmacology community
(codebases, issue trackers, chat rooms, mailing lists etc.) is expected
to follow the Open Systems Pharmacology [code of
conduct](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CODE_OF_CONDUCT.md).

# Contribution

We encourage contribution to the Open Systems Pharmacology community.
Before getting started please read the [contribution
guidelines](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CONTRIBUTING.md).
If you are contributing to the codebase, please be familiar with the
[coding
standards](https://github.com/Open-Systems-Pharmacology/developer-docs/blob/main/ospsuite-r-specifics/CODING_STANDARDS_R.md).

# License

OSPSuite-R is released under the [GPLv2 License](LICENSE).

All trademarks within this document belong to their legitimate owners.
