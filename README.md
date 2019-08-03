# OSPSuite-R

  <!-- badges: start -->

[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/Open-Systems-Pharmacology/OSPSuite-R?branch=develop&svg=true)](https://ci.appveyor.com/project/open-systems-pharmacology-ci/ospsuite-r)
[![codecov](https://codecov.io/gh/Open-Systems-Pharmacology/OSPSuite-R/branch/develop/graph/badge.svg)](https://codecov.io/gh/Open-Systems-Pharmacology/OSPSuite-R)

  <!-- badges: end -->

# Development tasks

## dev_mode

`devtools::dev_mode` function switches your version of R into "development mode". This is useful to avoid clobbering the existing versions of CRAN packages that you need for other tasks. Calling dev_mode() again will turn development mode off, and return you to your default library setup.

```R
# This will install the package in the folder C:/Rpackages
devtools::dev_mode(path="C:/Rpackages")
```

## Reload the package

```R
devtools::load_all()
```

or `Ctrl + Shift + L`

## Add or update script files

TODO: Is this the right way? Maybe use vignette for that?

`.R` files defined in `tests\dev\` will be removed from the package and can be used to simulate interaction with the package. See [scripts.R](tests/dev/scripts.R)

## Code of conduct

Everyone interacting in the Open Systems Pharmacology community (codebases, issue trackers, chat rooms, mailing lists etc...) is expected to follow the Open Systems Pharmacology [code of conduct](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CODE_OF_CONDUCT.md).

## Contribution

We encourage contribution to the Open Systems Pharmacology community. Before getting started please read the [contribution guidelines](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CONTRIBUTING.md). If you are contributing code, please be familiar with the [coding standards](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CODING_STANDARDS_R.md).

## License

OSPSuite-R is released under the [GPLv2 License](LICENSE).

All trademarks within this document belong to their legitimate owners.
