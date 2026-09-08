# Regenerating the example and test files

The files in `inst/extdata/` are generated with the OSP Suite .NET libraries that
ship in `inst/lib/`. When those libraries are updated (new suite version, new
`MoBi.R`/`PKSim.R` pin in `shared/DependencyManager/src/DependencyManager.csproj`),
the generated files can fall out of step with them. The scripts in this folder
rebuild the files for which the procedure is known.

`data-raw/` ships with the package, so the procedure travels with the sources it
belongs to and can be repeated on every major update of the libraries.

## Scripts

| Script | Rebuilds |
| --- | --- |
| `regenerate-aciclovir-pkml.R` | `inst/extdata/Aciclovir.pkml` |
| `regenerate-ind-and-pop-snapshot.R` | `inst/extdata/ind_and_pop_snapshot.json` |
| `regenerate-pk-analyses-csv.R` | `inst/extdata/PKAnalyses.csv` |
| `regenerate-pop-csv.R` | `inst/extdata/pop.csv` |
| `regenerate-test-snapshot.R` | `inst/extdata/test_snapshot.json` |
| `regenerate-pkml-fixtures.R` | the test fixtures in PKML format, see below |

Run them from the repository root, in the order listed. `PKAnalyses.csv` is
derived from `Aciclovir.pkml`, so the simulation has to be rebuilt first.

```
Rscript data-raw/regenerate-aciclovir-pkml.R
Rscript data-raw/regenerate-ind-and-pop-snapshot.R
Rscript data-raw/regenerate-pk-analyses-csv.R
Rscript data-raw/regenerate-pop-csv.R
Rscript data-raw/regenerate-test-snapshot.R
Rscript data-raw/regenerate-pkml-fixtures.R
```

Do not add `--vanilla`: it skips `.Rprofile`, so renv never activates and the
wrong `rSharp` is used (see `AGENTS.md`).

## Before you start

1. Update `inst/lib/` first (see "How `inst/lib` is populated" in `AGENTS.md`).
   Every script here runs against the binaries in `inst/lib/`, so the suite
   version must already be the new one.
2. Check the result before committing:

   ```r
   devtools::test()
   devtools::build_vignettes()
   ```

   Several tests compare against the values in these files, and the vignettes
   print them.

## What each script does

### `Aciclovir.pkml`

The simulation is described by the PK-Sim project snapshot that the PKML carries
inside it, base64 encoded, in a `<Snapshot>` element. Three of them are present,
one for the project, one for the individual and one for the expression profile,
and the project one is identified by its `"ApplicationName": "PK-Sim"` entry. The
script extracts it, writes it to `data-raw/Aciclovir_snapshot.json`, lets PK-Sim
rebuild the simulation from it with the current libraries, and writes the
simulation out as PKML.

**A simulation loaded from a snapshot carries no embedded snapshot of its own.**
`loadSimulationsFromSnapshot()` builds the simulation from the snapshot content,
and the PKML the script writes loses the snapshots. That is reported as a bug in
[OSPSuite-R#2029](https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/2029).

`saveSimulation()` itself preserves the snapshots; the loss comes from the
snapshot route. Until the bug is fixed, two things follow:

- `data-raw/Aciclovir_snapshot.json` is committed, because after one run of the
  script the PKML has nothing left to extract. The script then falls back to that
  file, and says so. Commit the JSON again whenever the simulation changes.
- To keep the embedded snapshot in the example file, do the regeneration in the
  PK-Sim user interface instead: load `data-raw/Aciclovir_snapshot.json` as a
  project, then export the single simulation to `inst/extdata/Aciclovir.pkml`.

The two routes give simulations that differ in one respect: a simulation
exported from PK-Sim carries no calculation method overrides, while one that is
opened in MoBi, configured again and saved does. Pick the route that matches
what the example is meant to show.

### `ind_and_pop_snapshot.json`

The script converts the snapshot to a PK-Sim project with the current libraries,
then converts that project back to a snapshot. Both directions run through the
PK-Sim converter, so the file comes out in the format the new version writes.

It uses `runSimulations = TRUE`, because the simulations in this snapshot have
`"HasResults": true` and only a project whose simulations were run carries that
flag back into the exported snapshot.

### `test_snapshot.json`

Same route as `ind_and_pop_snapshot.json`: snapshot to PK-Sim project, then back
to snapshot. The simulations in this snapshot have `"HasResults": false`, so
`runSimulations` stays at its default and they are not run.

### `PKAnalyses.csv`

The script runs the simulation from `Aciclovir.pkml`, calculates the PK
parameters, and exports them with `exportPKAnalysesToCSV()`, which writes the
file through OSPSuite.Core (`PKAnalysisTask.ExportPKAnalysesToCSV`) rather than
from R.

`vignettes/pk-analysis.Rmd` also calls `exportPKAnalysesToCSV` on the installed
copy of this file, so building the vignettes overwrites it in the installed
package library. That does not touch the repository copy.

### `pop.csv`

The script creates a European population of 10 individuals with
`createPopulation()` and writes it with `exportPopulationToCSV()`.

Keep the seed that is set in the script unless every individual in the file
should be replaced.

### The test fixtures in PKML format

`regenerate-pkml-fixtures.R` loads each file and writes it back, which lifts it
to the format version of the current libraries. It covers

- `tests/data/simple.pkml` and `inst/extdata/simple.pkml`, which hold the same
  simulation. The script writes the second from the first, so the two stay
  identical.
- `tests/data/concentration_based.pkml`
- `tests/data/MoBiProject/TestSim_2Modules.pkml`
- `tests/data/obs_data.pkml` and `tests/data/obs_data_no_error.pkml`, which are
  observed data and go through `loadDataSetFromPKML()` and
  `saveDataSetToPKML()` rather than through the simulation functions.

Both exports drop metadata that the model itself does not need but MoBi uses:
`saveSimulation()` empties the parameter `group` attribute
([OSPSuite-R#2030](https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/2030)),
and `saveDataSetToPKML()` drops the `date` and `source` of the observed data.
Regenerate through MoBi instead when that metadata has to survive.

## Fixtures that stay as they are

Some files must not be lifted to the current version, because their age is what
they test:

- `tests/data/simple_v11.pkml` carries an older format on purpose.
  `tests/testthat/test-simulation.R` asserts the error that a simulation from an
  earlier version of the suite raises.
- `tests/data/pop_5_spared_id.csv` has gaps in its `IndividualId` column, and a
  fresh export renumbers them.
- `tests/data/junk.csv` is malformed by design.

Other files are neither generated by the suite nor tied to its version: the
`CompiledDataSet*.xlsx` files and `Stevens_2012_placebo_indiv_results.csv` hold
published data and results from a model that is not part of this repository, and
the data importer configurations (`ImporterConfiguration.xml`,
`dataImporterConfiguration_*.xml`) can only be written by the data importer
itself, since the package reads them but does not save them.

`tests/data/baby_aging.csv` would need an aging data export, which the package
does not have; it only reads that format with `loadAgingDataFromCSV()`. Its
population file `tests/data/baby.csv` belongs with it, so both stay until PK-Sim
can supply the pair.

## Files without a documented procedure

The remaining files in `inst/extdata/` are not covered here. If you regenerate
one of them, add a script and a section to this folder.

In `inst/extdata/`: `CompiledDataSet.xlsx`, `ObsDataAciclovir_1.pkml`,
`ObsDataAciclovir_2.pkml`, `ObsDataAciclovir_3.pkml`, `SAResult.csv`,
`SimResults.csv`, `TH_QST_Platform.mbp3`, `Thyroid.pkml`, `aging_data.csv`,
`dataImporterConfiguration.xml`, `res.csv`, and `sa.csv`.

In `tests/data/`: `SimResults_pop.csv`, `res_10.csv`, `res_11-20.csv`, `sa.csv`,
`sa_1.csv`, `sa_2.csv`, and the two MoBi projects under `MoBiProject/`, which
only MoBi can write. `Test_Project.mbp3` is described in
`tests/data/MoBiProject/Test_Project.md`.
